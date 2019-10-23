library(shiny)
library(ggplot2)
library(purrr)
# Load survey questions

getInputID <- function(input){
  if(!inherits(input, "shiny.tag")){
    return()
  }
  c(
    if(!is.null(input$attribs$id)){list(list(id=input$attribs$id, type = input$name))}else{NULL},
    do.call("c", map(input$children, getInputID))
  )
}

shinyServer(
  function(input, output, session) {
    
    # Load survey images
    if (TRUE){
      image_list <-list.files("www/app_images/groupA", full.names = TRUE)
      image_list <- sample(image_list, length(image_list))
    } else {
      image_list <- list.files("www/app_images/groupB", full.names = TRUE)
      image_list <- sample(image_list, length(image_list))
    }
    
    
    v <- reactiveValues(
      imageNum = 1,
      responses = list(),
      time = Sys.time(),
      plot_order = 1
    )
    
    current_img <- reactive({
      image_list[v$imageNum]
    })
    
    
    output$out_img <- renderImage({
      list(src = current_img(), id = "taipan_current_img")
    }, deleteFile = FALSE)
    
    sceneInputs <- getInputID(questions$scene)
    demographicsInputs <- getInputID(questions$demographics)
    
    scene_vals <- reactive({
      vals <- map(sceneInputs, function(id){input[[id$id]]})
      names(vals) <- map_chr(sceneInputs, "id")
      vals
    })
    
    demographic_vals <- reactive({
      vals <- map(demographicsInputs, function(id){input[[id$id]]})
      names(vals) <- map_chr(demographicsInputs, "id")
      vals
    })
    
    output$ui_d_save <- renderUI({
      actionLink(
        "btn_saveInfo",
        box(
          "Save Info",
          width = 12,
          background = "blue"
          
        )
      )
    })
    
    output$ui_btn_next <- renderUI({
      if (v$imageNum != length(image_list)) {
        actionLink(
          "btn_next",
          box(
            "Next Image",
            width = 12,
            background = "green"
          )
        )
      }
      else {
        column(3)
      }
    })
    
    
    # Update the scene values when images change
    observeEvent(current_img(), {
      map(sceneInputs,
          function(io){
            # Update scene inputs
            val <- v$responses[[basename(current_img())]][["scene"]][[io$id]]
            if(!is.null(val)){
              session$sendInputMessage(
                io$id,
                list(
                  value = val,
                  selected = val
                )
              )
            }
          }
      )
      
    })
    
    observeEvent(scene_vals(), {
      v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
    })
    
    observeEvent(input$btn_next, {
      v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
      v$responses[[basename(current_img())]][["scene"]]$time <- Sys.time()
      v$responses[[basename(current_img())]][["scene"]]$plot_order <- match(current_img(), image_list)
    
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
      
      v$imageNum <- pmin(length(image_list), v$imageNum + 1)
    })
      
    output$out_img_info <- renderText({
      sprintf("Image: (%i/%i)",
              v$imageNum,
              length(image_list))
    })
    
    observeEvent(input$btn_saveInfo, {
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
      
      if (v$responses[[basename(current_img())]][["demographic"]]$consent==1) {
        showNotification(h3("Demographic information has been saved."), type = "message", duration = 1)
        
        # Switch to the survey tab
        updateTabItems(session = session, inputId = "tabs", selected = "Questions")
      } else{
      showNotification(h3("Consent must be given before you can proceed to questions."), type = "error", duration = 1)
      
      }
    })
    
    
    # change this to upload rows to survey google spreadsheet
    observeEvent(input$btn_export, 
      {
        v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
        v$responses[[basename(current_img())]][["scene"]]$time <- Sys.time()
        v$responses[[basename(current_img())]][["scene"]]$plot_order <- match(current_img(), image_list)
        v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
        out <- suppressWarnings( # hide coercion warnings
          v$responses %>%
            imap_dfr(
              function(img, image_name){
                scene_vals <- img$scene %>%
                  map(paste0, collapse = ", ")
                demographic_vals <- img$demographic %>%
                  map(paste0, collapse = ", ")
                df <- as.data.frame(c(image_name = image_name, scene_vals, demographic_vals))
                return(df)
              }
            )
        )
        # add the row of responses to google sheet
        gs_auth("data/authentication.rds")
        sheet <- gs_key("1PcKMmsojzljDZl4bftOu5lbG51AVYuiPIJFq2t7kiGg")
        gs_add_row(ss = sheet, ws = 1, input = out)
        
      }
    )
  }
)
