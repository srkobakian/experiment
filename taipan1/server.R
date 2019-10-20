library(shiny)
library(ggplot2)
library(purrr)

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
    questions <- readRDS("data/questions.Rds")
    image_list <- list.files("www/app_images", full.names = TRUE)
    
    v <- reactiveValues(
      imageNum = 1,
      responses = list()
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
    
    output$ui_save <- renderUI({
      actionLink(
        "btn_saveImage",
        box(
          "Save Image",
          width = 4,
          background = "blue"
          
        )
      )
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
            width = 4,
            background = "green"
          )
        )
      }
      else {
        column(3)
      }
    })
    
    output$ui_btn_prev <- renderUI({
      if (v$imageNum != 1) {
        actionLink(
          "btn_prev",
          box(
            "Previous Image",
            width = 4,
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
    
    observeEvent(input$btn_prev, {
      v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
      v$imageNum <- pmax(1, v$imageNum - 1)
    })
    
    observeEvent(input$btn_next, {
      v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
      v$imageNum <- pmin(length(image_list), v$imageNum + 1)
    })
    
    
    observeEvent(input$btn_saveImage, {
      showNotification(h3("Experiment answers have been saved."), type = "default")
      v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
      v$imageNum <- pmin(length(image_list), v$imageNum + 1)
      
    })
    
    output$out_img_info <- renderText({
      sprintf("Image: (%i/%i)",
              v$imageNum,
              length(image_list))
    })
    
    observeEvent(input$btn_saveInfo, {
      showNotification(h3("Demographic information has been saved."), type = "default")
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
    })
    
    output$btn_export <- downloadHandler(
      filename = function() {
        paste('experiment-export-', Sys.Date(), '.csv', sep='')
      },
      content = function(con){
        v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
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
        write.csv(out, con, row.names = FALSE)
      }
    )
  }
)
