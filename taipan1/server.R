library(shiny)
library(ggplot2)
library(purrr)
# Load survey questions
questions <- readRDS("data/questions.Rds")


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
    
    # Check connection to google sheet
    
    sheet <- tryCatch({
      gs_auth("data/authentication.rds")
      sheet <- gs_key("1PcKMmsojzljDZl4bftOu5lbG51AVYuiPIJFq2t7kiGg")
        }, error = function(e){ 
          message("Access has not been granted, please try again in 5 minutes.")
          return(NULL)
          })
    # Unique identifier function:
    create_unique_id <- function(seed_no = 100, char_len = 7){
      set.seed(seed_no)
      pool <- c(letters, LETTERS, 0:9)
      
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
      this_res
    }
    
    identifier <- create_unique_id()
    
    # Allocate to group A or B
    group_allocation <- sample(c("A", "B"), 1)
    
    # Load survey images
    image_list <- list.files(paste0("www/app_images/group", 
                             group_allocation), full.names = TRUE)
    image_list <- sample(image_list, length(image_list))
    
    
    v <- reactiveValues(
      imageNum = 1,
      responses = list(),
      start_time = Sys.time(),
      finish_time = Sys.time(),
      plot_order = 1,
      group = group_allocation
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
        div(
        p("Thank you for your participation"),
        p("Please click the SUBMIT button at the top of the window."), 
        p("It may take a minute to provide your unique verification code."))
      }
    })
    
    
    # Update the scene values when images change
    observeEvent(current_img(), {
      
      # return scene questions to original state
      updateSelectInput(session, "choice",
                        selected = 0)
      updateSelectInput(session, "reason",
                        selected = 0)
      updateSelectInput(session, "certainty",
                        selected = 3)
      
    })
    
    observeEvent(scene_vals(), {
      v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
    })
    
    observeEvent(input$btn_next, {
      #if (input$choice==0){
      #  showNotification(h3("Please choose one of the twelve maps shown."), 
      #                   type = "message", duration = 3)
      #  
      #} else {
      
      v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
      v$responses[[basename(current_img())]][["scene"]]$plot_order <- match(current_img(), image_list)
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
      v$responses[[basename(current_img())]][["demographic"]]$identifier <- identifier
      
      
      # Time calculations
      v$responses[[basename(current_img())]][["scene"]]$time_taken <- (Sys.time() - v$start_time)
      v$responses[[basename(current_img())]][["scene"]]$start_time <- v$start_time
      v$responses[[basename(current_img())]][["scene"]]$end_time <- Sys.time()
      # Upate time for next plot
      v$start_time <- Sys.time()
      
      v$imageNum <- pmin(length(image_list), v$imageNum + 1)
      #}
    })
      
    output$out_img_info <- renderText({
      sprintf("Image: (%i/%i)",
              v$imageNum,
              length(image_list))
    })
    
    observeEvent(input$btn_saveInfo, {
      
      
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
      
      if (nchar(input$contributor) < 8) {
        # check contributor ID
        showNotification(h3("Please check that you have provided your Figure-Eight contributor ID."), 
          type = "message", duration = 3, closeButton = TRUE)
      } else{
        # show ID
        showNotification(h3(paste("You have provided the ID:", input$contributor)), 
          type = "message", duration = 3, closeButton = TRUE)
      }
      
      
      if (input$consent == "Consented") {
        showNotification(h3("Demographic information has been saved."), type = "message", duration = 1)
        
        # Switch to the survey tab
        updateTabItems(session = session, inputId = "tabs", selected = "Questions")
        v$start_time <- Sys.time()
      } else{
      showNotification(h3("Consent must be given before you can proceed to questions."), type = "error", duration = 1)
      }
    })
    
    
    # change this to upload rows to survey google spreadsheet
    observeEvent(input$btn_export, 
      {
        # Switch to the thank you tab
        updateTabItems(session = session, inputId = "tabs", selected = "Thank_you")
        
        # Create a Progress object
        progress <- Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Sending survey responses. Please wait for your validation code.", value = 0)
        
        
        v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
        v$responses[[basename(current_img())]][["scene"]]$plot_order <- match(current_img(), image_list)
        v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()

        v$responses[[basename(current_img())]][["demographic"]]$identifier <- identifier
        
        progress$inc(amount = 0.2, message = NULL, detail = NULL)
        # Time calculations
        v$responses[[basename(current_img())]][["scene"]]$time_taken <- as.character((Sys.time() - v$start_time))
        v$responses[[basename(current_img())]][["scene"]]$start_time <- v$start_time
        v$responses[[basename(current_img())]][["scene"]]$end_time <- Sys.time()
        
        progress$inc(amount = 0.1, message = NULL, detail = NULL)
        out <- ""
        out <- suppressWarnings( # hide coercion warnings
          v$responses %>%
            imap_dfr(
              function(img, image_name, group = v$group){
                scene_vals <- img$scene %>%
                  map(paste0, collapse = ", ")
                demographic_vals <- img$demographic %>%
                  map(paste0, collapse = ", ")
                
                progress$inc(amount = 0.05, message = NULL, detail = NULL)
                df <- as.data.frame(c(group = group, 
                                      demographic_vals,
                                      image_name = image_name,
                                      scene_vals))
                return(df)
              }
            )
        )
      
        progress$inc(amount = 0.1, message = NULL, detail = NULL)
        # add the row of responses to google sheet
          
        gs_add_row(ss = sheet, ws = 2, input = out)
        
        showNotification(
          h3(paste("Your unique code for Figure-Eight: ", identifier)), 
            type = "message", duration = 10, closeButton = TRUE)
        
        
      }
    )
  }
)
