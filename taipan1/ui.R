library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "taipan",
      tags$li(class = "dropdown", downloadLink("btn_export", span(icon("save"), "Submit Survey"))),
      tags$li(class = "dropdown", a(href="https://github.com/srkobakian/experiment", target="_blank", span(icon("github"), "More Info")))
    ),  
    dashboardSidebar(
      sidebarMenu(
        menuItem("About you", tabName = "About_you", icon = icon("dashboard")),
        menuItem("Questions", tabName = "Questions", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "About_you",
                  box(
                  title = "Demographics",
                  questions$demographics,
                  div(uiOutput("ui_d_save")),
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE
                )
        ),
        
        tabItem(tabName = "Questions",
                includeScript("www/img_size.js"),
                includeCSS("www/taipan.css"),
                column(8,
                       box(
                         title = textOutput("out_img_info"),
                         div(class = "taipan_image_div",
                             imageOutput("out_img",
                                         inline = TRUE)
                         ),
                         width = 12,
                         status = "primary",
                         collapsible = FALSE
                       )),
                
                column(4,
                       box(
                         title = "Questions",
                         questions$scene,
                         div(
                           uiOutput("ui_btn_prev"),
                           uiOutput("ui_btn_next")),
                         width = 12,
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE
                       ),
                       
                )
        )
        
      )
      )
  )
)
