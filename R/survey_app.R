###############################################################################
##############################    SURVEY APP     ##############################
###############################################################################

# demographics and scene questions

taipanQuestions <- function(demographics, scene){
  structure(
    list(demographics = demographics, scene = scene),
    class = "taipanQuestions"
  )
}
questions <- taipanQuestions(
  demographics =
    div(
      # Contributor ID
      textInput("contributor", label = "For payment, provide Figure Eight Contributor ID:", 
                placeholder = "Contributor ID"),
      # Question 1, gender
      selectInput("gender",
                  h3("Select your gender:"),
                  choices = list("Choose one" = 1, 
                                 "Female" = 2, 
                                 "Male" = 3), 
                  selected = 1),
      # Question 2, education
      selectInput("education",
                  h3("Select the highest level of education achieved:"),
                  choices = list("Choose one" = 1, 
                                 "High School Diploma" = 2,
                                 "Bachelors Degree" = 3,
                                 "Master Degree" = 4,
                                 "Doctorate" = 4
                  ), 
                  selected = 1),
      # Question 3, age range
      selectInput("age",
                  h3("Select your age range:"),
                  choices = list("Choose one" = 1, 
                                 "18 - 24" = 2,
                                 "25 - 34" = 3,
                                 "35 - 44" = 4,
                                 "45 - 54" = 5,
                                 "55+" = 6
                  ), 
                  selected = 1),
      # Question 4, Australia
      selectInput("australia",
                  h3("Have you lived in Australia?"),
                  choices = list("Choose one" = 1, 
                                 "Yes" = 2,
                                 "No" = 3), 
                  selected = 1)
    ),
  scene =
    div(
      # Question 1, the choice of plot
      selectInput("select",
                  h3("Which map is most different from the others?"),
                  choices = list("Choose a map" = 0,
                                 "1" = 1, 
                                 "2" = 2,
                                 "3" = 3, 
                                 "4" = 4, 
                                 "5" = 5,
                                 "6" = 6, 
                                 "7" = 7, 
                                 "8" = 8,
                                 "9" = 9, 
                                 "10" = 10, 
                                 "11" = 11,
                                 "12" = 12), selected = 0),
      # Question 2, reason for their choice
      radioButtons("reason",
                   h3("What makes your chosen map different?:"),
                   choices = list(
                     "Groups of red areas" = 1,
                     "Groups of orange areas" = 2,
                     "Groups of blue areas" = 3,
                     "Contrasting colours nearby" = 4,
                     "Other" = 4)),
      # Question 3, certainty around their choice
      sliderInput("certainty", h3("How confident do you feel about this choice?<br> 0 = Not confident, 10 = Very confident."),
                  min = 0, max = 10, value = 5, ticks = FALSE)
    )
)
saveRDS(questions, file = "data/questions.Rds")
saveRDS(questions, file = "taipan1/data/questions.Rds")
