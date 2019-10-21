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
                placeholder = "id"),
      # Question 1, gender
      selectInput("gender",
                  h3("Select your preferred pronoun:"),
                  choices = list("Choose one" = 1, 
                                 "She" = 2, 
                                 "He" = 3,
                                 "They" = 4,
                                 "other = 5"), 
                  selected = 1),
      # Question 2, education
      selectInput("education",
                  h3("Select the highest level of education achieved:"),
                  choices = list("Choose one" = 1, 
                                 "High School Diploma" = 2,
                                 "Bachelors Degree" = 3,
                                 "Master Degree" = 4,
                                 "Doctorate" = 5,
                                 "Other" = 6
                  ), 
                  selected = 1),
      # Question 3, age range
      selectInput("education",
                  h3("Select your age range:"),
                  choices = list("Choose one" = 1, 
                                 "18 - 24" = 2,
                                 "25 - 34" = 3,
                                 "35 - 44" = 4,
                                 "45 - 54" = 5,
                                 "55+" = 6,
                                 "Other" = 7
                  ), 
                  selected = 1),
      # Question 4, Australia
      selectInput("australia",
                  h3("Have you lived in Australia?"),
                  choices = list("Choose one" = 1, 
                                 "Yes" = 2,
                                 "No" = 3, 
                                 "Other" = 4), 
                  selected = 1)
    ),
  scene =
    div(
      # Question 1, the choice of plot
      selectInput("select",
                  h3("Which map is most different from the others?"),
                  choices = list("Choose a plot number:" = 0,
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
                                 "12" = 12
                  ), selected = 1),
      # Question 2, reason for their choice
      checkboxGroupInput("reason",
                         h3("What makes your chosen map different?:"),
                         choices = list(
                           "None of these reasons" = 0,
                           "Clusters of colour" = 1,
                           "Colour trend across the areas" = 2,
                           "Big differences between neighbours" = 34)),
      # Question 3, certainty around their choice
      sliderInput("certainty", h3("How certain are you that the chosen map is different?"),
                  min = 1, max = 5, value = 3)
    )
)

saveRDS(questions, file = "data/questions.Rds")
saveRDS(questions, file = "taipan1/data/questions.Rds")
