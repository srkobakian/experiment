
surveyQuestions <- function(demographics, scene){
  structure(
    list(demographics = demographics, scene = scene),
    class = "taipanQuestions"
  )
}
questions <- surveyQuestions(
  demographics =
    div(
      # Contributor ID
      textInput("contributor", "Please provide a unique ID, eg. a favourite animal:", 
                placeholder = "id"),
      p("You will be shown a series of 12 displays.
        This survey should not take more than 10 minutes of your time,
        please answer all questions for each display."),
      tags$b("As this is a pilot study, your responses will not be used for analysis."),
      radioButtons("consent", "Do you consent to your responses being collected?\n
                   .", 
                   choices = c("Yes, I consent to my responses being used for research purposes and allow them to be published online in a de-identified form." = 1,
                               "No, I do not consent to my responses being used for research purposes and allow them to be published online in a de-identified form." = 2),
                   selected = character(0)),
      # Question 1, gender
      selectInput("gender",
                  "Select your preferred pronoun:",
                  choices = list("Choose one" = 1, 
                                 "She" = 2, 
                                 "He" = 3,
                                 "They" = 4,
                                 "Other" = 5), 
                  selected = 1),
      # Question 2, education
      selectInput("education",
                  "Select the highest level of education achieved:",
                  choices = list("Choose one" = 1, 
                                 "High School Diploma" = 2,
                                 "Bachelors Degree (Undergraduate)" = 3,
                                 "Masters Degree (Post graduate)" = 4,
                                 "Doctorate (Post graduate)" = 5,
                                 "Other" = 6
                  ), 
                  selected = 1),
      # Question 3, age range
      selectInput("age",
                  "Select your age range:",
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
                  "Have you lived in Australia?",
                  choices = list("Choose one" = 1, 
                                 "Yes" = 2,
                                 "No" = 3, 
                                 "Other" = 4), 
                  selected = 1)),
  scene =
    div(
      # Question 1, the choice of plot
      selectInput("choice",
                  "Which map is most different from the others?",
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
                  ), selected = 0),
      # Question 2, reason for their choice
      radioButtons("reason",
                   "What makes your chosen map different?:",
                   choices = list(
                     "None of these reasons" = 0,
                     "Clusters of colour" = 1,
                     "Colour trend across the areas" = 2,
                     "Big differences between neighbours" = 3,
                     "All areas have similar colours" = 4), selected = 0),
      # Question 3, certainty around their choice
      sliderInput("certainty", "How certain are you that the chosen map is different? 
                                  1 = Very uncertain, 3 = Neutral, 5 = Very certain",
                  min = 1, max = 5, value = 3)
    )
)
saveRDS(questions, file = "data/questions.Rds")