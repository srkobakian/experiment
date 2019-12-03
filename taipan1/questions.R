
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
      textInput("contributor", "Please provide your Figure-Eight contributor ID: (necessary for payment)", 
                placeholder = "Contributor ID"),
      p("You will be shown a series of 12 displays.
        This survey should not take more than 10 minutes of your time,
        please answer all questions for each display."),
      tags$b("As this is an experiment, your responses will be used for analysis."),
      tags$b("By selecting the consent button below, you are indicating that you:"),
      p("-	Have read and understood the information document regarding this research project."),
      p("-	Have had any questions answered to your satisfaction."),
      p("-	Understand that if you have any additional questions you can contact the research team."),
      p("-	Understand that you are free to withdraw without comment or penalty."),
      p("-	Understand that if you have concerns about the ethical conduct of the research project you can contact the Research Ethics Advisory Team on +61 7 3138 5123 or email humanethics@qut.edu.au."), 
      radioButtons(inputId = "consent", label = "Do you consent to your responses being collected?", 
                   choices = c("Yes, I consent to my responses being used for research purposes and allow them to be published online in a de-identified form." = "Consented",
                               "No, I do not consent to my responses being used for research purposes and allow them to be published online in a de-identified form." = "No Consent"),
                   selected = character(0)),
      # Question 1, gender
      selectInput("gender",
                  "Select your preferred pronoun:",
                  choices = list("Choose one" = NA, 
                                 "He" = "He",
                                 "She" = "She", 
                                 "They" = "They",
                                 "Other" = "Other"), 
                  selected = NA),
      # Question 2, education
      selectInput("education",
                  "Select the highest level of education achieved:",
                  choices = list("Choose one" = NA, 
                                 "High School Diploma" = "High School",
                                 "Bachelors Degree (Undergraduate)" = "Bach",
                                 "Masters Degree (Post graduate)" = "Masters",
                                 "Doctorate (Post graduate)" = "Doctorate",
                                 "Other" = "Other"
                  ), 
                  selected = NA),
      # Question 3, age range
      selectInput("age",
                  "Select your age range:",
                  choices = list("Choose one" = NA, 
                                 "18 - 24" = "18 - 24",
                                 "25 - 34" = "25 - 34",
                                 "35 - 44" = "35 - 44",
                                 "45 - 54" = "45 - 54",
                                 "55+" = "55+",
                                 "Other" =  "Other"), 
                  selected = NA),
      # Question 4, Australia
      selectInput("australia",
                  "Have you lived in Australia?",
                  choices = list("Choose one" = NA, 
                                 "Yes"   = "Yes",
                                 "No"    = "No", 
                                 "Other" = "Other"), 
                  selected = NA) #,
      # # Question 5, email
      # textInput("email", "You will be able to see the results and analysis online at: https://github.com/srkobakian/experiment. Provide a contact email address if you would like to be sent the results of the survey.", 
      #   placeholder = "Email address")
      ),
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
                     "Clusters of colour" = "clusters",
                     "Colour trend across the areas" = "trend",
                     "Big differences between neighbouring areas" = "hotspots",
                     "All areas have similar colours" = "consistent"), selected = 0),
      # Question 3, certainty around their choice
      sliderInput("certainty", "How certain are you that the chosen map is different? 
                                  1 = Very uncertain, 3 = Neutral, 5 = Very certain",
                  min = 1, max = 5, value = 3)
    )
)
saveRDS(object = questions, file = "data/questions.Rds")
