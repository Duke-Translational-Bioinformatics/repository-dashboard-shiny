

shinyUI(fluidPage(
  
  title = "repository-dashboard-shiny",
  theme="style.css", 
  titlePanel("Github Repository Scrum Dashboard"),
  plotOutput('plot'),  
  fluidRow(
    column(3,
           h4("GitHub Scrum Dashboard"),
           selectInput(inputId='plotType', 
                       label='Please choose a dashboard:', 
                       choices = apiResults$sprints,
                       selected = 'Backlog')
    ),
    column(3,
           strong("Average Velocity: "),
           div(textOutput("avg"), style = "color:#CBEA60"),
           strong("Points / Day")
    ),
    column(3,
           strong("Required Velocity: "),
           div(textOutput("req"), style = "color:#CBEA60"),
           strong("Points / Day")
    ),
    column(3,
           strong("Projected Completion Date: "),
           div(textOutput("proj"), style = "color:#CBEA60"),
           div(textOutput("late"), style = "color:#ff0000")
    )
  )
))# 


