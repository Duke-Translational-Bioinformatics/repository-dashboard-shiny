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
           strong("Projected Completion Date (at current velocity): "),
           div(textOutput("proj"), style = "color:#CBEA60"),
           div(textOutput("late"), style = "color:#ff0000")
           
    ),
    column(3,
           strong("Required Velocity (to complete): "),
           div(textOutput("req"), style = "color:#CBEA60"),
           strong("Points / Day")
    )
  )
))# 


