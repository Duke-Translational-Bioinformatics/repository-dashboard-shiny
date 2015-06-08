shinyUI(navbarPage("GitHub Issue Dashboard", theme="style.css",
                   tabPanel("Plot",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons(inputId="plotType", 
                                             label  ="Plot type",
                                             choices=c("Backlog"="b","Current Sprint Burndown"="c")
                                )
                              ),
                              mainPanel(
                                plotOutput("plot")
                              )
                            )
                   ),
                   tabPanel("Summary",
                            verbatimTextOutput("summary")
                   ),
                   navbarMenu("More",
                              tabPanel("Table",
                                       dataTableOutput("table")
                              )
                   )
))