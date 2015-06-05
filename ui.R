shinyUI(navbarPage("GitHub Issue Dashboard",
                   tabPanel("Plot",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons(inputId="plotType", 
                                             label  ="Plot type",
                                             choices=c("Scatter"="p","Line"="l")
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