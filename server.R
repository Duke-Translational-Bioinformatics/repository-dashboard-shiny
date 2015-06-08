shinyServer(function(input, output, session) {

  output$plot <- renderPlot({
    #If NULL, nothing
    if (is.null(input$plotType))
      return(NULL)
    #If backlog, return a bar plot showing the backlog graphic
    if (input$plotType == "b") {
    return(
    ggplot(data=apiResults)+
      geom_bar(stat="identity", aes(x=sprintBeginDate, fill=ticketState, y=ticketSize))+
      scale_fill_manual(values=c("#001A57","#B5B5B5")) +
      geom_vline(xintercept=as.numeric(tail(sprintDeadlines,n=1)), colour = "red")+
      geom_text(data=data.frame(x=tail(sprintDeadlines,n=1),y=paste0("Due Date: ",tail(sprintDeadlines,n=1))),
                mapping=aes(x=x, y=0, label=y), size=4, angle=90, vjust=-0.4, hjust=0) +
      theme_bw()+
      theme(panel.grid.major=element_blank(),
            legend.title=element_blank(),
            legend.position="bottom")+
      ylab("Size of Task")+
      xlab("Date"))
    #If current sprint, show it by week
    } else if (input$plotType == "c") {
      return(
        ggplot(data=currentSprintSum)+
          geom_bar(stat="identity", aes(x=day, y=openSize, fill=openSize))+
          geom_vline(xintercept=as.numeric(max(currentSprintSum$day))+1, colour = "red")+
          theme_bw()+
          theme(panel.grid.major=element_blank(),
                legend.title=element_blank(),
                legend.position="none")+
          ylab("Size of Task")+
          xlab("Date")
        
        )
    }
  })

  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- renderDataTable({
    cars
  }, options=list(pageLength=10))
})