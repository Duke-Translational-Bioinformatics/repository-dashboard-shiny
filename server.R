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
        ggplot(data=currentSprintSum$df)+
          geom_bar(stat="identity", aes(x=day, y=openSize, fill=openSize))+
          geom_vline(xintercept=as.numeric(currentSprintSum$lastDay), colour = "red")+
          geom_text(data=data.frame(x=currentSprintSum$lastDay,y=paste0("Current Sprint Due Date: ",currentSprintSum$lastDay)),
                    mapping=aes(x=x, y=0, label=y), size=4, angle=90, vjust=-0.4, hjust=0) +
          theme_bw()+
          theme(panel.grid.major=element_blank(),
                legend.title=element_blank(),
                legend.position="none")+
          ylab("Size of Task")+
          xlab("Date")
        
        )
    }
  })

  output$summary <- renderPlot({    
    ggplot(data=apiResults[!duplicated(apiResults$ticketOrder),])+
    geom_bar(aes(x=factor(""),fill=assignedTicket),colour="black")+
    coord_polar(theta="y")+
    scale_x_discrete("")+
      theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text.x=element_blank(),
        legend.title=element_blank(),
        legend.position="right",
        plot.title=element_text(size=14, face="bold"))
  })
  
  output$table <- renderDataTable({
    cars
  })
})