shinyServer(function(input, output, session) {

  output$plot <- renderPlot({
    #If NULL, nothing
    if (is.null(input$plotType))
      return(NULL)
    #If backlog, return a bar plot showing the backlog graphic
    if (input$plotType == "Backlog") {
    return(
    ggplot(data=apiResults$final)+
      geom_bar(stat="identity", aes(x=backlogBeginDate, fill=ticketState, y=ticketSize+1))+
      scale_fill_manual(values=c("#001A57","#B5B5B5")) +
      geom_vline(xintercept=as.numeric(tail(sprintDeadlines,n=1)), colour = "red")+
      geom_text(data=data.frame(x=tail(sprintDeadlines,n=1),y=paste0("DUE DATE:            ",tail(sprintDeadlines,n=1))),
                mapping=aes(x=x, y=0, label=y), size=4, angle=90, vjust=-0.4, hjust=0) +
      theme_bw()+
      theme(panel.grid.major=element_blank(),
            legend.title=element_blank(),
            legend.position="bottom")+
      ylab("Total Points")+
      xlab("Date")
    )
    #If current sprint, show it by week
    } else {
      tempDF <- currentSprintSum[which(currentSprintSum$sprintNo==input$plotType),]
      last   <- tail(tempDF$day,n=1)
      test   <- which(tempDF$day<=Sys.time())
      if (length(test)>0) {tempDF <- tempDF[test,]} else {tempDF <- tempDF[c(1),]}
      return(
        ggplot(data=tempDF)+
          geom_bar(stat="identity", aes(x=day, y=openSize, fill=openSize))+
          geom_vline(xintercept=as.numeric(last)+(2*60*60*24), colour = "red")+
          geom_text(data=data.frame(x=last+(2*60*60*24),y=paste0("Current Sprint Due Date:            ",last)),
                    mapping=aes(x=x, y=0, label=y), size=4, angle=90, vjust=-0.4, hjust=0) +
          theme_bw()+
          theme(panel.grid.major=element_blank(),
                legend.title=element_blank(),
                legend.position="none")+
          ylab("Total Points")+
          xlab("Date")
        
        )
    }
  })

#   output$summary <- renderPlot({    
#     ggplot(data=apiResults$final[!duplicated(apiResults$final$ticketOrder),])+
#     geom_bar(aes(x=factor(""),fill=assignedTicket),colour="black")+
#     coord_polar(theta="y")+
#     scale_x_discrete("")+
#       theme_minimal()+
#       theme(
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid=element_blank(),
#         axis.ticks = element_blank(),
#         axis.text.x=element_blank(),
#         legend.title=element_blank(),
#         legend.position="right",
#         plot.title=element_text(size=14, face="bold"))
#   })
  
  metrics <- reactive({
    if (is.null(input$plotType)) {return(NULL)} else if (input$plotType == "Backlog") {
    tempDF <- apiResults$final[which(apiResults$final$backlog==max(apiResults$final$backlog)),]
    last   <- tail(sprintDeadlines,n=1)
    avg <- sum(tempDF$ticketSize[which(tempDF$ticketState=="closed")])/sum(tempDF$ticketSize)
    req <- sum(tempDF$ticketSize)/as.numeric(tail(sprintDeadlines,n=1)-tempDF$backlogBeginDate[1])
    proj<- ceiling(sum(tempDF$ticketSize[which(tempDF$ticketState=="open")])/avg)
    finish <- tempDF$backlogBeginDate[1]+(proj*60*60*24)
    if (is.na(finish)) {late<-'***not started'} else if (finish>last) {late<-'projected late - get crackin\''} else {late<-'on target - good job!'} 
    return(list(avg=avg,
                req=req,
                proj=proj,
                finish=finish,
                late=late))
    } else {
    tempDF <- currentSprintSum[which(currentSprintSum$sprintNo==input$plotType),]
    last   <- tail(tempDF$day,n=1)
    test   <- which(tempDF$day<=Sys.time())
    if (length(test)>0) {tempDF <- tempDF[test,]} else {tempDF <- tempDF[c(1),]}
    avg <- mean(tempDF$closeSize)
    req <- tempDF$openSize[1] / as.numeric(last-tempDF$day[1])
    proj<- ceiling(tempDF$openSize[1]/avg)
    finish <- tempDF$day[1]+(proj*60*60*24)
    if (is.infinite(proj)) {late<-'need at least 1 closed ticket'} else if (finish>last) {late<-'projected late - get crackin\''} else {late<-'on target - good job!'}
    return(list(avg=avg,
                req=req,
                proj=proj,
                finish=finish,
                late=late))
    }
    
  })

  output$avg <- renderText({ paste0(ceiling(metrics()$avg)) })
  output$req <- renderText({ paste0(ceiling(metrics()$req)) })
  output$late<- renderText({ paste0(metrics()$late) })
  output$proj<- renderText({ paste0(metrics()$finish) })
})