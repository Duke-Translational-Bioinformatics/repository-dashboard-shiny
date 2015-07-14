shinyServer(function(input, output, session) {
  
  #User Defined Functions to accomplish repetative tasks throughout the remaining code----------------------------------------  
  estimateCompleteDateNoWkds <- function(y,x){
    if (x==1) { 
      days <- tail(seq(as.Date(y), by = "day", length.out = x+1),n=1)
    } else {
      days <- seq(as.Date(y), by = "day", length.out = x)
    }
    logic <- (!is.weekend(days))
    added <- sum(logic==FALSE)
    if (added>0) {estimateCompleteDateNoWkds(y=tail(days,n=1),
                                             x=added)} else {return(tail(days,n=1))}
    
  }
  #--------------------------------------------------------------------------------------------------------------------------- 
  #Data will change based on user input (i.e. dynamic), so this function generates dynamic data------------------------------- 
  metrics <- reactive({
    #Not finished building loading yet ---------------------------------------
    if (is.null(input$plotType)) {
      return(NULL)
    #Default, or user selected Backlog graphic--------------------------------
      } else if (input$plotType == "Backlog") {
      tempDF      <- apiResults$final[which(apiResults$final$backlog==max(apiResults$final$backlog)),]
      metricDat   <- apiResults$finalll
      myDays      <- seq.Date(to   = as.Date(trunc(Sys.time(),'days')), from = as.Date(sprintDeadlines)[1], by   = 1)
      days2today  <- length(myDays[!is.weekend(myDays)])
      myDays      <- seq.Date(to   = as.Date(tail(sprintDeadlines,n=1)), from = as.Date(trunc(Sys.time(),'days')), by   = 1)
      days2end    <- length(myDays[!is.weekend(myDays)])
      last        <- tail(sprintDeadlines,n=1)
      avg         <- ceiling(sum(metricDat$ticketSize[which(metricDat$ticketState=="closed")])/days2today)
      req         <- ceiling(sum(metricDat$ticketSize[which(metricDat$ticketState=="open")])/ days2end)
      if (avg==0) {
        proj        <- NA
        finish      <- NA
        late        <- 'No tickets closed - need some momentum to project!'
        
      } else {
      proj        <- ceiling(sum(tempDF$ticketSize[which(tempDF$ticketState=="open")])/avg)
      #Added recursive function to determine the projected end date-----------------------------------------------
      #End of function and call it--------------------------------------------------------------------------------    
      finish <- estimateCompleteDateNoWkds(y=Sys.time(),x=proj)
      if (is.na(finish)) {late<-'***not started'} else if (as.Date(finish)>as.Date(last)) {late<-'projected late - get crackin\''} else {late<-'on target - good job!'} 
      }
      return(list(tempDF=tempDF,
                  avg=avg,
                  req=req,
                  proj=proj,
                  finish=finish,
                  late=late))
    #Any of the sprint chose
    } else if (input$plotType == "Historical Velocity") {
      dayz      <- seq.Date(to   = as.Date(trunc(Sys.time(),'days')), from = as.Date(sprintDeadlines)[1], by   = 1)
      days      <- dayz[!is.weekend(dayz)]
      temp      <- apiResults$output
      for (z in 1:(length(days))) {
        day                   <- days[z]
        openSize              <- sum(temp[which( (as.Date(temp$ticketCloseDate)>as.Date(day)) | (is.na(temp$ticketCloseDate))),]$ticketSize)
        closeSize             <- sum(temp[which(as.Date(temp$ticketCloseDate)<=as.Date(day)),]$ticketSize)
        avgVelocity           <- closeSize / z
#         closeSize             <- sum(currentSprint[which(as.Date(currentSprint$ticketCloseDate)==as.Date(day)),]$ticketSize)
#         sprintNo              <- unique(currentSprint$sprintNo)
        out                   <- data.frame(day,openSize,closeSize,avgVelocity)
        if (z==1) {final <- out} else {final <- rbind(final,out)}
      }
      metricDat   <- temp
      myDays      <- seq.Date(to   = as.Date(trunc(Sys.time(),'days')), from = as.Date(sprintDeadlines)[1], by   = 1)
      days2today  <- length(myDays[!is.weekend(myDays)])
      myDays      <- seq.Date(to   = as.Date(tail(sprintDeadlines,n=1)), from = as.Date(trunc(Sys.time(),'days')), by   = 1)
      days2end    <- length(myDays[!is.weekend(myDays)])
      last        <- tail(sprintDeadlines,n=1)
      avg         <- ceiling(sum(metricDat$ticketSize[which(metricDat$ticketState=="closed")])/days2today)
      req         <- ceiling(sum(metricDat$ticketSize[which(metricDat$ticketState=="open")])/ days2end)
      if (avg==0) {
        proj        <- NA
        finish      <- NA
        late        <- 'No tickets closed - need some momentum to project!'
        
      } else {
        proj        <- ceiling(sum(temp$ticketSize[which(temp$ticketState=="open")])/avg)
        #Added recursive function to determine the projected end date-----------------------------------------------
        #End of function and call it--------------------------------------------------------------------------------    
        finish <- estimateCompleteDateNoWkds(y=Sys.time(),x=proj)
        if (is.na(finish)) {late<-'***not started'} else if (as.Date(finish)>as.Date(last)) {late<-'projected late - get crackin\''} else {late<-'on target - good job!'} 
      }
      return(list(tempDF=final,
                  avg=avg,
                  req=req,
                  proj=proj,
                  finish=finish,
                  late=late))
      #Any of the sprint chose
    } else {
#       #Three scenarios exist: (1) Sprints that are complete - (2) Current Sprints - (3) Future Sprints
      tempDF      <- currentSprintSum[which(currentSprintSum$sprintNo==input$plotType),]
      tempDF$openMinusClose <- tempDF$openSize-tempDF$closeSize
      last        <- tail(tempDF$day,n=1)
      head        <- head(tempDF$day,n=1)
      test        <- which(tempDF$day<=Sys.time())
      if (length(test)>0) {tempDF2 <- tempDF[test,]} else {tempDF2 <- tempDF[c(1),]}
#       #Let's determin where we are:
#       #(1) Past Sprints?:
      if (as.Date(trunc(Sys.time(),'days'))>as.Date(last)){
        req         <- ceiling(sum(tail(tempDF$openMinusClose,n=1)))
              if (req>0){
                myDays      <- seq.Date(to   = as.Date(trunc(Sys.time(),'days')), from = as.Date(tempDF$day[1]), by   = 1)
                days2today  <- length(myDays[!is.weekend(myDays)])  
                avg         <- ceiling(sum(tempDF$closeSize)/days2today)   
                proj        <- ceiling(tail(tempDF$openSize,n=1)/avg)
                finish      <- estimateCompleteDateNoWkds(y=Sys.time(),x=proj)
                if (req>0) {status="Sprint Over: Open Tickets Still Exist"} else {status="Sprint Over: All Tickets Closed!"}   
              } else {
                myDays      <- seq.Date(to   = as.Date(tail(tempDF$day,n=1)), from = as.Date(tempDF$day[1]), by   = 1)
                days2today  <- length(myDays[!is.weekend(myDays)])  
                avg         <- ceiling(sum(tempDF$closeSize)/days2today)   
                proj        <- ceiling(tail(tempDF$openSize,n=1)/avg)
                finish      <- estimateCompleteDateNoWkds(y=Sys.time(),x=proj)
                if (req>0) {status="Sprint Over: Open Tickets Still Exist"} else {status="Sprint Over: All Tickets Closed!"}     
              }  
              traj        <- seq(from=head(tempDF$openSize,n=1), to=head(tempDF2$openSize,n=1)-(avg*nrow(tempDF)), length.out=nrow(tempDF))
              goal        <- seq(from=head(tempDF$openSize,n=1), to=0, length.out=nrow(tempDF))
              df2         <- data.frame(tempDF$day,goal,traj)
        #(2) FUTURE:
      } else if (as.Date(trunc(Sys.time(),'days'))<as.Date(head)) {
              traj        <- seq(from=head(tempDF2$openSize,n=1), to=0, length.out=nrow(tempDF))
              goal        <- seq(from=head(tempDF$openSize,n=1), to=0, length.out=nrow(tempDF))
              df2         <- data.frame(tempDF$day,goal,traj)
              myDays      <- seq.Date(to   = as.Date(tail(tempDF$day,n=1)), from = as.Date(head(tempDF$day,n=1)), by   = 1)
              days2end    <- length(myDays[!is.weekend(myDays)])
              req         <- ceiling(sum(tail(tempDF$openMinusClose,n=1))/ days2end)
              myDays      <- seq.Date(to   = as.Date(tail(tempDF$day,n=1)), from = as.Date(head(tempDF$day,n=1)), by   = 1)
              days2today  <- length(myDays[!is.weekend(myDays)])  
              avg         <- NA 
              proj        <- NA
              finish      <- NA
              status      <- "FUTURE Sprint - no status available at this time"
         #(3) PRESENT:
      } else {
              myDays      <- seq.Date(to   = as.Date(tail(tempDF$day,n=1)), from = as.Date(trunc(Sys.time(),'days')), by   = 1)
              days2end    <- length(myDays[!is.weekend(myDays)])
              req         <- ceiling(sum(tail(tempDF$openMinusClose,n=1))/ days2end)  
              myDays      <- seq.Date(to   = as.Date(trunc(Sys.time(),'days')), from = as.Date(tempDF$day[1]), by   = 1)
              days2today  <- length(myDays[!is.weekend(myDays)])  
              avg         <- ceiling(sum(tempDF$closeSize)/days2today)  
              proj        <- ceiling(tail(tempDF$openSize,n=1)/avg)
              finish      <- estimateCompleteDateNoWkds(y=Sys.time(),x=proj) 
              
              traj        <- seq(from=head(tempDF2$openSize,n=1), to=head(tempDF2$openSize,n=1)-(avg*nrow(tempDF)), length.out=nrow(tempDF))
              goal        <- seq(from=head(tempDF$openSize,n=1), to=0, length.out=nrow(tempDF))
              df2         <- data.frame(tempDF$day,goal,traj)
              if (as.Date(finish)>as.Date(last)) {status="projected late - get crackin\'"
              } else {status='on target - good job!'}
      }
     return(list(tempDF=tempDF,
              tempDF2=tempDF2,
              last=tail(tempDF$day,n=1),
              df2=df2,
              avg=avg,
              req=req,
              proj=proj,
              finish=finish,
              late=status))

  } 
})
  #--------------------------------------------------------------------------------------------------------------------------- 
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
    } else if (input$plotType == "Historical Velocity") {
      return(
        ggplot(data=metrics()$tempDF, aes(x=day, y=avgVelocity))+
          geom_point()+
          geom_line()+
          scale_colour_gradient(limits=c(0, max(metrics()$tempDF$avgVelocity)))+
          theme_bw()+
          theme(panel.grid.major=element_blank(),
                legend.title=element_blank(),
                legend.position="none")+
          ylab("Average Velocity")+
          xlab("Date")
      )
      #If current sprint, show it by week
    } 
      else {
      tempDF3 <- metrics()$tempDF2[!is.weekend(metrics()$tempDF2$day),]
      return(
        if (as.Date(trunc(Sys.time(),'days'))>as.Date(metrics()$last)){
        ggplot(data=tempDF3)+
          geom_bar(stat="identity", aes(x=day, y=openMinusClose, fill=openMinusClose))+
          geom_vline(xintercept=as.numeric(metrics()$last), colour = "#ca0020")+
          geom_text(data=data.frame(x=metrics()$last,y=paste0("        Sprint Due Date:            ",metrics()$last)),
                    mapping=aes(x=x, y=3, label=y), size=4, angle=90, vjust=-0.4, hjust=0) +
          theme_bw()+
          theme(panel.grid.major=element_blank(),
                legend.title=element_blank(),
                legend.position="none")+
          ylab("Total Points")+
          xlab("Date")
        } else {
          ggplot(data=tempDF3)+
            geom_bar(stat="identity", aes(x=day, y=openMinusClose, fill=openMinusClose))+
            geom_vline(xintercept=as.numeric(metrics()$last), colour = "#ca0020")+
            geom_text(data=data.frame(x=metrics()$last,y=paste0("        Sprint Due Date:            ",metrics()$last)),
                      mapping=aes(x=x, y=3, label=y), size=4, angle=90, vjust=-0.4, hjust=0) +
            geom_line(data=metrics()$df2, aes(x=tempDF.day, y=goal), colour="#f4a582", size=2)+
            geom_line(data=metrics()$df2, aes(x=tempDF.day, y=traj), colour="#ca0020", size=2)+
            theme_bw()+
            theme(panel.grid.major=element_blank(),
                  legend.title=element_blank(),
                  legend.position="none")+
            ylab("Total Points")+
            xlab("Date")
          
        }
        
        )
    }
  })
  #Return the following to the UI for display--------------------------------------------------------------------------------
  output$avg <- renderText({ paste0(ceiling(metrics()$avg)) })
  output$req <- renderText({ paste0(ceiling(metrics()$req)) })
  output$late<- renderText({ paste0(metrics()$late) })
  output$proj<- renderText({ paste0(metrics()$finish) })

  output$downloadData <- downloadHandler(
    filename = function() { paste('repoIssues.csv', sep='') },
    content = function(file) {
      write.csv(apiResults$output, file)
    }
  )
})