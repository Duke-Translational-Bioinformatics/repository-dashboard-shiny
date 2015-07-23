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
  #Create a dataframe that can be used for all the graphics
  graphData <- reactive({
    dayz                    <- seq.Date(to   = as.Date(trunc(Sys.time(),'days')), from = as.Date(sprintDeadlines)[1], by   = 1)
    days                    <- dayz[!is.weekend(dayz)]
    if (input$plotType %in% c('Historical Velocity','Backlog')) {
      temp                  <- apiResults$output
    } else {temp            <- apiResults$output[which(apiResults$output$sprintNo==input$plotType),]}
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
    if (input$plotType %in% c('Historical Velocity','Backlog')) {
      return(final)
    } else {
      temps                 <- final[which(final$day>=as.Date(apiResults$sprintDF[which(apiResults$sprintDF$sprintNo==input$plotType),]$sprintBeginDT)),]
      pos1                  <- which(temps$openSize %in% 0)[1] 
      if (!is.na(pos1)) {
        return(temps[c(1:pos1),])
      } else { return(temps)}
    }
  })
  
  #--------------------------------------------------------------------------------------------------------------------------- 
  #Data will change based on user input (i.e. dynamic), so this function generates dynamic data------------------------------- 
  metrics <- reactive({
    #Not finished building loading yet ---------------------------------------
    if (is.null(input$plotType)) {
      return(NULL)
      #Default, or user selected Backlog graphic--------------------------------
    } else if (input$plotType %in% c("Backlog","Historical Velocity")) {
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
    } else {
      #       #Three scenarios exist: (1) Sprints that are complete - (2) Current Sprints - (3) Future Sprints  
      sprintInfo            <- apiResults$sprintDF[which(apiResults$sprintDF$sprintNo==input$plotType),]
      last                  <- as.Date(sprintInfo$sprintEndDT)
      head                  <- as.Date(sprintInfo$sprintBeginDT)
      lineDays              <- seq.Date(to   = as.Date(sprintInfo$sprintEndDT), from = as.Date(sprintInfo$sprintBeginDT), by   = 1)
      lineLength            <- length(lineDays)
      #       #Let's determin where we are:
      #       #(1) Past Sprints?:
      if (as.Date(trunc(Sys.time(),'days'))>as.Date(last)) {
        req         <- ceiling(tail(graphData()$openSize,n=1))
        if (req>0){
          myDays      <- seq.Date(to   = as.Date(trunc(Sys.time(),'days')), from = as.Date(sprintInfo$sprintBeginDT), by   = 1)
          days2today  <- length(myDays[!is.weekend(myDays)])  
          avg         <- tail(graphData()$closeSize,n=1)/days2today   
          proj        <- tail(graphData()$openSize,n=1)/avg
          finish      <- estimateCompleteDateNoWkds(y=Sys.time(),x=proj)
          status      <- "Sprint Over: Open Tickets Still Exist"
        } else {
          myDays      <- seq.Date(to   = as.Date(tail(graphData()$day,n=1)), from = as.Date(sprintInfo$sprintBeginDT), by   = 1)
          days2today  <- length(myDays[!is.weekend(myDays)])  
          avg         <- tail(graphData()$closeSize,n=1)/days2today  
          proj        <- tail(graphData()$openSize,n=1)/avg
          finish      <- estimateCompleteDateNoWkds(y=Sys.time(),x=proj)
          status      <- "Sprint Over: All Tickets Closed!"     
        }  
        traj        <- seq(from=head(graphData()$openSize,n=1), to=head(graphData()$openSize,n=1)-(avg*lineLength), length.out=lineLength)
        goal        <- seq(from=head(graphData()$openSize,n=1), to=0, length.out=lineLength)
        df2         <- data.frame(lineDays,goal,traj)
        colnames(df2)<-c('day','goal','traj')
        #(2) FUTURE:
      } else if (as.Date(trunc(Sys.time(),'days'))<as.Date(head)) {
        traj        <- seq(from=head(graphData()$openSize,n=1), to=0, length.out=lineLength)
        goal        <- seq(from=head(graphData()$openSize,n=1), to=0, length.out=lineLength)
        df2         <- data.frame(lineDays,goal,traj)
        colnames(df2)<-c('day','goal','traj')
        myDays      <- seq.Date(to   = as.Date(tail(graphData()$day,n=1)), from = as.Date(head(graphData()$day,n=1)), by   = 1)
        days2end    <- length(myDays[!is.weekend(myDays)])
        req         <- ceiling(sum(tail(graphData()$openMinusClose,n=1))/ days2end)
        myDays      <- seq.Date(to   = as.Date(tail(graphData()$day,n=1)), from = as.Date(head(graphData()$day,n=1)), by   = 1)
        days2today  <- length(myDays[!is.weekend(myDays)])  
        avg         <- NA 
        proj        <- NA
        finish      <- NA
        status      <- "FUTURE Sprint - no status available at this time"
        #(3) PRESENT:
      } else {
        myDays      <- seq.Date(to   = as.Date(sprintInfo$sprintEndDT), from = as.Date(trunc(Sys.time(),'days')), by   = 1)
        days2end    <- length(myDays[!is.weekend(myDays)])
        req         <- tail(graphData()$openSize,n=1)/ days2end  
        myDays      <- seq.Date(to   = as.Date(trunc(Sys.time(),'days')), from = as.Date(sprintInfo$sprintBeginDT), by   = 1)
        days2today  <- length(myDays[!is.weekend(myDays)])  
        avg         <- tail(graphData()$closeSize,n=1)/days2today 
        proj        <- tail(graphData()$openSize,n=1)/avg
        if (avg==0) {  finish<-NA} else {
          finish      <- estimateCompleteDateNoWkds(y=Sys.time(),x=proj)  
        }
        traj        <- seq(from=head(graphData()$openSize,n=1), to=head(graphData()$openSize,n=1)-(avg*lineLength), length.out=lineLength)
        goal        <- seq(from=head(graphData()$openSize,n=1), to=0, length.out=lineLength)
        df2         <- data.frame(lineDays,goal,traj)
        colnames(df2)<-c('day','goal','traj')
        if (avg==0) {
          status='No sprint tickets closed yet.'
        } else if (as.Date(finish)>as.Date(last)) {status="projected late - get crackin\'"
        }  else {status='on target - good job!'}
      }
      return(  list(last=as.Date(sprintInfo$sprintEndDT),
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
        ggplot(data=graphData(), aes(x=day, y=avgVelocity))+
          geom_point()+
          geom_line()+
          scale_colour_gradient(limits=c(0, max(graphData()$avgVelocity)))+
          theme_bw()+
          theme(panel.grid.major=element_blank(),
                legend.title=element_blank(),
                legend.position="none")+
          ylab("Average Velocity")+
          xlab("Date")
      )
      #If current sprint, show it by week
    } else {
      
      return(
          ggplot(data=graphData())+
            geom_bar(stat="identity", aes(x=day, y=openSize, fill=openSize))+
            geom_vline(xintercept=as.numeric(metrics()$last), colour = "#ca0020")+
            geom_text(data=data.frame(x=metrics()$last,y=paste0("        Sprint Due Date:            ",metrics()$last)),
                      mapping=aes(x=x, y=3, label=y), size=4, angle=90, vjust=-0.4, hjust=0) +
            geom_line(data=metrics()$df2, aes(x=metrics()$df2$day, y=goal), colour="#f4a582", size=2)+
            geom_line(data=metrics()$df2, aes(x=metrics()$df2$day, y=traj), colour="#ca0020", size=2)+
            theme_bw()+
            theme(panel.grid.major=element_blank(),
                  legend.title=element_blank(),
                  legend.position="none")+
            ylab("Total Points")+
            xlab("Date")     
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