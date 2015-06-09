################################################
# Program: global.R
# Programmer: Ben Neely
# Date: 6/5/15
# Dependencies: shiny,httr,ggplot2
################################################
library(httr)
library(ggplot2)
library(plotrix)
#This should be set by user on screen
repoURL = "https://api.github.com/repos/Duke-Translational-Bioinformatics/duke-data-service/issues?state=all"
sprintDeadlines <- c(strptime("2015-05-01T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-06-08T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-06-26T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-07-17T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-08-07T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-08-28T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-09-18T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"))
#No matter pagination or not, below we need a function to parse the api response and create a consumable data type (data.frame) for shiny
apiConsumer <- function(x) { #x is of type 'response' from the httr package-----------------------------------------------
                         titles <- sapply(content(x,"parsed"), function(y) return(y$title))
                         user   <- as.character(sapply(content(x,"parsed"), function(y) return(y$assignee$login)))
                         ticketOrder <- sapply(content(x,"parsed"), function(y) return(y$number))
                         ticketState <- sapply(content(x,"parsed"), function(y) return(y$state))
                         ticketOpenDate <- as.character(sapply(content(x,"parsed"), function(y) return(y$created_at)))
                         ticketCloseDate <- as.character(sapply(content(x,"parsed"), function(y) return(y$closed_at)))
                         #Need to Identify a way to get size, this is current stuck
                         #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#
                         #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#
                         #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#
                         #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#
                         size <- rep(1,length(ticketState))
                         aa<-data.frame("ticketTitle"=titles,
                                    "assignedTicket"=user,
                                    "ticketOrder"=ticketOrder,
                                    "ticketState"=ticketState,
                                    "ticketOpenDate"=ticketOpenDate,
                                    "ticketCloseDate"=ticketCloseDate,
                                    "ticketSize"=size)
                         aa$ticketOpenDate <- strptime(aa$ticketOpenDate, "%Y-%m-%dT%H:%M:%SZ")
                         aa$ticketCloseDate <- strptime(aa$ticketCloseDate, "%Y-%m-%dT%H:%M:%SZ")
                         return(aa)
}#-----------------------------------------------------------------------------------------------------------------------
#grab the repo api header (for issues specifically)
h <- headers(HEAD(repoURL))$link
#Look through the header, parse out the header link and GET the paginated URLs
makeDataFrame <- function(h) {#------------------------------------------------------------------------------------------
  if (!is.null(h)) {
    #if so, need to parse a link header - consider making this a function
    parts = unlist(strsplit(h,','))
    section = lapply(parts,strsplit,';')
    #We only want the URL of the rel=last tag so that we know all the URLs to call
    getLast <- function(x) {#--------------------------------------------------------------------------------------------
      s <- unlist(x)
      if (gsub("^\\s+|\\s+$", "", s[2])=="rel=\"last\"") {
        urlLast= gsub("^\\s+<|>$","",s[1])
        urlstr = unlist(strsplit(urlLast,"page="))
        urlPrefix = paste0(urlstr[1],'page=')
        totPages  = urlstr[2]
        return(list("urlPrefix"=urlPrefix,
                    "totPages"=totPages))
      } 
    }#-------------------------------------------------------------------------------------------------------------------
    info <- lapply(section,getLast)
    info <- unlist(info[!sapply(info,is.null)])  
    #Let's create several objects that get all the issues we need
    myURLS <- paste(info["urlPrefix"],1:as.numeric(info["totPages"]),sep="")
    #there has to be a better way than this, but can't get apply nor do.call to return a 'response' object
    for (i in 1:length(myURLS)) {
      temp <- GET(myURLS[i])
      temp <- apiConsumer(temp)
      if (i==1) { output <- temp } else {output <- rbind(temp,output)}
      }
  } else {
    output <- apiConsumer(GET(repoURL))
  }
  #We only want to consider sprints that are in the past or touch the most recent date/time:
  indx <- which(sprintDeadlines<Sys.time())
  indexx <- c(indx,tail(indx,n=1)+1)
  sprintDeadlinesToday <- sprintDeadlines[indexx]
  #Between sprint dates we'll need metrics to display the backlog - not sure how this will work for open / close / open /close workflows
  for (j in 1:(length(sprintDeadlinesToday)-1)) {
    temp <- output[which( ((output$ticketOpenDate>=sprintDeadlinesToday[j]) & (output$ticketOpenDate<sprintDeadlinesToday[j+1])) |
                             ((output$ticketOpenDate<sprintDeadlinesToday[j+1]) & (output$ticketState=='open'))),]
    temp$sprint <- rep(paste0("Sprint ",j-1),nrow(temp))
    temp$sprintBeginDate <-  sprintDeadlinesToday[j] 
    temp$sprintEndDate <-  sprintDeadlinesToday[j+1] 
    temp$currentSprint <- rep(0,nrow(temp))
    if (j==1) {final <- temp} else { final <- rbind(final,temp)}
  }
  final <- final[order(final$sprint,-as.numeric(final$ticketState)),]
  return(final)
}#-----------------------------------------------------------------------------------------------------------------------
#GET the data and put in a dataframe that can be used for backlog and other metrics
apiResults <- makeDataFrame(h)
#------------------------------------------------------------------------------------------------------------------------
bySprint <- function(x) { #Assumes a data.frame from makeDataFrame is supplied
  currentSprint <- x[which(x$sprint==max(x$sprint)),]
  days <- seq(unique(currentSprint$sprintBeginDate),unique(currentSprint$sprintEndDate),by="days")
  for (z in 1:(length(days))) {
    day <- days[z]
    openSize <- sum(currentSprint$ticketSize)
    closeSize <- nrow(currentSprint[which(as.Date(currentSprint$ticketCloseDate)==as.Date(day)),])
    out <- data.frame(day,openSize,closeSize)
    if (z==1) {final <- out} else {final <- rbind(final,out)}
    if (sum(as.Date(currentSprint$ticketCloseDate)==as.Date(day),na.rm=T)>0) {
    currentSprint <- currentSprint[-which(as.Date(currentSprint$ticketCloseDate)==as.Date(day)),]
    }
  }
  final <- final[which(as.numeric(as.Date(final$day))<=as.numeric(as.Date(as.POSIXlt(Sys.time(), "GMT")))),]
  return(list(df=final,
              lastDay=tail(days,n=1)))
}
currentSprintSum <- bySprint(apiResults)

#gut check
#GET("https://api.github.com/rate_limit")