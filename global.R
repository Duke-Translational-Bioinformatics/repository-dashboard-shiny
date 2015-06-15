################################################
# Program: global.R
# Programmer: Ben Neely
# Date: 6/5/15
# Dependencies: shiny,httr,ggplot2
################################################
library(httr)
library(ggplot2)
#This should be set by user on screen
repoURL = "https://api.github.com/repos/Duke-Translational-Bioinformatics/duke-data-service/issues?state=all"
sprintDeadlines <- c(strptime("2015-05-01T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-06-08T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-06-26T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-07-17T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-08-07T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-08-28T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"),
                     strptime("2015-09-18T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"))
sizeColor = "fad8c7" #hexidecimal color for Github label that indicates size of the task (for weighting)
#No matter pagination or not, below we need a function to parse the api response and create a consumable data type (data.frame) for shiny
apiConsumer <- function(x) { #x is of type 'response' from the httr package-----------------------------------------------
                         titles       <- sapply(content(x,"parsed"), function(y) return(y$title))
                         user         <- as.character(sapply(content(x,"parsed"), function(y) return(y$assignee$login)))
                         ticketOrder  <- sapply(content(x,"parsed"), function(y) return(y$number))
                         ticketState  <- sapply(content(x,"parsed"), function(y) return(y$state))
                         ticketOpenDate <- as.character(sapply(content(x,"parsed"), function(y) return(y$created_at)))
                         ticketCloseDate <- as.character(sapply(content(x,"parsed"), function(y) return(y$closed_at)))
                         #Get the size of the card-------------------------------------------------------------------------------
                         numericTagIT <- function(x) {#Due to the way SIZE labels were created, well need a function to translate
                           if (is.null(x)) {return(0)
                           } else if (x=="2hr") { return(2)
                           } else if (x=="4hr") { return(4)
                           } else if (x=="1day") { return(8)
                           } else if (x=="2day") {return(16)}
                         }
                         size      <- sapply(sapply(content(x,"parsed"),
                                            function(x) sapply(x$labels,
                                                               function(z) { if (z$color==sizeColor) {return(z$name)} })),
                                                                          function(u) sum(unlist(lapply(u,numericTagIT))))
                         #Create indicator for duplicate-------------------------------------------------------------------------
                         numericTagITDups <- function(x) {#Due to the way SIZE labels were created, well need a function to translate
                           if (is.null(x)) {return(0)
                           } else if (x=="duplicate") { return(1)}
                         }
                         dups      <- sapply(sapply(content(x,"parsed"),
                                                    function(x) sapply(x$labels,
                                                                       function(z) { if (z$name=="duplicate") {return(z$name)} })),
                                             function(u) sum(unlist(lapply(u,numericTagITDups))))
                         sprintNo  <- as.character(sapply(content(x,"parsed"), function(y) return(y$milestone$title)))
                         aa<-data.frame("ticketTitle"=titles,
                                    "assignedTicket"=user,
                                    "ticketOrder"=ticketOrder,
                                    "ticketState"=ticketState,
                                    "ticketOpenDate"=ticketOpenDate,
                                    "ticketCloseDate"=ticketCloseDate,
                                    "ticketSize"=size+1,
                                    "dups"=dups,
                                    "sprintNo"=sprintNo,stringsAsFactors=F)
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
        urlLast           <- gsub("^\\s+<|>$","",s[1])
        urlstr            <- unlist(strsplit(urlLast,"page="))
        urlPrefix         <- paste0(urlstr[1],'page=')
        totPages          <- urlstr[2]
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
      temp                <- GET(myURLS[i])
      temp                <- apiConsumer(temp)
      if (i==1) { output <- temp } else {output <- rbind(temp,output)}
      }
  } else {
    output                <- apiConsumer(GET(repoURL))
  }
  #Per Github requirements we do not keep duplicates:
  output                  <- output[which(output$dups!=1),]
  #Add begin/end date for each sprint for visualizations
  sprints                 <- as.character(unique(sort(output$sprintNo)))
  sprints                 <- sprints[which(sprints!="NULL")]
  sprintsBeginDT          <- sprintDeadlines[(1:length(sprints))]
  sprintsEndDT            <- sprintDeadlines[(1:length(sprints))+1]
  sprintDF                <- data.frame(sprints,sprintsBeginDT,sprintsEndDT,stringsAsFactors=F)
  colnames(sprintDF)      <- c("sprintNo","sprintBeginDT","sprintEndDT")
  finalll                 <- merge(x=output,y=sprintDF,by="sprintNo",all.x=T)
  
  indx <- which(sprintDeadlines<Sys.time())
  indexx <- c(indx,tail(indx,n=1)+1)
  sprintDeadlinesToday <- sprintDeadlines[indexx]
  #Between sprint dates we'll need metrics to display the backlog - not sure how this will work for open / close / open /close workflows
  for (j in 1:(length(sprintDeadlinesToday)-1)) {
    temp <- finalll[which( ((finalll$ticketOpenDate>=sprintDeadlinesToday[j]) & (finalll$ticketOpenDate<sprintDeadlinesToday[j+1])) |
                            ((finalll$ticketOpenDate<sprintDeadlinesToday[j+1]) & (finalll$ticketState=='open'))),]
    temp$backlog <- rep(paste0("Backlog ",j-1),nrow(temp))
    temp$backlogBeginDate <-  sprintDeadlinesToday[j] 
    temp$backlogEndDate <-  sprintDeadlinesToday[j+1] 
    if (j==1) {final <- temp} else { final <- rbind(final,temp)}
  }
  
  final <- final[order(final$backlog,-rank(final$ticketState)),]
  return(list(final=final,
              sprintDF=sprintDF,
              sprints=sprints))
}#-----------------------------------------------------------------------------------------------------------------------
#GET the data and put in a dataframe that can be used for backlog and other metrics
apiResults <- makeDataFrame(h)
#------------------------------------------------------------------------------------------------------------------------
#We want to be able to choose from any particular sprint, so we'll do that now
bySprint <- function(x) { #Assumes a data.frame from makeDataFrame is supplied
  uniqueTix                 <- apiResults$final[!(duplicated(apiResults$final$ticketOrder)),]
  for (j in (1:length(apiResults$sprints))) {
    currentSprint           <- uniqueTix[which(uniqueTix$sprintNo==apiResults$sprints[j]),]
    days                    <- seq(unique(currentSprint$sprintBeginDT),unique(currentSprint$sprintEndDT),by="days")
    for (z in 1:(length(days))) {
      day                   <- days[z]
      openSize              <- sum(currentSprint$ticketSize)
      closeSize             <- sum(currentSprint[which(as.Date(currentSprint$ticketCloseDate)==as.Date(day)),]$ticketSize)
      sprintNo              <- unique(currentSprint$sprintNo)
      out                   <- data.frame(day,openSize,closeSize,sprintNo)
      if (z==1) {final <- out} else {final <- rbind(final,out)}
      if (sum(as.Date(currentSprint$ticketCloseDate)==as.Date(day),na.rm=T)>0) {
        currentSprint         <- currentSprint[-which(as.Date(currentSprint$ticketCloseDate)==as.Date(day)),]
      }
    }
    if (j==1) {sprintTot <- final} else {sprintTot <- rbind(sprintTot,final)}  
  }

  return(sprintTot)
}
currentSprintSum <- bySprint(apiResults)

#The following needed for the dropdown menu
names(apiResults$sprints) <- apiResults$sprints
apiResults$sprints <- c("Backlog"="Backlog",apiResults$sprints)
#gut check
#GET("https://api.github.com/rate_limit")