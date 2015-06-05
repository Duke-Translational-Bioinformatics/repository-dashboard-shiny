shinyServer(function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(data=apiResults, aes(x=sprintDate, fill=ticketState, y=ticketSize))+
      geom_bar(stat="identity")+
      scale_fill_manual(values=c("#001A57","#B5B5B5")) +
      theme_bw()+
      ylab("Sum of Sprint Tasks")+
      xlab("End of Sprint")
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- renderDataTable({
    cars
  }, options=list(pageLength=10))
})