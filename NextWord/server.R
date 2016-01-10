library(shiny)

shinyServer(function(input,output)
    {
        output$nextWord<- renderPrint(input$textIn)
    
})