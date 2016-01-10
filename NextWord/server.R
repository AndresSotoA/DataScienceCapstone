library(shiny)
load("ngramDF_pct.RData")
source("NGramModel_Predict.R")


shinyServer(function(input,output,session)
    {
        # predict next word
        sentence<- input$textIn
        nw<- predictKN(df)
        
    updateSelectInput(session,"selectNextWord",
                          choices = c("try","this"),
                          selected= 1)
})