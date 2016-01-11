library(shiny)
load("ngramCommonDF.RData")
source("NGramModel_Predict.R")


shinyServer(function(input,output,clientData,session)
    {
      observe({
        # predict next word
        sentence<- input$textIn
        nwList<- predictKN(df1c,df2c,df3c,df4c,sentence)
        
        updateSelectInput(session,"selectNextWord",
                          choices = nwList,
                          selected= 1)
      })
})