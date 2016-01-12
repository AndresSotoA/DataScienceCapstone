library(shiny)
load("ngramCommonDF.RData")
source("NGramModel_Predict.R")


shinyServer(function(input,output,clientData,session)
    {
      observe({
        # predict next word
        sentence<- input$textIn
        
        # if last char is space, do next word predict.
        # otherwise, do the word completion for the last word
        lastChar<- substr(sentence,nchar(sentence),nchar(sentence))
        if (lastChar==" ")
        {
            nwList<- predictKN(df1c,df2c,df3c,df4c,sentence)
        }
        else
        {
            nwList<- predictOne(df1,sentence)
        }
        
        # handle zero prediction
        if (nwList[1]=="")
        {
            nwList<-c("No prediction"," ")
        }
       
        
        updateSelectInput(session,"selectNextWord",
                          choices = nwList,
                          selected= 1)
      })
})