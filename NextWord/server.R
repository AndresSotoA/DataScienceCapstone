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
        if (length(nwList)==0)
        {
            nwListOut<-c("No prediction")
        }
        else
        {
            # attach index numbers
            nwListOut<- paste(1:length(nwList),"  ",nwList)
        }
        
        # keyboard input
        if (length(input$numPress)>0)
        {
            if (input$numPress>0) # number key is pressed
            {
                iWord<- input$numPress
                nextWord<- nwList[iWord]
                if (lastChar==" ") # next word prediction mode
                {
                    fullstr<- paste0(sentence,nextWord)
                }
                else # autocomplete mode
                {
                    fullstr<- sentence
                }
                updateTextInput(session,"textIn", value = fullstr)
                
            }
            # update key pressed indicator
            updateTextInput(session,"testTxt",value = sprintf('%.0f',input$numPress))
            
        }
        
        # print out next words on selectInput
        updateSelectInput(session,"selectNextWord",
                          choices = nwListOut,
                          selected= 3)
      })
})