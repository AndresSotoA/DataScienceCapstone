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
        
        if (length(input$keyPress)>0)
        {
            if (input$keyPress>0)
                # print out next words on selectInput
                updateSelectInput(session,"selectNextWord",
                                  choices = nwListOut)
        }
        
        # keyboard input
        if (length(input$numPress)>0)
        {
            selectedItem<- input$selectNextWord
            
            # detect the unlock event: spacebar transition from down to up
            if ((input$numPress==32))
                lock<<-0
            
            if ((lock==0) && (input$numPress==-32))
            {
                iWord<- (nwListOut==selectedItem)
                nextWord<- nwList[iWord]
                if (lastChar==" ") # next word prediction mode
                {
                    fullstr<- paste0(sentence,nextWord)
                }
                else # autocomplete mode
                {
                    words<- strsplit(sentence,split=" ")[[1]]
                    newWords<- c(words[1:(length(words)-1)], nextWord)
                    newSentence<- paste(newWords, collapse = " ")
                    fullstr<- newSentence
                }
                updateTextInput(session,"textIn", value = fullstr)
                # reset words on selectInput
                updateSelectInput(session,"selectNextWord",
                                  choices = c("No prediction"))
                lock<<- 1
            }
            
        }
        
      })
})