library(shiny)

# Shiny UI

shinyUI(fluidPage(
    # application title
    titlePanel(
        h1("Next Word Prediction", align= "center")
        ),
    
    # fluidRow 1: overview
    fluidRow(
        column(width= 12,
               h2("Next word prediction SHINY application"),
               br(),
               p("Author: JiaHsuan Lo"),
               br(),
               p("This shiny application predicts the current and next word
                 as the user types. The prediction method is based on N-Gram
                 language model with Kneser-Ney smoothing algorithm.")
               ) 
    ),
    # fluidRow 2: input
    fluidRow(
        column(width=4,
               textInput("txtIn",label = "Please type in your sentences...",
                         value="type here")
               )
    )
    
)

)