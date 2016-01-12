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
                 language model with Kneser-Ney smoothing algorithm."),
               hr()
               ) 
       
    ),
    # fluidRow 2: input
    fluidRow(
        column(width=12,
               h4("Please type in your sentences..."),
               tags$textarea(id="textIn", rows=10, cols=140, "")
               )
    ),
    
    # output row
    fluidRow(
        column(width=12,
               hr(),
               selectInput("selectNextWord",size = 8,
                           selectize = FALSE,
                           choice= "no prediction",
                           label = "Next Word Prediction"
                           )
               )
    )
    
)

)