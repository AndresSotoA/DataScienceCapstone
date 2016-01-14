library(shiny)

# javascript codes to get keyboard input
jscode_up <- "
$(function(){ 
  $(document).keyup(function(e) {
    if (e.which ==32) {
        Shiny.onInputChange('numPress', 32);
    } 
  });
})
"

jscode_down <- "
$(function(){ 
  $(document).keydown(function(e) {
    if (e.which >=49 && e.which<=57) {
      Shiny.onInputChange('numPress', e.which-48);
    }
    else
    {
      Shiny.onInputChange('numPress', 0);
    }
  });
})
"


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
                           ),
               tags$script(HTML(jscode_down)),
               tags$script(HTML(jscode_up))
               )
    ),
    
    # test use
    fluidRow(
        column(width=12,
               textInput("testTxt","test output","test")
               )
    )
    
    
)

)