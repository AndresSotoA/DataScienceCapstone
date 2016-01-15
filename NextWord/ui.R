library(shiny)

# javascript codes to get keyboard input
jscode_spacePress <- "
$(function(){ 
    $('#selectNextWord').keyup(function(e) {
        if (e.which ==32) {
            Shiny.onInputChange('numPress',-32);
            document.getElementById('textIn').focus();
        } 
     });

    $('#selectNextWord').keydown(function(e) {
        if (e.which ==32) {
            Shiny.onInputChange('numPress',32);
        }
    });
})
"


jscode_keyPress <- "
$(function(){ 
    $('#textIn').keyup(function(e) {
           Shiny.onInputChange('keyPress',e.which);
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
               tags$textarea(id="textIn", rows=10, cols=140, ""),
               tags$script(HTML(jscode_keyPress))
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
               tags$script(HTML(jscode_spacePress))
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