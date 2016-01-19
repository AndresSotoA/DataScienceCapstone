library(shiny)

# javascript codes to get keyboard input
jscode_spacePress <- "
$(function(){ 
    $('#selectNextWord').keyup(function(e) {
        if (e.which ==32) {
            Shiny.onInputChange('numPress',-32);
            document.getElementById('textIn').focus();
        }
        Shiny.onInputChange('keyPress',-32);
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
        if ($('#textIn').is(':focus'))
           Shiny.onInputChange('keyPress',e.which);
        else 
           Shiny.onInputChange('keyPress',-1);
     });
})
"

# Shiny UI

shinyUI(fluidPage(
    # application title
    titlePanel(
        h1(strong("Next Word Prediction"), align= "center"
           , style="background-color: #00f; padding: 30px; color: #fff")
        ),
    
    # fluidRow 1: overview
    fluidRow(
        column(width= 12,
               h2(strong("Next word prediction SHINY application"),align="center",
                  style=""),
               h3("Author: JiaHsuan Lo", align="center"),
               hr()
               ) 
       
    ),
    fluidRow(
        column(width=12,
               p(HTML("<font size=5> This shiny application predicts the current and next word
                 as the user types. The prediction method is based on N-Gram
                 language model with Kneser-Ney smoothing algorithm.</font>"))
        )
    ),
    
    # fluidRow 2
    fluidRow(
        # input text area
        column(width=12,
               h4(strong("Please type in your sentences...")),
               tags$textarea(id="textIn", rows=10, cols=140, ""),
               tags$script(HTML(jscode_keyPress))
               )
    ),
    fluidRow(
        # output 
        column(width=4,
               selectInput("selectNextWord",size = 8,
                           selectize = FALSE,
                           choice= "no prediction",
                           label="Next Word Prediction"
               ),
               tags$script(HTML(jscode_spacePress))
        ),
        column(width=4,
               h4(strong("Press [TAB] key to go to the next word prediction listbox"),
                  style="color: #11f; padding-left:40 px",
                  align="center"),
               h4(img(src="downArrow.jpg", width="100 px"), align="center")
        ),
        column(width=4,
               h4(img(src="upArrow.jpg", width="100 px"), align="center"),
               h4(strong("[UP], [DOWN] or [NUMBER] keys to select the word. 
                         Press [SPACEBAR] key to return to text area"),
                  style="color: #11f; padding-left:40 px",
                  align="center")
        )
    )
    
)

)