---
title       : Next Word Prediction Tool
subtitle    : A R Shiny application that predicts the next word   
author      : JiaHsuan Lo   
job         : Coursera Data Science Student
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Introduction

Automatic word completion and next word prediciton are common features in the user input devices that can help users input texts easier, faster, and more accurately. 

This application is designed to provide a convenient and efficient typing tool to user. The underlying algorithm was based on N-gram language model and Kneser-Ney smoothing algorithm. Some backgrouhd information can be found at the following links:

* [Natrual Language Processing Wikipedia](https://en.wikipedia.org/wiki/Natural_language_processing)
* [N-Gram: Speech and Language Processing Course Material by Daniel Jurafsky & James H. Martin at Stanford University](https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf)

--- .class #id

## Data used to Build the Language Model

* The data is from a corpus called HC Corpora [www.corpora.heliohost.org](www.corpora.heliohost.org), and the data files are obtained from the [Coursera website](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).

* The sources of texts include blogs, twitter, and news. Only US english files were used. The lines and words counts are summarized in the following table:

File Name         |   Number of Lines          |    Number of Words  
------------------|----------------------------|-------------------------
en_US.blogs.txt   |  899288 	               |    38315977
en_US.twitter.txt |  167155                    |	2191565
en_US.news.txt    |  1010242 	               |    35627434 

* To make the app small and fast for general PC and mobile device, following sampling steps were used when building the model:
    + <font size="4">6% of the texts from each files were strafified-randomly sampled. Then 1- to 4-gram frequency table were created.</font> 
    + <font size="4">From 1-gram data, 5000 most common words were identified based on the frequency.</font>
    + <font size="4">Only those n-gram data (n=1...4) that contains the 5000 most common words were preserved.</font>

--- .class #id 

## Algorithm Behind the Scene

* Automatic word completion:
    + 1-gram model was used to predict more probable word.
    
* Next word prediction: A mixed smoothing algorithm was used:
    + Kneser-Ney Recursive formula was applied to 1- to 4-gram language models to calculate the probabilities of the next word candidates.
    + 2- and 3-gram models were also used to estimate the probabilities of the next word candidates.
    + An interpolation scheme was used to calculate the final probablities.
    + The most probable word was then predicted.




--- .class #id

## Application Tutorial



---

## Where to Play With the App?
