library(shiny)

# Next Word prediction based on the input
shinyUI(fluidPage(theme = "cyborg.css",
        fluidRow(
            column(12, align="center",
                             tags$img(src='pic12.jpg', height="15%", width="100%") 
      )  
     ),
    
    
    fluidRow(
      column(12, align="center",
             h4("Made for the John Hopkins Coursera Data Science Capstone Project", style="color:gray") 
      )  
    ),
    
    hr(),
    
    fluidRow(
        column(5,
               h5("This application predicts the next word in a phrase as the users types it. The supervised
                  learning algorithm was trained using Ngrams (n=1,2,3,4),which were extracted from about 
                  1 GB of text data and modified Kneser-Ney smoothing was performed."),
               h4("Please start typing words to see predictions.")
               ),

        column(7,offset = 0,
               
               fluidRow(column(12,offset = 0,align="center",
                               textInput(inputId="input",label=NULL , value = "", width = "75%" , placeholder = "type here..."),
                               tags$style(type='text/css', "#input {  height: 50px; font-size: 25px;}")
                               
                   )
               
               ),
               fluidRow(
                   
                   column(12,offset = 0,align="center",
                          textOutput( 'next_word1'),
                          tags$style(type='text/css', "#next_word1 {color:rgb(102,102,0); height: 50px; font-size: 35px; b;}")
                          
                   )
               ),
               fluidRow(
                   
                   column(3,offset = 0,align="center",
                          textOutput( 'next_word2'),
                          tags$style(type='text/css', "#next_word2 { color:rgb(102,102,0); font-size: 25px;}")
                          
                   ),
                   column(3,offset = 0,align="center",
                          textOutput( 'next_word3'),
                          tags$style(type='text/css', "#next_word3 {color:rgb(102,102,0); font-size: 25px;}")
                          
                   ),
                   column(3,offset = 0,align="center",
                          textOutput( 'next_word4'),
                          tags$style(type='text/css', "#next_word4 {color:rgb(102,102,0); font-size: 25px;}")
                          
                   ),
                   column(3,offset = 0,align="center",
                          textOutput( 'next_word5'),
                          tags$style(type='text/css', "#next_word5 {color:rgb(102,102,0); font-size: 25px;}")
                          
                   )
               )
        )
        
        
    ),
    
    hr()
    
    
))
