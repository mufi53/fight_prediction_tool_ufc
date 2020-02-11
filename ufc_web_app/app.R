#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(flexdashboard)
library(sqldf)
library(caTools)
library(e1071)
library(randomForest)
library(neuralnet)
library(sqldf)

source("Model_Deployment.R")
setwd("/Users/mufaddalsadiq/pba_coursework/ufc_web_app")


# read the player data
fighter_stats<-read.csv("master_fighter_recent_stats.csv")
image_path<- sqldf("select fighter_img_path from fighter_stats where Fighter_ID =2")[1][1]


player_info_red<-sample(setNames(fighter_stats$Fighter_ID , fighter_stats$Fighter))
player_info_blue<-sample(setNames(fighter_stats$Fighter_ID , fighter_stats$Fighter))
#print(player_info_red<-sample(setNames(fighter_stats$Fighter_ID , fighter_stats$Fighter)))


# Define UI for application 
ui <- fluidPage(
 
    tags$style(
        "#main-heading{margin-bottom: 30px; font-size:50px}
         #gauge_row{margin-top: 80px;}
         #submit{color: #fff; background-color: #337ab7; border-color: #2e6da4 ; font-size:30px ; margin-top:100px}
         #winner-label{font-size:50px}
         #winner_name{color:red}"
    ),

    titlePanel(
        h1(id="main-heading","UFC Fight Predictor", align = "center" ) ,
    ),
    
    
    
    # top fluid row 
    fluidRow(id='main_row',
        
        column(4,
               wellPanel( style = "font-size:20px; text-align = center",
               selectInput("red_fighter", "Red fighter:",
                          player_info_red
                          ),
       ) ),
        
        column(4,
             h3(id="winner-label","VS", align = "center" ) ,
            
             
              
        ),
        
    
        column(4,
               wellPanel( style = "font-size:20px; text-align = center",
               selectInput("blue_fighter", "Blue fighter:",
                           player_info_blue),
        )),
        
        
        
        
       
    ) , 
    
    # image row 
    fluidRow(
      
      
      column(4 ,  align="center" ,  imageOutput("redImage" , height = "250px")),
      column(4  , align="center", h1(textOutput("winner_name")), h2(textOutput("wins")), actionButton("submit" , label = "Predict", icon = NULL, width = "50%" )),
      
      
      column(4 ,   align="center", imageOutput("blueImage" , height = "250px")),
      
      
    ),
    
    # predict button row 
    fluidRow( id='gauge_row',
        
        
        column(4 , gaugeOutput('prob_meter_red', width = "100%", height = "400px")),
        column(4 ,
                
              
               
               ),
        
        
        column(4 , gaugeOutput('prob_meter_blue', width = "100%", height = "400px") , ),
        
        
    )
    
    
    
    

    # Sidebar with a slider input for number of bins 
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # observe event for updating the reactiveValues
    observeEvent(input$submit,
                 {
                     blue_fighter_id <- input$blue_fighter
                     red_fighter_id <- input$red_fighter
                     fighter_details <- get_fighter_details(blue_fighter_id,red_fighter_id)
                     prob_blue_fighter = fighter_details[[2]][1]
                     prob_red_fighter = fighter_details[[3]][1]
                     winner_name = fighter_details[[1]][1]
                     
                    
                     #render name
                     output$winner_name = renderText({
                       winner_name
                       
                     })
                     #render name
                     output$wins = renderText({
                        'WINS'
                       
                     })
                     
                     #render gauge
                     output$prob_meter_red<- renderGauge(gauge(prob_red_fighter, min = 0, max = 100, symbol = '%', gaugeSectors(
                       success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                     )))
                     
                     #render gauge
                     output$prob_meter_blue<- renderGauge(gauge(prob_blue_fighter, min = 0, max = 100, symbol = '%', gaugeSectors(
                       success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                     )))
                     
                     print(fighter_details)
                     
                     
                     #print(red_fighter_id)
                     #print(blue_fighter_id)
                 })
  
  
   
    
    # reactiveValues
    text_reactive <- reactiveValues(
        text = "No text has been submitted yet."
        
    )
    
    # event listener for select input 
    observeEvent(input$red_fighter, {
      red_fighter_id <- input$red_fighter
      image_src <-as.character(sqldf(paste("select fighter_img_path from fighter_stats where Fighter_ID =",red_fighter_id))$fighter_img_path[1])
      output$redImage <- renderImage({   #This is where the image is set 
        
        list(src=image_src, width = 300 , height= 350)
        
        
      }, deleteFile = FALSE)
      
      #print(red_fighter_id)
    })
    
    
    # event listener for select input 
    observeEvent(input$blue_fighter, {
      blue_fighter_id <- input$blue_fighter
      
      output$blueImage <- renderImage({   #This is where the image is set 
        
        list(src =as.character(sqldf(paste("select fighter_img_path from fighter_stats where Fighter_ID =",blue_fighter_id))[1][1]$fighter_img_path[1]), width = 300 , height=350)
        
        
      }, deleteFile = FALSE)
      
      
      #print(blue_fighter_id)
    })
    
  
    
    
   
    
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)



