library(shiny)
library(roll)
library(dplyr)
library(grid)
library(ggplot2)

source("data/Contact Consistency.R")

# Define UI for application that draws a histogram
ui <- navbarPage("Contact Consistency",
                 
                 # Application title
                 tabPanel("Rolling Charts",
                          
                          
                          # Show a plot of the generated distribution
                          mainPanel(selectInput("hitter",
                                                "Select Hitter:",
                                                sort(unique(as.character(all_play_by_play$player_name)))),
                                    sliderInput("bips",
                                                "Number Of Balls in Play",
                                                min = 0, 
                                                max = 100, value = 30),
                                    plotOutput("rollplot")
                          )
                 ),
                 tabPanel("Leaders",
                          
                          DT::dataTableOutput('leaders')
                          ),
                 tabPanel("wOBACon",
                          
                          plotOutput("wOBACon")
                          
                 )
                 )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$rollplot <- renderPlot({
    contact_consistency_chart(input$hitter,input$bips)
  })
  
  output$leaders <- DT::renderDataTable(DT::datatable({
    data <- summary
  },
  rownames= FALSE))
  
  output$wOBACon <- renderPlot({
    plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)