#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Career Triangle"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "text1",
                        "Criteria 1 - Left", 
                        value = "Money"),
            sliderInput("Criteria1",
                        label = element_blank(),
                        min = 0,
                        max = 10,
                        value = 3),
            textInput(inputId = "text2",
                      "Criteria 2 - Top",
                      value = "Job Security"),
            sliderInput("Criteria2",
                        label = element_blank(),
                        min = 0,
                        max = 10,
                        value = 5),
            textInput(inputId = "text3",
                      "Criteria 3 - Right",
                      value = "Passion"),
            sliderInput("Criteria3",
                        label = element_blank(),
                        min = 0,
                        max = 10,
                        value = 10)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterplot")
        )
    )
)

# Define server logic required to make a triangle
server <- function(input, output) {

    output$scatterplot <- renderPlot({
      req(input$Criteria1)
      req(input$Criteria2)
      req(input$Criteria3)
      
      x1 <- c(-input$Criteria1*cos(pi/6), 0, input$Criteria3*cos(pi/6))
      y1 <- c(-input$Criteria1*sin(pi/6), input$Criteria2, -input$Criteria3*sin(pi/6))
      data <- as.data.frame(cbind(x1,y1) ) 
      
      distance <- numeric(3)
      a <- distance[1] <- sqrt((x1[1]-x1[2])^2+(y1[1]-y1[2])^2)
      b <- distance[2] <- sqrt((x1[2]-x1[3])^2+(y1[2]-y1[3])^2)
      c <- distance[3] <- sqrt((x1[3]-x1[1])^2+(y1[3]-y1[1])^2)
      s <- (a+b+c)/2
      area <- sqrt(s*(s-a)*(s-b)*(s-c))
    
      
      ggplot(data, mapping = aes(x = x1, y = y1)) +
        geom_point() +
        theme_minimal() + 
        geom_segment(aes(x = x1[1], y = y1[1], xend = x1[2], yend = y1[2])) +
        geom_segment(aes(x = x1[1], y = y1[1], xend = x1[3], yend = y1[3])) +
        geom_segment(aes(x = x1[3], y = y1[3], xend = x1[2], yend = y1[2])) +
        geom_segment(aes(x = 0, y = 0, xend = x1[1], yend = y1[1]), color = "red") +
        geom_segment(aes(x = 0, y = 0, xend = x1[2], yend = y1[2]), color = "red") +
        geom_segment(aes(x = 0, y = 0, xend = x1[3], yend = y1[3]), color = "red") +
        geom_label(aes(label=str_wrap(input$Criteria1,12), x=x1[1]/2, y = y1[1]/2)) +
        geom_label(aes(label=str_wrap(input$Criteria2,12), x=x1[2]/2, y = y1[2]/2)) +
        geom_label(aes(label=str_wrap(input$Criteria3,12), x=x1[3]/2, y = y1[3]/2)) +
        ggtitle(sprintf("Area = %s", area)) +
        theme(aspect.ratio = 1)
   
        
        })
}


### now add two more triangles (just scroll down to see next ones and each are different color)


# Run the application 
shinyApp(ui = ui, server = server)
