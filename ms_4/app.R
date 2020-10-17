#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(PPBDS.data)
library(tidyverse)

DP_LIVE_17102020190158566 <- read_csv("data/DP_LIVE_17102020190158566.csv", 
                                      col_names = TRUE, 
                                      cols(
    LOCATION = col_character(),
    INDICATOR = col_character(),
    SUBJECT = col_character(),
    MEASURE = col_character(),
    FREQUENCY = col_character(),
    TIME = col_double(),
    Value = col_double(),
    `Flag Codes` = col_character()
))

API_BM.KLT.DINV.CD.WD_DS2_en_csv_v2_1497113 <- read_csv("data/API_BM.KLT.DINV.CD.WD_DS2_en_csv_v2_1497113.csv", 
                                                        col_names = TRUE, 
                                                        cols(
    `Data Source` = col_character(),
    `World Development Indicators` = col_character(),
    X3 = col_character()
))
    
# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model Title"),
                     mainPanel(plotOutput("line_plot"))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is Satoshi Yanaizu and I study Social Studies. 
             You can reach me at satoshi_yanaizu@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
       DP_LIVE_17102020190158566 %>%
            filter(TIME == 2012) %>%
            ggplot(aes(LOCATION, Value)) +
            geom_point()
    })
}
# Run the application 
shinyApp(ui = ui, server = server)