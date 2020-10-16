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
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
       ggplot(PPBDS.data::sps, aes(age, education)) +
            geom_point()
    })
}
# Run the application 
shinyApp(ui = ui, server = server)