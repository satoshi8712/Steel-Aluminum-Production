
library(shiny)
library(PPBDS.data)
library(tidyverse)
library(ggthemes)

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
    "Milestone 4 - Satoshi Yanaizu",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Plot"),
                     mainPanel(plotOutput("line_plot"))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Link to Repository"),
             p("https://github.com/satoshi8712/milestone_4"),
             h3("Project Progress"),
             p("I am thinking about doing something about foreign direct invest
               at the moment. I have collected two datasets, one  from OECD 
               website and the other from the World Bank. The OCED one, 
               DP_LIVE_17102020190158566, mesures 
               both inflow and outflow of country-specific FDI for each year, 
               while the WB dataset, API_BM.KLT.DINV.CD.WD_DS2_en_csv_v2_1497113, 
               is the list of country codes and the corresponding country names. 
               I am yet to figure out what is the best way to process data, my 
               apology for the delay. But I have here made a priliminary graoh 
               about country-specific value of FDI. My plan till next week is to 
               consolidate my idea as to what variables do I want to look for 
               and whether there are more appropriate sources of data -- I am 
               sure that, regarding this sort of economic data, there are 
               abundnace of dataset.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
       DP_LIVE_17102020190158566 %>%
            filter(TIME == 2018 | INDICATOR == "INWARD") %>%
            ggplot(aes(reorder(LOCATION, Value), Value)) +
            geom_col() +
            scale_x_discrete(guide = guide_axis(n.dodge=3)) + 
            theme_classic() +
            labs(title = "Inward Foreign Direct Investment in 2018", 
                 x = "Country", 
                 y = "Value of FDI")})
}
# Run the application 
shinyApp(ui = ui, server = server)