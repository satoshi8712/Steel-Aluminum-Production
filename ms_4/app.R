
library(shiny)
library(PPBDS.data)
library(tidyverse)
library(ggthemes)
library(readxl)

steel_monthly <- read_excel("data/exp-2020-11-06_08_00_25.xlsx")

steel_yearly <- read_excel("data/exp-2020-11-06_07_56_38.xlsx", 
                           skip = 1) %>% 
    pivot_longer(cols = -Country, 
                 names_to = "Years", 
                 values_to = "Production",
                 names_transform = list(Years = as.numeric)) %>% 
    filter(Years >= 2008) %>% 
    filter(!is.na(Production))
    

steel_export <- read_excel("data/exp-2020-11-06_08_02_22.xlsx")

aluminum_production <- read_excel("data/aluminum_production.xlsx",
                                  skip = 1) %>%
    select(-c("Sub-commodity", "2008.0", "2009.0", "2010.0", "2011.0", "2012.0",
              "2013.0", "2014.0", "2015.0", "2016.0", "2017.0", "2018.0")) %>% 
    rename("2008" = "...4", 
           "2009" = "...6", 
           "2010" = "...8", 
           "2011" = "...10", 
           "2012" = "...12", 
           "2013" = "...14", 
           "2014" = "...16", 
           "2015" = "...18", 
           "2016" = "...20", 
           "2017" = "...22", 
           "2018" = "...24", 
           "Country" = "\n\tCountry") %>% 
    pivot_longer(cols = -Country, 
                 names_to = "Years", 
                 values_to = "Production",
                 names_transform = list(Years = as.numeric)) %>% 
    filter(!is.na(Production))
    

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Name",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Plot"),
                     mainPanel(sidebarLayout(
                         sidebarPanel(
                             selectInput(
                                 "country_choice",
                                 "Country",
                                 choices = c('Albania', 
                                             'Algeria', 
                                             'Argentina', 
                                             'Australia', 
                                             'Austria', 
                                             'Azerbaijan', 
                                             'Bangladesh', 
                                             'Belarus', 
                                             'Belgium', 
                                             'Bosnia and Herzegovina', 
                                             'Brazil', 
                                             'Bulgaria', 
                                             'Canada', 
                                             'Chile', 
                                             'China', 
                                             'Colombia', 
                                             'Croatia', 
                                             'Cuba', 
                                             'Czech Republic',  
                                             'D.P.R. Korea', 
                                             'Denmark', 
                                             'Ecuador', 
                                             'Egypt',
                                             'El Salvador',
                                             'Finland', 
                                             'France', 
                                             'Germany', 
                                             'Ghana', 
                                             'Greece', 
                                             'Guatemala', 
                                             'Hungary', 
                                             'India', 
                                             'Indonesia', 
                                             'Iran', 
                                             'Israel', 
                                             'Italy', 
                                             'Japan', 
                                             'Jordan', 
                                             'Kazakhstan', 
                                             'Kenya', 
                                             'Latvia', 
                                             'Libya', 
                                             'Luxembourg', 
                                             'Macedonia', 
                                             'Malaysia', 
                                             'Mexico', 
                                             'Moldova', 
                                             'Montenegro', 
                                             'Morocco', 
                                             'Myanmar', 
                                             'Netherlands', 
                                             'New Zealand', 
                                             'Nigeria', 
                                             'Norway', 
                                             'Oman', 
                                             'Pakistan', 
                                             'Paraguay', 
                                             'Peru', 
                                             'Philippines', 
                                             'Poland', 
                                             'Portugal', 
                                             'Qatar', 
                                             'Romania', 
                                             'Russia', 
                                             'Saudi Arabia', 
                                             'Serbia', 
                                             'Singapore', 
                                             'Slovak Republic', 
                                             'Slovenia', 
                                             'South Africa', 
                                             'South Korea', 
                                             'Spain', 
                                             'Sri Lanka', 
                                             'Sweden', 
                                             'Switzerland', 
                                             'Syria', 
                                             'Taiwan, China', 
                                             'Thailand', 
                                             'Trinidad and Tobago', 
                                             'Tunisia', 
                                             'Turkey', 
                                             'Ukraine', 
                                             'United Arab Emirates', 
                                             'United Kingdom', 
                                             'United States', 
                                             'Uruguay', 
                                             'Uzbekistan', 
                                             'Venezuela', 
                                             'Vietnam', 
                                             'World'))), 
                         mainPanel(plotOutput("line_plot")))
             ))),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Link to Repository"),
             p("https://github.com/satoshi8712/milestone"),
             h3("Project Progress"),
             p("I changed the theme of the project and decided to focus on the 
               global trend in manifacturing industries. For this milestone, I 
               found datasets about steel industries, one of which is about 
               steel production from 2020 in each country. I cleaned the data,
               and made a plot about China's growth in steel production. Before
               next milestone, I plan to do three things: 1) make the plot more
               interative by giving an option of which country to display 2) add
               similar data about aluminum industry 3) think of model to use.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({ 
                                             steel_yearly %>% 
                                                 filter(Country == input$country_choice) %>% 
                                                 ggplot(aes(x = Years, y = Production)) +
                                                 geom_line() +
                                                 labs(title = "Trend in Steel Production, 2008-2018", 
                                                      x = "Year", 
                                                      y = "Production") +
                                                 theme_bw()
       }) 
}
# Run the application 
shinyApp(ui = ui, server = server)