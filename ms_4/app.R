

library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(readr)
library(janitor)
library(gt)
library(ggplot2)
library(scales)
library(plotly)
library(reshape2)


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
ui <- fluidPage(theme = shinytheme("journal"),
                navbarPage(
    "Final Project Name",
    tabPanel("Steel",
             fluidPage(
                 titlePanel("Plot"),
                     mainPanel(sidebarLayout(
                         sidebarPanel(
                             selectInput(
                                 "country_choice_1",
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
                                             'World'), 
                                 selected = "China"),  
                         radioButtons("global_axis_1",
                                                                     "Choose a scale on Y-axis:",
                                                                     
                                                                     # Two scales based on Log 10 or arithmetic.
                                                                     
                                                                     choices = c("Arithmetic","Logarithmic"), 
                                                                     
                                                                     # Set default to "Arithmetic".
                                                                     
                                                                     selected = "Arithmetic")),
                         mainPanel(plotOutput("line_plot_1")))
             ))),
    tabPanel("Aluminum",
             fluidPage(
                 titlePanel("Plot"),
                 mainPanel(sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "country_choice_2",
                             "Country",
                             choices = c('Argentina', 
                                         'Australia', 
                                         'Azerbaijan', 
                                         'Bahrain', 
                                         'Bosnia & Herzegovina', 
                                         'Brazil', 
                                         'Cameroon', 
                                         'Canada', 
                                         'China', 
                                         'Croatia', 
                                         'Egypt', 
                                         'France', 
                                         'Germany', 
                                         'Ghana', 
                                         'Greece', 
                                         'Iceland', 
                                         'India', 
                                         'Indonesia', 
                                         'Iran', 
                                         'Italy', 
                                         'Japan', 
                                         'Kazakhstan', 
                                         'Malaysia', 
                                         'Montenegro', 
                                         'Mozambique', 
                                         'Netherlands', 
                                         'New Zealand', 
                                         'Nigeria', 
                                         'Norway', 
                                         'Oman', 
                                         'Poland', 
                                         'Qatar', 
                                         'Romania', 
                                         'Russia', 
                                         'Saudi Arabia', 
                                         'Slovakia', 
                                         'Slovenia', 
                                         'South Africa', 
                                         'Spain', 
                                         'Sweden', 
                                         'Tajikistan', 
                                         'Turkey', 
                                         'USA', 
                                         'Ukraine', 
                                         'United Arab Emirates', 
                                         'United Kingdom', 
                                         'Venezuela'), 
                             selected = "China"),  
                         radioButtons("global_axis_2",
                                      "Choose a scale on Y-axis:",
                                      
                                      # Two scales based on Log 10 or arithmetic.
                                      
                                      choices = c("Arithmetic","Logarithmic"), 
                                      
                                      # Set default to "Arithmetic".
                                      
                                      selected = "Arithmetic")),
                     mainPanel(plotOutput("line_plot_2")))
                 ))),
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
               similar data about aluminum industry 3) think of model to use."))))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot_1 <- renderPlot({ if(input$global_axis_1 == "Logarithmic"){
                                              steel_yearly %>% 
                                                 filter(Country %in% c("United States", input$country_choice_1)) %>% 
                                                 ggplot(aes(x = Years, y = Production, color = Country)) +
                                                 geom_line() +
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = scales::comma_format(accuracy = 1)) +
                                                 labs(title = "Steel Production 2008-2018", 
                                                      x = "Year", 
                                                      y = "Production") +
                                                 theme_bw()}
        else{steel_yearly %>% 
                filter(Country %in% c("United States", input$country_choice_1)) %>% 
                ggplot(aes(x = Years, y = Production, color = Country)) +
                geom_line() +
                labs(title = "Steel Production, 2008-2018", 
                     x = "Year", 
                     y = "Production") +
                theme_bw()
            
            }
       })
    
    output$line_plot_2 <- renderPlot({ if(input$global_axis_2 == "Logarithmic"){
        aluminum_production %>% 
            filter(Country %in% c("USA", input$country_choice_2)) %>% 
            ggplot(aes(x = Years, y = Production, color = Country)) +
            geom_line() +
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = scales::comma_format(accuracy = 1)) +
            labs(title = "Steel Production 2008-2018", 
                 x = "Year", 
                 y = "Production") +
            theme_bw()}
        else{aluminum_production %>% 
                filter(Country %in% c("USA", input$country_choice_2)) %>% 
                ggplot(aes(x = Years, y = Production, color = Country)) +
                geom_line() +
                labs(title = "Aluminum Production, 2008-2018", 
                     x = "Year", 
                     y = "Production") +
                theme_bw()
            
        }
    })
}
# Run the application 
shinyApp(ui = ui, server = server)