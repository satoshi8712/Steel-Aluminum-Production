
# Loading necessary packages. 

library(shiny)
library(shinythemes)
library(tidyverse)
library(readr)
library(gt)
library(ggplot2)
library(dplyr)
library(readxl)
library(rstanarm)
library(gtsummary)
library(broom.mixed)



# Cleaning data - for now, I have downloaded three datasets, about steel
# production, aluminum production, country data, respectively. I needed to use
# pivot_longer for both steel and aluminum dataset because they have years as a
# variable by default. 

steel_yearly <- read_excel("source/data/exp-2020-11-06_07_56_38.xlsx", 
                           skip = 1) %>% 
    pivot_longer(cols = -Country, 
                 names_to = "Years", 
                 values_to = "Production",
                 names_transform = list(Years = as.numeric)) %>% 
    filter(Years >= 2008) %>% 
    filter(!is.na(Production))
    
steel_2018 <- steel_yearly %>% 
    filter(Years == 2018) 

# Because the formatting of years is not proper in the original aluminum
# dataset, I need to manually rename them. 

aluminum_production <- read_excel("source/data/aluminum_production.xlsx",
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

# For country data, I need to rename some countries; this is due to the fact 
# that some countries have different names in steel dataset and this country 
# data set. For example, Myanmar is called Burma in this dataset originally. 

country_data <- read_csv("source/data/countries of the world.csv", 
                         col_types =  cols(
                             .default = col_character(),
                             Population = col_double(),
                             `Area (sq. mi.)` = col_double(),
                             `Infant mortality (per 1000 births)` = col_number(),
                             `GDP ($ per capita)` = col_double(),
                             `Literacy (%)` = col_number(),
                             `Other (%)` = col_number(),
                             Climate = col_number(),
                             Birthrate = col_number(),
                             Deathrate = col_number()
                         )) %>% 
    mutate(Country = str_replace(Country, pattern = "Bosnia & Herzegovina", replacement = "Bosnia and Herzegovina")) %>% 
    mutate(Country = str_replace(Country, pattern = "Korea, North", replacement = "D.P.R. Korea")) %>% 
    mutate(Country = str_replace(Country, pattern = "Burma", replacement = "Myanmar")) %>% 
    mutate(Country = str_replace(Country, pattern = "Slovakia", replacement = "Slovak Republic")) %>% 
    mutate(Country = str_replace(Country, pattern = "Korea, South", replacement = "South Korea")) %>% 
    mutate(Country = str_replace(Country, pattern = "Taiwan", replacement = "Taiwan, China")) %>%     
    mutate(Country = str_replace(Country, pattern = "Trinidad & Tobago", replacement = "Trinidad and Tobago")) 
    
               
    

# It is important to modify variables relating to industrial profiles, as in
# the original dataset, agriculture/industry/service are character variables,
# and include comma instead of period - so we need to use str_replace to have
# valid numeric variables. 

steel_country <- left_join(steel_2018, country_data, by = "Country") %>%  
    mutate(Agriculture = str_replace(Agriculture, pattern = "0,", replacement = "")) %>% 
    mutate(Industry = str_replace(Industry, pattern = "0,", replacement = "")) %>% 
    mutate(Service = str_replace(Service, pattern = "0,", replacement = "")) %>% 
    mutate(Agriculture = as.numeric(Agriculture)) %>% 
    mutate(Industry = as.numeric(Industry)) %>% 
    mutate(Service = as.numeric(Service)) %>% 
    mutate(Agriculture = Agriculture / 1000) %>% 
    mutate(Industry = Industry / 1000) %>% 
    mutate(Service = Service / 1000) %>% 
    mutate(GDP = `GDP ($ per capita)` * Population) %>% 
    mutate(Population = as.numeric(Population)) %>% 
    drop_na()




    

    
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("journal"),
                navbarPage(
    "Analysis of Global Steel and Aluminum Production",
    tabPanel("Steel",
             fluidPage(
                 titlePanel("Steel Production"),
                     mainPanel(sidebarLayout(
                         sidebarPanel(
                             
                             # Manually listing countries from steel dataset 
                             # for the sidebar
                             
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
             )), 
             br(), 
             h3("Background"), 
             p("Steel is the world's most important engineering and construction
             material. It is used in every aspect of our lives; in cars and
             construction products, refrigerators and washing machines, cargo
             ships and surgical scalpels. It can be recycled over and over again
             without loss of property.
"), 
             br(), 
             h3("About Plot"), 
             p("The plot above visualizes the annual steel production in
             countries around the world. Data on the steel production in the
             United States is added by default You can select a country of your
             interest from the side on the left. Because the volume of steel
             production varies vastly between countries (i.e., China’s steel
             production is enormous and accounts for more than half of global
             production), I added the option to choose Logarithmic scale on the
             Y axis in case a country’s production is significantly larger or
             smaller than the United States. You can choose this option with the
             button on the left. 
")),
    tabPanel("Aluminum",
             fluidPage(
                 titlePanel("Aluminum Production"),
                 mainPanel(sidebarLayout(
                     sidebarPanel(
                         
                         # Manually listing countries from aluminum data
                         # for the sidebar 
                         
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
                 )), 
             br(), 
             h3("Background"), 
             p("Aluminum is used in a huge variety of products including cans,
               foils, kitchen utensils, window frames, beer kegs and aeroplane
               parts. Aluminum is a good electrical conductor and is often used
               in electrical transmission lines. It is cheaper than copper and
               weight for weight is almost twice as good a conductor."), 
             br(), 
             h3("About Plot"), 
             p("The plot above visualizes the annual aluminum production in
               countries around the world. Data on the aluminum production in
               the United States is added by default You can select a country
               of your interest from the side on the left. Because the volume
               of aluminum production varies vastly between countries (i.e.,
               China’s aluminum production is enormously large compared to other
               major producers), I added the option to choose Logarithmic scale
               on the Y axis in case a country’s production is significantly
               larger or smaller than the United States. You can choose this
               option with the button on the left.")),
    tabPanel("Model", 
             titlePanel("Model"), 
             h3("Discussion of the model"), 
             p("The purpose in this section was to predict which factor
               correlates with a given country’s steel production. I used
               country data from CIA’s world factbook which includes
               country-specific information such as: region, population, area,
               population density, coastline, net migration, infant mortality,
               GDP, literacy, death rate, percentage of agriculture/industry/service 
               sector within overall economy. The general flow was to conceive
               multiple models with several of these variables to see which
               factor correlates with steel production."),
             br(), 
             p("The initial mode that I conceived was as follows:"),
             withMathJax(),
             helpText("$$ production_i = \\beta_{1}GDP_i + \\beta_{2}Population_i + \\beta_{3}Industry_i +  \\epsilon_i $$"),
             p("This model is inspired by my assumption that as a country gets richer, more
               populated, and more industrialized, the larger demand there is
               for steel. However, my statistical analysis showed that the
               median posterior increase in steel production alongside with an
               increase in a country’s GDP or population is minimal. Thus,
               both GDP and population have to be taken off from the model."),
             br(), 
             p("Next, I suspected there would be negative correlation between
               steel production and the percentage of agriculture/service sector
               in a given country’s GDP, because both agrarian and service-based
               economy would supposedly have smaller demand for domestic
               manufacturing. With this assumption, I conceived my next potential
               model, which is as follows:"),
             withMathJax(),
             helpText("$$ production_i = \\beta_{1}Agriculture_i + \\beta_{2}Industry_i + \\beta_{3}Service_i +  \\epsilon_i $$"),
             br(), 
             p("The regression table is as follows:"), 
             gt_output("table"),
             br(),
             p("However, although the median posterior increase in steel
               production alongside with a relative increase in a country’s
               agriculture or service sector is large, it is accompanied by even
               larger error (MAD_SD). The 95 percent confidence interval for the 
               industry variable is also wide and includes negative value. 
               Therefore, we cannot assume with full certainty that the size of 
               the overall industry sector correlates positively with steel 
               production. However, the result still suggests that some specific 
               sectors within the whole industry sector would have a positive 
               correlation with steel production. We would be able to have a full 
               picture if we can quantify trends in the size of specific sectors 
               (such as automobile and infrastrcture) within the whole industry 
               sectors. Altogether, two conclusions are deduced: 1) out of all 
               the available variables from CIA world factbook, only the size of 
               a country's industry sector (at least some portion of it) in a 
               country’s GDP could be interpreted as correlating posivively with 
               steel production 2) a country’s steel output depends more on the 
               country’s unique industrial profile and other factors that cannot 
               easily compared across nations."),
             withMathJax(),
             helpText("$$ production_i = \\beta_{1}Industry_i  +  \\epsilon_i $$"),
             br()
             ),
    tabPanel("About",
             br(),
             h4("About Me"),
             p("Hey there, my name is Satoshi. I am a sophomore at
                 Harvard concentrating in Social Studies.
                 Welcome  to my final project for Gov 50, a class in data
                 science."),
             h4("About the project"),
             p("This project aims to capture the recent trend in
               country-to-country steel and aluminum production. It was inspired
               my project in Harvard Undergraduate Foreign Policy Initiative (HUFPI)
               about the industrial overcapacity in steel and aluminum. Despite
               the rising global demands for these essential commodities, global
               prices of steel and aluminum have stagnated since 2010s, allegedly
               due to overproduction in China escalating competitions and pushing
               steelmakers and aluminum producers in other countries out of business.
               In investigating the extent of overproduction and global trends in
               the volume of steel/aluminum production, I thought a platform like
               this is immensely useful in comparing countries’ industrial productions."),
             p("You can find the link to my Github right here: https://github.com/satoshi8712"),
             h4("Data Source"),
             p("The data on steel production was published by the World Steel 
             Association. The data on aluminum production was published by
               British Geological Survey. Finally the data on country's properties
               reflects content from CIA's World Factbook."),
             p("Steel Data: https://www.worldsteel.org/steel-by-topic/statistics/steel-data-viewer/P1_crude_steel_total"),
             p("Aluminum Data: https://www2.bgs.ac.uk/mineralsUK/statistics/worldStatistics.html"),
             p("Country Data: https://www.kaggle.com/fernandol/countries-of-the-world"))))



server <- function(input, output) {
    output$line_plot_1 <- renderPlot({ if(input$global_axis_1 == "Logarithmic"){
    steel_yearly %>% 
            
# Including the United States as a default point of comparison
            
    filter(Country %in% c("United States", input$country_choice_1)) %>% 
    ggplot(aes(x = Years, y = Production, color = Country)) +
    geom_line() +
            
# Adding functions from scales package for log scale
            
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                           labels = scales::comma_format(accuracy = 1)) +
            
# Because the label of x axis had some problems (it somehow included numbers
# such as 2007.5, I manually set them, also for the aluminum plot)
            
    scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018),
                       labels = c("2008", "2010", "2012", "2014", "2016", "2018")) +
    labs(title = "Steel Production 2008-2018", 
         x = "Year", 
         y = "Production") +
    theme_bw()}
        else{steel_yearly %>% 
                filter(Country %in% c("United States", input$country_choice_1)) %>% 
                ggplot(aes(x = Years, y = Production, color = Country)) +
                geom_line() +
                scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018),
                                   labels = c("2008", "2010", "2012", "2014", "2016", "2018")) +
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
            
            # Adding functions from scales package for log scale
            
            scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                          labels = scales::comma_format(accuracy = 1)) +
            scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018),
                               labels = c("2008", "2010", "2012", "2014", "2016", "2018")) +
            labs(title = "Steel Production 2008-2018", 
                 x = "Year", 
                 y = "Production") +
            theme_bw()}
        else{aluminum_production %>% 
                filter(Country %in% c("USA", input$country_choice_2)) %>% 
                ggplot(aes(x = Years, y = Production, color = Country)) +
                geom_line() +
                scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018),
                                   labels = c("2008", "2010", "2012", "2014", "2016", "2018")) +
                labs(title = "Aluminum Production, 2008-2018", 
                     x = "Year", 
                     y = "Production") +
                theme_bw()}})
    
    output$table <- render_gt({
        
        model <- stan_glm(formula = Production ~ Agriculture + Industry + Service, 
                          data = steel_country, 
                          refresh = 0) 
        
        tbl_regression(model, intercept = TRUE) %>%
            as_gt() %>% 
            tab_header(title = "Regression of Country-by-country Steel Production", 
                       subtitle = "How Industrial Compositions are Correlated with Steel Production?")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)