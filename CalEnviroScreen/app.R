library(shiny)
library(tidyverse)
library(bslib)
library(shinythemes)
library(here)
library(readxl)
library(janitor)

# read in data
calenviroscreen4 <- read_excel(here("Data", "calenviroscreen40resultsdatadictionary_f_2021.xlsx")) %>%
  drop_na() %>%
  clean_names()

# custom theme
shiny_theme <- bs_theme(bootswatch = "minty")

# start shiny app
ui <- fluidPage(theme = shiny_theme,
navbarPage("CalEnviroScreen",
    tabPanel("Home", 
    sidebarLayout(
    sidebarPanel(),
      mainPanel(
        h1("Project Description"),
        p("CalEnviroScreen was designed to assist CalEPA with carrying out its environmental justice mission to ensure the fair treatment of all Californians, including minority and low-income communities." 
          ),
        br(), 
        p("CalEnviroScreen uses environmental, health, and socioeconomic information to produce scores for every census tract in the state. An area with a high score is one that experiences a much higher pollution burden than areas with low scores. CalEnviroScreen ranks communities based on data that are available from state and federal government sources."
          ),
        br(),
        p("The purpose of this Shiny App is to explore data from CalEnviroScreen 2.0 (2014),  3.0 (2018), and 4.0 (2021) to better visualize how pollution-based parameters affect demographics in California, and to analyze how these parameters have or have not changed over time." 
          ), 
      ), #end mainpanel
    ) # end sidebarlayout
), #end tabPanel 1
    tabPanel("Pollution Map",
             sidebarLayout(
                sidebarPanel(
                checkboxGroupInput(inputId = "pick_california_county",
                                    label = "Choose California County:",
                                    choices = unique(calenviroscreen4$california_county)
                 ) # end checkboxGroupInput
               ), #end sidebarPanel                 
                  mainPanel("Map of California",
                            plotOutput("cal_plot1"))
             ) # end sidebarLayout
    ), # end tabpanel 2
    tabPanel("Pollution Burden Per Capita",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(inputId = "pick_california_county",
                                    label = "Choose California County:",
                                    choices = unique(calenviroscreen4$california_county)
                 ) # end checkboxGroupInput
               ), #end sidebarPanel                 
               mainPanel("Pollution Burden Per Capita",
                         plotOutput("cal_plot2"))
             ) # end sidebarLayout
    ), # end tabpanel 3
    ) #end navbar
) # end ui

server <- function(input, output) {
  cal_reactive1 <- reactive({
    calenviroscreen4 %>%
      filter(california_county %in% input$pick_california_county)
  }) # end output$cal_plot 1
  
  cal_reactive2 <- reactive({
    calenviroscreen4 %>%
      filter(california_county %in% input$pick_california_county)
  }) # end output$cal_plot 2
  
    output$cal_plot1 <- renderPlot(
    ggplot(data = cal_reactive1(), aes(x = ozone, y = haz_waste)) +
    geom_point(aes(color = california_county))
    ) # end output$cal_plot1

output$cal_plot2 <- renderPlot(
  ggplot(data = cal_reactive2(), aes(x = ozone, y = haz_waste)) +
    geom_point(aes(color = california_county))
)
} # end output$cal_plot2


shinyApp(ui = ui, server = server)



