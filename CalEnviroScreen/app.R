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
    ), # end tabpanel 1
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
    ), # end tabpanel 2
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



