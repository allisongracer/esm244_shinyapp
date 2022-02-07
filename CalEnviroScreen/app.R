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
                            plotOutput("cal_plot"))
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
                          plotOutput("cal_plot"))
                ) # end sidebarLayout
             ), # end tabpanel 2
    ) #end navbar
) # end ui

server <- function(input, output) {
  cal_reactive <- reactive({
    calenviroscreen4 %>%
      filter(california_county %in% input$pick_california_county)
  }) # end output$cal_plot
  
    output$cal_plot <- renderPlot(
    ggplot(data = cal_reactive(), aes(x = ozone, y = haz_waste)) +
    geom_point(aes(color = california_county))
    )
} # end output$cal_plot

shinyApp(ui = ui, server = server)



