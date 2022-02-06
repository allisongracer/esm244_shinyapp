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
    tabPanel("Widget 1 - Map",
             sidebarLayout(
                 sidebarPanel("Widgets go here"), # end sidebarPanel
                  checkboxGroupInput(inputId = "pick_parameter",
                                     label = "Choose parameter:",
                                     choices = unique(calenviroscreen4$california_county)
                                     ) # end checkboxGroupInput
                 ), # end sidebarLayout
             
                 mainPanel("OUTPUT GOES HERE",
                            plotOutput("")) # end mainPanel
             ), # end tabpanel 
    tabPanel("Widget 2 - Bar Graph"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
      ), #end sidebarPanel
      mainPanel(plotOutput("distPlot")
                ) # end mainPanel
    ) # end sidebarLayout
    ) #end navbar
) # end ui

server <- function(input, output) {
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgreen', border = 'white')
  })
}

shinyApp(ui = ui, server = server)



