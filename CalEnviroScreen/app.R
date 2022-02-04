library(shiny)
library(tidyverse)
library(bslib)
library(shinythemes)

# custom theme
shiny_theme <- bs_theme(bootswatch = "minty")

# start shiny app
ui <- fluidPage(theme = shiny_theme,
navbarPage("CalEnviroScreen",
    tabPanel("Widget 1",
             sidebarLayout(
                 sidebarPanel("Widgets go here"), # end sidebarPanel
                 mainPanel("OUTPUT GOES HERE",
                            plotOutput("")) # end mainPanel
                 ) # end sidebarlayout 
             ), # end tabpanel 
    tabPanel("Widget 2")
    ) #end navbar
) # end ui

server <- function(input, output) {}

shinyApp(ui = ui, server = server)



