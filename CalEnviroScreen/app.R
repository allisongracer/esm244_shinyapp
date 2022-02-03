library(shiny)
library(tidyverse)
library(bslib)


ui <- fluidPage(theme = bs_theme(versiion = 5),
navbarPage(
    "CalEnviroScreen",
    tabPanel("Widget 1",
             sidebarLayout(
                 sidebarPanel("Widgets go here"), # end sidebarPanel
                 maainPanel("OUTPUT GOES HERE",
                            plotOutput(""))
                 ) # end sidebarlayout 
             ) # end tabpanel 
    tabPanel("Widget 2"),
    ) #end navbar
) # end ui
