library(shiny)
library(tidyverse)
library(bslib)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("cerulean"),
navbarPage(
    "CalEnviroScreen",
    tabPanel("Widget 1",
             sidebarLayout(
                 sidebarPanel("Widgets go here"), # end sidebarPanel
                 mainPanel("OUTPUT GOES HERE",
                            plotOutput(""))
                 ) # end sidebarlayout 
             ), # end tabpanel 
    tabPanel("Widget 2")
    ) #end navbar
) # end ui



