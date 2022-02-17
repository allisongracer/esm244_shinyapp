library(shiny)
library(tidyverse)
library(bslib)
library(shinythemes)
library(here)
library(readxl)
library(janitor)
library(sf)
library(tmap)

# read in data
calenviroscreen4 <- read_excel(here("Data", "calenviroscreen40resultsdatadictionary_f_2021.xlsx")) %>%
  drop_na() %>%
  clean_names()

# custom theme
shiny_theme <- bs_theme(bootswatch = "minty")

# start shiny app
ui <- fluidPage(theme = shiny_theme,
navbarPage("CalEnviroScreen",
    tabPanel("Project Overview",
      mainPanel(
        fluidRow(
        h1("Project Description"),
        p("CalEnviroScreen was designed to assist CalEPA with carrying out its environmental justice mission to ensure the fair treatment of all Californians, including minority and low-income communities." 
          ),
        br(), 
        p("CalEnviroScreen uses environmental, health, and socioeconomic information to produce scores for every census tract in the state. An area with a high score is one that experiences a much higher pollution burden than areas with low scores. CalEnviroScreen ranks communities based on data that are available from state and federal government sources."
          ),
        br(),
        p("The purpose of this Shiny App is to explore data from CalEnviroScreen 2.0 (2014),  3.0 (2018), and 4.0 (2021) to better visualize how pollution-based parameters affect demographics in California, and to analyze how these parameters have or have not changed over time." 
          ),
        ) # end fluidRow
      ), #end mainpanel
    ), # end tabPanel 1
    tabPanel("Pollution Map", # start panel 2
             mainPanel( # start main panel 2
               tmapOutput("tmap_ej") # ERROR
               ) # end main panel 2
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

# create server object

server <- function(input, output) {
  
# data for ej map
  
pollution_map <- calenviroscreen4 %>%
  select(total_population:ces_4_0_percentile_range, haz_waste, haz_waste_pctl, pesticides, pesticides_pctl, tox_release, tox_release_pctl, pollution_burden, pollution_burden_score, pollution_burden_pctl) %>%
  group_by(california_county)

pollution_map_sf <- pollution_map %>%
  st_as_sf(coords = c("longitude", "latitude"))

# end data for ej map

# output for ej map

output$tmap_ej <- renderTmap({
  tm_shape(pollution_map_sf) +
    tm_dots()
})
  
# output for map filters
  cal_reactive1 <- reactive({
    calenviroscreen4 %>%
      filter(california_county %in% input$pick_california_county)
  }) # end output$cal_plot 1
  
  
# output for wigit 2
  cal_reactive2 <- reactive({
    calenviroscreen4 %>%
      filter(california_county %in% input$pick_california_county)
  }) # end output$cal_plot 2
  
# graph for wigit 2
    output$cal_plot2 <- renderPlot(
    ggplot(data = cal_reactive2(), aes(x = ozone, y = haz_waste)) +
    geom_point(aes(color = california_county))
    ) # end output$cal_plot1
} # end server


shinyApp(ui = ui, server = server)



