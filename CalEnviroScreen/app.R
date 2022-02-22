##### packages #####


library(shiny)
library(tidyverse)
library(bslib)
library(shinythemes)
library(here)
library(readxl)
library(janitor)
library(sf)
library(tmap)
library(viridis)
library


##### data and wrangling #####

# read in data for tab 2
suppressWarnings({
calenviroscreen4 <- read_xlsx(here("Data", "calenviroscreen40resultsdatadictionary_f_2021.xlsx")) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  na_if(0.000) %>%
  drop_na() %>%
  clean_names()
})

# end data for tab 2

pollution_map <- calenviroscreen4 %>%
  select(total_population:ces_4_0_percentile_range, haz_waste, pesticides, tox_release, pollution_burden, pollution_burden_score, poverty) %>%
  group_by(california_county)

# read in data for tab 3

pollution_map_sf <- pollution_map %>%
  st_as_sf(coords = c('longitude', 'latitude'))

ca_county_map <- st_read(here("data", "ca_counties","CA_Counties_TIGER2016.shp")) %>%
  clean_names() %>%
  select(county_name = name, land_area = aland) %>%
  st_as_sf(coords = "geometry")

# end data for tab 3

# read in data for tab 4

# end data for tab 4

# read in data for tab 5

# end data for tab 5


##### theme #####

shiny_theme <- bs_theme(
  bg = "#FFDAB9",
  fg = "#800000",
  primary = "white",
  base_font = font_google("Lato"),
  heading_font = font_google("Lato")
)

##### start shiny app UI #####

ui <- fluidPage(theme = shiny_theme,

##### homepage - tab 1 #####
                
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
    ), # end tabPanel
    
##### tab 2  #####
    
    tabPanel("California Pollution Score by Poverty",
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "pick_california_county",
                                     label = h3("Choose California County:"),
                                     choices = unique(calenviroscreen4$california_county),
                                     selected = "Los Angeles"), #end checkboxGroupInput
                  hr(),
                  helpText("By selecting a county from the top-down menu, users can view the differences in the pollution burden score."),
                ), # end sidebarPanel 2
              mainPanel(plotOutput("pollution_plot")) # end mainPanel 2
              ) # end sidebarLayout 2
    ), # end tabpanel 2

##### tab 3 #####

    tabPanel("California Pollution Map", # start panel 2
             mainPanel( # start main panel 2
               tmapOutput("tmap_ej")
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

##### tab 4 #####


##### tab 5 #####

    ) #end navbar
) # end ui

###### End user interface : start server #####

server <- function(input, output) {

##### tab 2 output #####
  
# select county
  
  cal_reactive1 <- reactive({
    pollution_map %>%
      filter(california_county %in% c(input$pick_california_county))
  }) 
  
# reactive plot
  
  output$pollution_plot <- renderPlot(
    
    ggplot(data = cal_reactive1(), aes(x = poverty, y = pollution_burden)) +
      geom_col(fill = "darkred", color = "black", stat = "identity", width = 0.5) +
      theme_minimal(base_size = 12) +
      labs(x = "Poverty", 
           y = "Pollution Burden",
           title = "Pollution Burden by Poverty") +
      theme(axis.text = element_text(size = 12))
  ) # end renderPlot
  
##### tab 3 output #####

output$tmap_ej <- renderTmap({
    tm_shape(ca_county_map) +
    tm_polygons("land_area", legend.show = FALSE) +
    tm_shape(pollution_map_sf) +
    tm_bubbles("pollution_burden_score")
})

  pollution_variables <- reactive({
    pollution_map %>%
      filter(california_county %in% input$pick_california_county)
  }) # end output$cal_plot 1
  
  
##### tab 4 output #####
  
  
##### tab 5 output #####
  cal_reactive2 <- reactive({
    calenviroscreen4 %>%
      filter(california_county %in% input$pick_california_county)
  }) # end output$cal_plot 2
  
# graph for widgit 2
    output$cal_plot2 <- renderPlot(
    ggplot(data = cal_reactive2(), aes(x = ozone, y = haz_waste)) +
    geom_point(aes(color = california_county))
    ) # end output$cal_plot1
} 

##### end server #####


shinyApp(ui = ui, server = server)



