#### packages

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bslib)
library(shinythemes)
library(here)
library(readxl)
library(janitor)
library(sf)
library(tmap)
library(dplyr)
library(RColorBrewer)
library(shinyWidgets)
library(lubridate)


tmap_mode("view")

#### data

### read in the 2.0 data
ces_2.0 <- read_csv(here("data", "cal_enviro_2.0.csv")) %>% 
  clean_names()

### read in the 3.0 data
ces_3.0 <- read_csv(here("data", "cal_enviro_3.0.csv")) %>% 
  clean_names()

### read in the 4.0 data
suppressWarnings({
  ces_4.0 <- read_excel(here("data", "calenviroscreen40resultsdatadictionary_F_2021.xlsx")) %>% 
    clean_names()
})


suppressWarnings({
calenviroscreen4 <- read_xlsx(here("Data", "calenviroscreen40resultsdatadictionary_f_2021.xlsx")) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  na_if(0.000) %>%
  drop_na() %>%
  clean_names()
})

### tab 2

pollution_graph <- complete_map %>%
  select(california_county, name, value, year) %>%
  filter(year == "2021") %>%
  group_by(california_county)

### tab 3

# pollution map data filtering
pollution_map <- calenviroscreen4 %>%
  select(total_population:ces_4_0_percentile_range, haz_waste, pesticides, tox_release, pollution_burden, pollution_burden_score, poverty) %>%
  group_by(california_county)

# map data

pollution_map_sf <- pollution_map %>%
  st_as_sf(coords = c('longitude', 'latitude'))

ca_county_map <- st_read(here("data", "ca_counties","CA_Counties_TIGER2016.shp")) %>%
  clean_names() %>%
  select(california_county = name)

almost_complete_map <- bind_rows(ces_2.0_clean2, ces_3.0_clean2, ces_4.0_clean2)

complete_map <- almost_complete_map %>% 
  pivot_longer(pollution_burden_pctl:pesticides_pctl) %>% 
  mutate(name = case_when(name %in% c("pollution_burden_pctl") ~ "Pollution Burden %",
                          name %in% c("tox_release_pctl") ~ "Toxic Release %",
                          name %in% c("haz_waste_pctl") ~ "Hazardous Waste %",
                          name %in% c("pm2_5_pctl") ~ "PM 2.5 %",
                          name %in% c("groundwater_threats_pctl") ~ "Groundwater Threats %",
                          name %in% c("pesticides_pctl") ~ "Pesticides %"))

map_data <- left_join(ca_county_map, complete_map, "california_county")


#### tab 4

### wrangle data for tab 3 and 4 

ces_2.0_clean2 <- ces_2.0 %>% 
  select(`pollution_burden_pctl`, `california_county`, `tox_release_pctl`, `haz_waste_pctl`, `pm2_5_pctl`, `groundwater_threats_pctl`, `pesticides_pctl`) %>% 
  group_by(california_county) %>% 
  summarize_all(~mean(.x, na.rm = TRUE)) %>% 
  mutate(year = 2014)


ces_3.0_clean2 <- ces_3.0 %>% 
  select(`pollution_burden_pctl`, `california_county`, `tox_release_pctl`, `haz_waste_pctl`, `pm2_5_pctl`, `groundwater_threats_pctl`, `pesticides_pctl`) %>%
  group_by(california_county) %>% 
  summarize_all(~mean(.x, na.rm = TRUE)) %>% 
  mutate(year = 2018)


ces_4.0_clean2 <- ces_4.0 %>% 
  select(`pollution_burden_pctl`, `california_county`, `tox_release_pctl`, `haz_waste_pctl`, `pm2_5_pctl`, `groundwater_threats_pctl`, `pesticides_pctl`) %>%
  group_by(california_county) %>% 
  summarize_all(~mean(.x, na.rm = TRUE)) %>% 
  mutate(year = 2021)


#### tab 5 

### sort by county for each data set, only include pollution percentage

ces_2.0_clean <- ces_2.0 %>% 
  select(`pollution_burden_pctl`, `california_county`) %>% 
  group_by(california_county) %>% 
  summarize_all(~mean(.x, na.rm = TRUE)) %>% 
  mutate(version = 2)


ces_3.0_clean <- ces_3.0 %>% 
  select(`pollution_burden_pctl`, `california_county`) %>% 
  group_by(california_county) %>% 
  summarize_all(~mean(.x, na.rm = TRUE)) %>% 
  mutate(version = 3)


ces_4.0_clean <- ces_4.0 %>% 
  select(`pollution_burden_pctl`, `california_county`) %>% 
  group_by(california_county) %>% 
  summarize_all(~mean(.x, na.rm = TRUE)) %>% 
  mutate(version = 4)

complete_df <- bind_rows(ces_2.0_clean, ces_3.0_clean, ces_4.0_clean)

complete_df2 <- complete_df %>% 
  rename(year = version) 
  
complete_df2$year[complete_df2$year == 2] <- 2014
complete_df2$year[complete_df2$year == 3] <- 2018
complete_df2$year[complete_df2$year == 4] <- 2021



#### custom theme
shiny_theme <- bs_theme(
  bg = "#4e5d6c",
  fg = "honeydew",
  primary = "white",
  base_font = font_google("Lato"),
  heading_font = font_google("Lato")
)

##### start shiny app ui #####

ui <- fluidPage(theme = shiny_theme,

##### homepage  ######
                
navbarPage("CalEnviroScreen Interactive Tool",
    tabPanel("Project Overview",
             titlePanel(h2("Environmental Justice Screening and Mapping Tool", align = "center")),
      mainPanel(
        fluidRow(
        h1("Project Description", align = "center"),
        p("CalEnviroScreen was designed to assist CalEPA with carrying out its environmental justice mission to ensure the fair treatment of all Californians, including minority and low-income communities.",
          ),
        br(), 
        p("CalEnviroScreen uses environmental, health, and socioeconomic information to produce scores for every census tract in the state. An area with a high score is one that experiences a much higher pollution burden than areas with low scores. CalEnviroScreen ranks communities based on data that are available from state and federal government sources.",
          ),
        br(),
        p("The purpose of this Shiny App is to explore data from CalEnviroScreen 2.0 (2014),  3.0 (2018), and 4.0 (2021) to better visualize how pollution-based parameters affect demographics in California, and to analyze how these parameters have or have not changed over time."),
        br(),
        h2("Data Citation:", align = "center"),
        p("1. United States Environmental Protection Agency. 2018 - 2020. EJScreen. Retrieved: 1/12/22, from url https://www.epa.gov/ejscreen/download-ejscreen-data"),
        ) # end fluidRow
      ), #end mainpanel
    ), # end tabPanel
    
##### tab 2  ######

    tabPanel("California Pollution Burden by County",
              sidebarLayout(
                sidebarPanel(
                  pickerInput(inputId = "pick_county_tab2",
                                     label = h3("Choose California Counties:"),
                                     choices = unique(pollution_graph$california_county),
                                     multiple = TRUE,
                                     selected = "Alameda"
                              ), #end checkboxGroupInput
                  hr(),
                  helpText("By selecting multiple counties from the top-down menu, users can view how pollution variables compare bassed on the 2020 data."),
                ), # end sidebarPanel 2
              mainPanel(plotOutput("pollution_plot"), # end mainPanel 2
              br(),
              ) # end mainPanel
              ) # end sidebarLayout 2
    ), # end tabpanel 2

##### tab 3  ######
    tabPanel("California Pollution Map", # start panel 2
             sidebarLayout(
               sidebarPanel(
                 "Choose Pollution Variable and Year",
                 hr(),
                 radioButtons(inputId = "pick_name",

                             label = "Select Variable",
                             choices = unique(map_data$name),
                             selected = "Pollution Burden %"
                             ), # end selectInput
                 sliderTextInput(inputId = "pick_year",
                             label = "Choose timeframe",
                             choices = unique(map_data$year), 
                             grid = TRUE,
                             animate = TRUE, 
                             dragRange = TRUE
                 ), # end sliderInput
               ), # end sidebarPanel
             mainPanel( # start main panel 2
               tmapOutput(outputId = "pollution_map")
               ) # end main panel 2
             ) # end sidebarLayout
             ), # end tabpanel 2


##### tab 4 ######
    tabPanel("California Pollution Burden Through Time",
            sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "pick_county_tab5",
                            label = h3("Choose California County:"),
                            choices = unique(complete_df2$california_county),
                            selected = "Los Angeles"), #end dropdown county input
             hr(),
             helpText("By selecting a county from the top-down menu, users can view the mean change in pollution burden percentages through time, from 2014-2021."),
              ), # end sidebarPanel 5
           mainPanel(plotOutput("pollutionburden_plot")) # end mainPanel 5
            ) # end sidebarLayout 5
          ), # end tabpanel 5

    ) #end navbar
) 

##### end ui #####

##### start server #####

server <- function(input, output) {

##### Tab 2 output ###### 
# output for widget 1
  
# select county
  
  cal_reactive1 <- reactive({
    pollution_graph %>%
      filter(california_county %in% input$pick_county_tab2)
  }) 
  
# reactive plot
  
  output$pollution_plot <- renderPlot(
    
    ggplot(data = cal_reactive1(), aes(x = name, y = value)) +
      geom_col(aes(fill = california_county), position = "dodge") +
      theme_minimal(base_size = 12) +
      scale_fill_brewer(palette = "Paired") +
      coord_flip() +
      labs(x = "Pollution Variable",
           y = "Percentile %",
           title = "Pollution Variable Percentiles by County",
           fill = "California County") +
      theme(axis.text = element_text(size = 12)) 
  ) # end renderPlot
  
  # end widget 1

##### Tab 3 output #####
# output for widget 3

output$tmap_ej <- renderTmap({
    tm_shape(pollution_map_sf) +
    tm_dots("pollution_burden_score") +
    tm_basemap("OpenStreetMap")
})

# output for widget 2 and 3
  
    ## Contaminant Map 
    map_reactive <- reactive({
      message('input$pick_name = ', input$pick_name)  ## Figure out why pick value is showing up blank 
      message('input$pick_year = ', input$pick_year)
      map_data %>% 
        filter(name %in% input$pick_name) %>% 
        filter(year %in% input$pick_year) %>%
        return(map_data)
    }) # end map_reactive
    
    output$pollution_map <- renderTmap({
      tm_shape(shp = map_reactive()) +
        tm_polygons(col = 'value') +
        tm_fill(col = 'value',
                title = "Mean Contaminant Concentration Per County",
                style = 'cont',
                # popup.vars = c("Population in Poverty (2019)"="povall_2019","Percent of Population in Poverty (2019)"="pctpovall_2019"),
                popup.format = list()) 
    })
    
    
##### tab 4 output #####
    
# select county drop down
    cal_reactive5 <- reactive({
      complete_df2 %>%
        filter(california_county %in% input$pick_county_tab5)
    }) # end output$cal_reactive5
  
# graph for widget 5
    output$pollutionburden_plot <- renderPlot(
    ggplot(data = cal_reactive5(), aes(x = year, y = pollution_burden_pctl)) +
      geom_col(fill = "darkolivegreen4",
               color = "darkolivegreen",
               width = .5) +
      geom_text(aes(label = round(pollution_burden_pctl, 1)), # label exact co2 values on graph
                vjust = 1.3, # adjust label placement and size
                hjust = 0.5,
                size = 4,
                color = "white") +
      scale_x_continuous(breaks = c(2014,2018,2021)) +
      labs(x = "\nYear\n", 
           y = "\nPollution Burden %\n") +
      theme(axis.text = element_text(size = 12)) +
      theme_minimal()
    ) # end renderPlot
} 

##### end server #####


shinyApp(ui = ui, server = server)



