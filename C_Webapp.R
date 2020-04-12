### Program to webscrap information about###
###CoronaVirus around the world and then map it###

######## LOADING PACKAGES ##################
install.packages("rvest")
install.packages("dplyr")
install.packages("tmap")
install.packages("tmaptools")
install.packages("sf")
install.packages("leaflet")
install.packages("scales")
install.packages("shiny")
install.packages("shinydashboard")

library(rvest)
library(dplyr)
library(tmap)
library(tmaptools)
library(sf)
library(leaflet)
library(scales)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plyr)


################## DATA WRANGLING #################
### Getting Data from html page


get.data <- function(x){
  CoronaPage <- "https://www.worldometers.info/coronavirus/"
  Corona <- read_html(CoronaPage)
  
  
  body_nodes <- Corona %>% html_node("body") %>% html_children()
  
  
  
  c_table <- CoronaPage %>% html() %>% 
    html_nodes(xpath = '//*[@id="main_table_countries_today"]') %>%
    html_table()
  
  c_dataframe <- as.data.frame(c_table)

  ## Changing the name of the top 50 Countries to ensure its the same
  ## in the shape file as well as the datafile

  c_dataframe <- as_tibble(lapply(c_dataframe, FUN = function(foo)
    recode(foo,"S. Korea"="South Korea",
           "USA" = "United States",
           "UK" = "United Kingdom",
           "Czechia" = "Czech Republic",
           "UAE" = "United Arab Emirates")))
  c_dataframe
}
get.infobox.val <- function(x){
  
  df1 <- get.data() # run the scraping function above and assign that data.frame to a variable
  df1 <- df1$TotalCases[1]  # assign the first value of the % gain column to same variable
  df1   # return value
  
}

get.map <- function(x){
  ### Loading shape file map function
  
  shapefile <-"~/Desktop/Coding /R Code/World_Countries.shp"
  world_shp <- st_read(shapefile)
  
  ## Creating map
  
  wmap <- append_data(world_shp,c_dataframe,key.shp = "COUNTRY",key.data ="Country.Other",
                      ignore.duplicates=TRUE,ignore.na = TRUE)
  
  
  ##improving looks
  
  mypal <- colorNumeric(palette = "Blues",domain = NULL)
  palbin <- colorBin(palette = "Blues", domain =as.numeric(wmap$TotalCases),bins = 9, na.color = "C0C0C0"
                     ,pretty = FALSE)
  qpal <- colorQuantile("Blues", domain = as.numeric(wmap$TotalCases), n=9)
  
  vpopup <- paste0("Country: ",wmap$COUNTRY,
                   "<br>Total Cases: ",wmap$TotalCases,
                   "<br>Total Deaths: ",wmap$TotalDeaths)
  wmap
}

get.infobox.name <- function(x){
  
  df <- get.data()  # run the scraping function above and assign that data.frame to a variable
  df <- df$Country.Other[1]  # assign the first value of the name column to same variable
  df   # return value
  
}


########## UI ################

ui <- dashboardPage (
  
  #HEADER
  
  dashboardHeader(title = "Spread of Coronavirus"),
  
  #SIDEBAR
  
  dashboardSidebar(
    
    h5("A dashboard showing some information about the spread of COVID-19 across the world"),
    
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    h6("Project created by Anirudh Chatty "),
    br(),
    a("anirudh.chatty@gmail.com", href="mailto:anirudh.chatty@gmail.com")
  ),
  
  #BODY
  dashboardBody(
    
    fluidRow(
      
      # Info Box - Value
      infoBoxOutput("top.Value",
                    width = 5),
      
      # Info Box - Country
      infoBoxOutput("top.Country",
                    width = 5)
      
    ),
    
    fluidRow(
      
      column(
        # Data table
        box(
          status = "primary",
          headerPanel("Data Table"),
          solidHeader = T,
          br(),
          DT::dataTableOutput("table", height = "550px"),
          width = 12,
          height = "600px"
        ),
        
        # Chart
        box(
          status = "primary",
          headerPanel("Map"),
          solidHeader = T,
          br(),
          leafletOutput("plot", height = "600px"),
          width = 12,
          height = "700px"
        ),
        width = 12
      )
      
    )
  )
)

########## SERVER ################

server <- function(input,output,session)  {
  
  
  #REACTIVE
  liveish_data <- reactive({
    invalidateLater(4000000)
    get.data()
  })
  
  live.infobox.val <- reactive({
    invalidateLater(4000000)
    get.infobox.val()
  })
  
  live.infobox.name <- reactive({
    invalidateLater(4000000)
    get.infobox.name()
  })
  
  ## OUTPUT FOR DATA TABLE
  output$table <- DT::renderDataTable(DT::datatable({data<- liveish_data()}))
  
  ## OUTPUT FOR MAP
  output$plot <- renderLeaflet({leaflet(wmap) %>%
        addProviderTiles("CartoDB.Positron") %>%
       addPolygons(stroke = FALSE,
                   smoothFactor = 0.2,
                    fillOpacity = 0.5,
                    popup = vpopup,
                    color = ~qpal(as.numeric(wmap$TotalCases))) })
  
  # INFO BOX - VALUE OUTPUT
  output$top.Value <- renderInfoBox({
    infoBox(
      "Highest Country Count", paste0(live.infobox.val()),
      icon = icon("signal"),
      color = "purple",
      fill = TRUE)
  })
  # INFO BOX - COUNTRY NAME OUTPUT
  output$top.Country <- renderInfoBox({
    infoBox(
      "Country", paste0(live.infobox.name()),
      icon = icon("signal"),
      color = "purple",
      fill = TRUE)
  })    
  
}

########## DEPLOY ################

shinyApp(ui = ui, server = server)

