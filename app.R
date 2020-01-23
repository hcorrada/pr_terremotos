library(tidyverse)
library(httr)
library(leaflet)
library(shiny)

url <- "https://earthquake.usgs.gov/fdsnws/event/1/query?"
url <- paste0(url, "format=csv&updatedafter=2019-12-25")
url <- paste0(url, "&minlatitude=17&maxlatitude=19&minlongitude=-68&maxlongitude=-64")

res <- GET(url)

dat <- res %>%
  content() %>%
  mutate(day=lubridate::round_date(time,"12 hours"))
 

ui <- fluidPage(
  sliderInput("time", "fecha", 
              min(dat$day),
              max(dat$day),
              value=min(dat$day),
              step=12*60*60,
              animate=T),
  leafletOutput("themap")
)

server <- function(input, output, session) {
  points <- reactive({
    dat %>%
      filter(lubridate::round_date(day, "12 hours") == 
               lubridate::round_date(input$time, "12 hours"))
  })
  
  output$themap <- renderLeaflet({
      leaflet(options=leafletOptions(zoomControl=FALSE)) %>%
      setView(lat=18, lng=-66.5, zoom=8) %>%
      addTiles() %>%
      addCircleMarkers(data=points(),
                       lat=~latitude,
                       lng=~longitude,
                       radius=~mag*2.5,
                       color="navy",
                       fillOpacity=0.5,
                       stroke=FALSE)
  })
}

shinyApp(ui, server)
