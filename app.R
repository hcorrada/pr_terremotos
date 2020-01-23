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
  mutate(day=lubridate::round_date(time,"12 hours")) %>%
  mutate(mag_cat=factor(ceiling(mag)))
 
make_plot <- function(slider_time) {
  dat %>%
    mutate(time_cat=lubridate::round_date(day, "12 hours") ==
             lubridate::round_date(slider_time, "12 hours")) %>%
    mutate(alpha=ifelse(time_cat, 1, 0.75)) %>%
    ggplot(aes(x=time,y=mag,color=mag_cat,size=mag^2, alpha=alpha)) +
      geom_point() +
      theme_bw() +
      theme(legend.position="none") +
      xlab("fecha") +
      ylab("magnitud")
}

sf <- lubridate::stamp("Jan 7, 2020 4am")

ui <- fluidPage(
  h1("Serie de Terremotos en Puerto Rico Invierno 2019-2020"),
  sliderInput("time", "Fecha", 
              min(dat$day),
              max(dat$day),
              value=min(dat$day),
              step=12*60*60,
              animate=T),
  leafletOutput("themap"),
  h3(textOutput("fecha")),
  plotOutput("theplot")
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
  
  output$theplot <- renderPlot(
    make_plot(input$time)
  )
  
  output$fecha <- renderText(
    input$time %>%
      lubridate::round_date("12 hours") %>%
      sf()
  )
}

shinyApp(ui, server)
