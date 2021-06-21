library(tidyverse)
library(shiny)
library(sf)
library(leaflet)

ui <- fluidPage(
  fluidRow(column(width = 8, leafletOutput("map",height=1200)),
           column(width = 3 , verbatimTextOutput("mouse")))
)

server <- function(input, output,session){
  
  dat_map <- read_rds("data/processed/map.rds")
  
  output$map <- leaflet::renderLeaflet({
    # ggplot(dat_map) +
    #   geom_sf() +
    #   coord_sf(xlim = c(125,145.5), y = c(25,46))
    leaflet(dat_map) %>% 
      addPolygons(weight = 2) %>% 
      addTiles()
    
  })
  
  output$mouse <- renderPrint({
    #return(input$map_shape_click)
    ccc <- input$map_shape_click
    
    pt_lat <<- ccc$lat
    pt_lon <<- ccc$lng
    the_pt <<- st_point(c(pt_lat, pt_lon))
    ddd <<- dat_map %>%
      select(name_ja, geometry)
      
    ddd %>% 
      mutate(is_within = map_lgl(geometry, ~{as.logical(st_within(the_pt, .))})) %>%
      filter(is_within) %>%
      pull(name_ja)

    okinawa <- st_polygon(list(rbind(c(125,25), c(125,30), c(130,30), c(130,25), c(125,25))))

    within_okinawa <- if_else(as.logical(st_within(the_pt, okinawa)),TRUE,FALSE,FALSE)

    if(within_okinawa){
      ddd <- "Okinawa"
    }
    warning(ddd)
    return(ddd)
  
  })
}

shinyApp(ui,server)



