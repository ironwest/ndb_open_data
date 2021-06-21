library(shiny)
library(shinyWidgets)
library(dplyr)
library(magrittr)
library(colorspace)
library(DT)
library(openxlsx)
library(officer)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(showtext)
library(shinycssloaders)
library(knitr)
library(cowplot)
library(geofacet)
library(leaflet)

showtext_auto()

source("functions/functions.R" , encoding = "UTF-8")
source("module/med_pref_module.R"   , encoding = "UTF-8" )


ui <- med_pref_UI("mod")


server <- function(input, output){
  # kensaServer("kensa")
  # monsinServer("monsin")
  #ika_pref_Server("ika_pref")
  med_pref_Server("mod")
  
  # res <- iyakuhin_type_server("mod")
  # 
  # output$out <- renderText({unlist(res())})
}

shinyApp(ui,server)