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

showtext_auto()

source("functions/functions.R" , encoding = "UTF-8")
source("module/med_overall_module.R"   , encoding = "UTF-8" )
source("module/sub_module/iyakuhin_type_module.R"   , encoding = "UTF-8" )

ui <- med_overall_UI("mod")
# ui <- fluidPage( iyakuhin_type_UI("mod"), textOutput("out") )

server <- function(input, output){
  # kensaServer("kensa")
  # monsinServer("monsin")
  #ika_pref_Server("ika_pref")
  med_overall_Server("mod")
  
  # res <- iyakuhin_type_server("mod")
  # 
  # output$out <- renderText({unlist(res())})
}

shinyApp(ui,server)