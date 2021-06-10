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
library(shinydashboard)

showtext_auto()

source("functions/functions.R" , encoding = "UTF-8")
source("module/ika_pref_module.R"   , encoding = "UTF-8" )

ui <- ika_pref_UI("ika_pref")

server <- function(input, output){
  # kensaServer("kensa")
  # monsinServer("monsin")
  ika_pref_Server("ika_pref")
}

shinyApp(ui,server)