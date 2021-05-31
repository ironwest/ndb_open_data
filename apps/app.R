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

source("functions.R", encoding = "UTF-8")
source("module/kensa_module.R", encoding = "UTF-8")

# UI-----------------------------
## Dashboard Header------------------------------
dhead <- dashboardHeader(title = "NDBオープンデータ ダッシュボード")

## Dashboard Sidebar----------------------------
dside <- dashboardSidebar(
  sidebarMenu(
    menuItem("特定健診:検査", tabName = "kensa", icon = icon("vials")),
    menuItem("特定健診:問診", tabName = "monsin", icon = icon("pencil-alt"))
  )
)

## Dashboard Body------------------------------

tab_kensa <- tabItem(tabName = "kensa",kensaUI("kensa"))

tab_monsin <- tabItem(tabName = "monsin", "MONSIN")

dbody <- dashboardBody(
  tabItems(
    tab_kensa,
    tab_monsin
  )
)

ui <- dashboardPage(header = dhead, sidebar = dside, body = dbody)

#Server----------------------------
server <- function(input, output){
  kensaServer("kensa")
}

shinyApp(ui,server)