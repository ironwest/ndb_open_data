library(colorspace)
library(dplyr)
library(DT)
library(shiny)
library(shinyWidgets)
library(magrittr)
library(openxlsx)
library(officer)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(showtext)
library(shinydashboard)

showtext_auto()

source("functions/functions.R"      , encoding = "UTF-8")
source("module/kensa_module.R"      , encoding = "UTF-8")
source("module/monsin_module.R"     , encoding = "UTF-8")
source("module/ika_module.R"        , encoding = "UTF-8" )
source("module/ika_pref_module.R"   , encoding = "UTF-8")
source("module/med_overall_module.R", encoding = "UTF-8" )
source("module/med_pref_module.R"   , encoding = "UTF-8")
source("module/med_age_module.R"    , encoding = "UTF-8" )

# UI-----------------------------
## Dashboard Header------------------------------
dhead <- dashboardHeader(title = "NDBオープンデータ ダッシュボード")

## Dashboard Sidebar----------------------------
dside <- dashboardSidebar(
  sidebarMenu(
    menuItem("特定健診:検査"        , tabName = "kensa"     , icon = icon("vials")),
    menuItem("特定健診:問診"        , tabName = "monsin"    , icon = icon("pencil-alt")),
    menuItem("医科診療行為:年齢性別", tabName = "ika_agesex", icon = icon("stethoscope")),
    menuItem("医科診療行為:都道府県", tabName = "ika_pref"  , icon = icon("map")),
    menuItem("医薬品:全体集計"      , tabName = "med_all"   , icon = icon("prescription-bottle-alt")),
    menuItem("医薬品:都道府県"      , tabName = "med_pref"  , icon = icon("tablets"))
    #menuItem("医薬品:年齢性別別"    , tabName = "med_age"  , icon = icon("birthday-cake"))
  )
)

## Dashboard Body------------------------------

tab_kensa  <- tabItem(tabName = "kensa"     ,kensaUI("kensa"))
tab_monsin <- tabItem(tabName = "monsin"    ,monsinUI("monsin"))
tab_ika    <- tabItem(tabName = "ika_agesex",ika_sex_age_UI("ika"))
tab_ikapref<- tabItem(tabName = "ika_pref"  ,ika_pref_UI("ika_pref"))
tab_medall <- tabItem(tabName = "med_all"   ,med_overall_UI("med_all"))
tab_medpref   <-tabItem(tabName = "med_pref"  ,med_pref_UI("med_pref")) 
#tab_agegender <-tabItem(tabName = "med_age"   ,med_age_UI("med_age")) 

dbody <- dashboardBody(
  tabItems(
    tab_kensa,tab_monsin, tab_ika, tab_ikapref,tab_medall,tab_medpref#, tab_agegender
  )
)

ui <- dashboardPage(header = dhead, sidebar = dside, body = dbody)

#Server----------------------------
server <- function(input, output){
  kensaServer("kensa")
  monsinServer("monsin")
  ika_sex_age_Server("ika")
  ika_pref_Server("ika_pref")
  med_overall_Server("med_all")
  med_pref_Server("med_pref")
  #med_age_Server("med_age")
}

shinyApp(ui,server)