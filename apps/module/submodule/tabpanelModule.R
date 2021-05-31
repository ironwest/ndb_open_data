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

tabPanelUI <- function(id){
  ns <- NS(id)
  q_data <- read_rds("data/processed/tokutei_monsin/question.rds")
  
  set_koumoku <- read_rds("setting/ndb_kensa_koumoku.rds")
  set_pref <- read_rds("setting/ndb_prefecture.rds")
  set_age <- read_rds("setting/ndb_age_kubun.rds")
  set_kubun <- read_rds("setting/ndb_kensa_kubun.rds")
  set_nendo <- read_rds("setting/ndb_nendo.rds")
}

tabPanelServer <- function(id, ...){
  moduleServer(
    id,
    function(input, output, session){
      
      browser()
      
    }
      
  )
  
}
