library(shiny)

source("module/submodule/tabpanelModule.R", encoding="UTF-8")

aui <- function(ns){
  fluidRow(
    numericInput(ns("num"), "num", min = 1, max = 10, value = 5),
    plotOutput(ns("out"))
  )
}

ui <- fluidPage(
  tabPanelUI("one",aui)
)

server <- function(input,output,session){
  gg1 <- function(.data, inp){
    ggplot(.data)+geom_histogram(aes(x=rnorm(100)), bins = inp) + labs(title="gg1")
  }
  
  gg2 <- function(.data, inp){
    ggplot(.data)+geom_histogram(aes(x=rnorm(100)), bins = inp) + labs(title="gg2")
  }
  
  
  
  tabPanelServer("one", gg1, gg2)
}


shinyApp(ui,server)
