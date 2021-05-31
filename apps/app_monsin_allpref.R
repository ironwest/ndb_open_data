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

showtext_auto()

source("functions.R", encoding = "UTF-8")

raw_data <- read_rds("data/processed/tokutei_monsin/data.rds")
q_data <- read_rds("data/processed/tokutei_monsin/question.rds")

set_koumoku <- read_rds("setting/ndb_kensa_koumoku.rds")
set_pref <- read_rds("setting/ndb_prefecture.rds")
set_age <- read_rds("setting/ndb_age_kubun.rds")
set_kubun <- read_rds("setting/ndb_kensa_kubun.rds")
set_nendo <- read_rds("setting/ndb_nendo.rds")

#side panel -----------------------------------
side_panel <- sidebarPanel(
  h2("設定:"),
  pickerInput("seireki","西暦年",choices=2014:2017,selected = 2017),
  pickerInput("kensa", "検査項目",choices = set_koumoku$kensa_koumoku),
  pickerInput("pref", "都道府県", choices = set_pref$pref, multiple = TRUE),
  radioGroupButtons("sex","性別:",choices=c("全体","男","女"), selected="全体"),
  pickerInput("agegrp","年齢",choices=set_age$age,selected=1),
  sliderInput("perc_range", "グラフの%の表示範囲",min=0, max=100,value=c(0,100),step=1,post="%"),
  uiOutput("kubun_d"),
  hr(),
  downloadButton("dlppt","ダウンロード(PPT)"),
  downloadButton("dlexcel","ダウンロード(EXCEL)"),
  p("使い方"),
  p("ダウンロード(PPT)で、表示されているグラフを含んだスライドをダウンロードできます。"),
  p("ダウンロード(EXCEL)で、データの表に表示されているテーブルが保存されたエクセルファイルをダウンロードできます。"),
  width=3
)

#tabpanel1:
panel1 <- tabPanel(
  title = "Q1:血圧降下薬",
  fluidRow(
    column(width=6, withSpinner( plotOutput("graph_q1_single",height=400) ) ),
    column(width=6, withSpinner( plotOutput("graph_q1_multiple",height=400) ) )
  ),
  fluidRow(
    column(width=6, withSpinner( plotOutput("graph_q1_male_single",height=400) ) ),
    column(width=6, withSpinner( plotOutput("graph_q1_male_multiple",height=400) ) )
  ),
  fluidRow(
    column(width=6, withSpinner( plotOutput("graph_q1_female_single",height=400) ) ),
    column(width=6, withSpinner( plotOutput("graph_q1_female_multiple",height=400) ) )
  ),
  fluidRow(
    column(width=6, withSpinner( plotOutput("graph_q1_age_single",height=400) ) ),
    column(width=6, withSpinner( plotOutput("graph_q1_age_multiple",height=400) ) )
  )
  
  
)

ui <- fluidPage(
  title = "NDBオープンデータ可視化:特定健診",
  titlePanel("NDBオープンデータ可視化:特定健診 全都道府県比較 問診結果"),
  sidebarLayout(
    sidebarPanel = side_panel,
    mainPanel = mainPanel(
      tabsetPanel(
        tabPanel(
          title = "全都道府県グラフ",
          fluidRow(
            column(width=6,plotOutput("graph_bar_all_pref" , height=400)),
            column(width=6,plotOutput("graph_line_all_pref", height=400))
          )
        ),
        tabPanel(
          title = "全都道府県:男女別グラフ",
          fluidRow(
            column(width=6,plotOutput("graph_bar_all_pref_male", height=400)),
            column(width=6,plotOutput("graph_line_all_pref_male", height=400))
          ),
          fluidRow(
            column(width=6,plotOutput("graph_bar_all_pref_female", height=400)),
            column(width=6,plotOutput("graph_line_all_pref_female", height=400))
          )
        ),
        tabPanel(
          title = "全都道府県:年齢別グラフ",
          fluidRow(
            column(width=6, plotOutput("graph_bar_all_pref_age", height=400)),
            column(width=6, plotOutput("graph_line_all_pref_age", height=400))
          )
        ),
        tabPanel(
          title = "年齢、性別別グラフ",
          fluidRow(
            column(width=12,plotOutput("graph_age_sex",height=800))
          )
        ),
        tabPanel(
          title = "表",
          fluidRow(
            column(width=6,
                   checkboxGroupButtons(
                     inputId = "groupvar",
                     label = "集計する変数を指定",
                     choices=c("都道府県" = "pref", "性別"="sex","年齢区分"="age"),
                     select = "都道府県")
            ),
            column(width=6,
                   radioGroupButtons(
                     inputId = "kensa_kubun",
                     label="割合を計算する検査区分を指定",
                     choices=c("複数区分","2値区分"))),
            column(width=12, dataTableOutput("hyou"))
          )
        )
      )
    )))


server <- function(input, output){