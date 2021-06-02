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

source("functions/functions.R", encoding = "UTF-8")

kensaUI <- function(id){
  ns <- NS(id)
  
  #data and setting file------------------------
  raw_data <- read_rds("data/processed/tokutei_kensin/data.rds")
  set_koumoku <- read_rds("setting/ndb_kensa_koumoku.rds")
  set_pref <- read_rds("setting/ndb_prefecture.rds")
  set_age <- read_rds("setting/ndb_age_kubun.rds")
  set_kubun <- read_rds("setting/ndb_kensa_kubun.rds")
  set_nendo <- read_rds("setting/ndb_nendo.rds")
  
  #ui side panel ----------------------------------
  side_panel <- sidebarPanel(
    h2("設定:"),
    pickerInput(ns("seireki"),"西暦年",choices=2014:2017,selected = 2017),
    pickerInput(ns("kensa"), "検査項目",choices = set_koumoku$kensa_koumoku),
    pickerInput(ns("pref"), "都道府県", choices = set_pref$pref, multiple = TRUE),
    pickerInput(ns("agegrp"),"年齢",choices=set_age$age,selected=1),
    sliderInput(ns("perc_range"), "グラフの%の表示範囲",min=0, max=100,value=c(0,100),step=1,post="%"),
    uiOutput(ns("kubun_d")),
    hr(),
    downloadButton(ns("dlppt"),"ダウンロード(PPT)"),
    downloadButton(ns("dlexcel"),"ダウンロード(EXCEL)"),
    p("使い方"),
    p("ダウンロード(PPT)で、表示されているグラフを含んだスライドをダウンロードできます。"),
    p("ダウンロード(EXCEL)で、データの表に表示されているテーブルが保存されたエクセルファイルをダウンロードできます。"),
    width=3
  )
  
  #ui----------------------------------------
  
  ui <- fluidPage(
    title = "NDBオープンデータ可視化:特定健診",
    titlePanel("NDBオープンデータ可視化:特定健診検査結果"),
    sidebarLayout(
      sidebarPanel = side_panel,
      mainPanel = mainPanel(
        tabsetPanel(
          tabPanel(
            title = "全都道府県グラフ",
            fluidRow(
              column(width=6, withSpinner( plotOutput( ns("graph_bar_all_pref") , height=400)) ),
              column(width=6, withSpinner( plotOutput( ns("graph_line_all_pref"), height=400)) )
            )
          ),
          tabPanel(
            title = "全都道府県:男女別グラフ",
            fluidRow(
              column(width=6, withSpinner( plotOutput( ns("graph_bar_all_pref_male") , height=400)) ),
              column(width=6, withSpinner( plotOutput( ns("graph_line_all_pref_male"), height=400)) )
            ),
            fluidRow(
              column(width=6, withSpinner( plotOutput( ns("graph_bar_all_pref_female") , height=400)) ),
              column(width=6, withSpinner( plotOutput( ns("graph_line_all_pref_female"), height=400)) )
            )
          ),
          tabPanel(
            title = "全都道府県:年齢別グラフ",
            fluidRow(
              column(width=6, withSpinner( plotOutput( ns("graph_bar_all_pref_age") , height=400)) ),
              column(width=6, withSpinner( plotOutput( ns("graph_line_all_pref_age"), height=400)) )
            )
          ),
          tabPanel(
            title = "年齢、性別別グラフ",
            fluidRow(
              column(width=12, withSpinner( plotOutput( ns("graph_age_sex"), height=800)) )
            )
          ),
          tabPanel(
            title = "表",
            fluidRow(
              column(width=6,
                     checkboxGroupButtons(
                       inputId = ns("groupvar"),
                       label = "集計する変数を指定",
                       choices=c("年度"="seireki","都道府県" = "pref", "性別"="sex","年齢区分"="age"),
                       select = "都道府県")
              ),
              column(width=6,
                     radioGroupButtons(
                       inputId = ns("kensa_kubun"),
                       label="割合を計算する検査区分を指定",
                       choices=c("複数区分","2値区分"))),
              column(width=12, dataTableOutput(ns("hyou")))
            )
          )))))
}      


kensaServer <- function(id){
  moduleServer(
    id,
    function(input,output,session) {
      ns <- session$ns
      
      raw_data <- read_rds("data/processed/tokutei_kensin/data.rds")
      set_koumoku <- read_rds("setting/ndb_kensa_koumoku.rds")
      set_pref <- read_rds("setting/ndb_prefecture.rds")
      set_age <- read_rds("setting/ndb_age_kubun.rds")
      set_kubun <- read_rds("setting/ndb_kensa_kubun.rds")
      set_nendo <- read_rds("setting/ndb_nendo.rds")
      
      #UI部分のコントロールを行うLogic-----------------------
      levlab <- reactive({
        extract_levlab(input$kensa,set_kubun)
      })
      
      output$kubun_d <- renderUI({
        
        choices <- unique(levlab()$label)
        upto <- choices[length(choices)-1]
        sliderTextInput(ns("kubun_d"),"検査区分(二値とした場合の低値の範囲を指定)",choices=choices,from_min=choices[1], from_max=upto)
      })
      
      dich_label <- reactive({
        #二値変数にしたばあいに、カットオフの次のカテゴリの　XXXX以上XXXX未満の　XXXX以上を取り出す。
        str_extract(unique(levlab()$label)[which(unique(levlab()$label) == input$kubun_d)+1],".+以上")
      })
      
      #データの加工部分---------------------------------
      dat <- reactive({
        dat <- filter_kensa(
          .data        = raw_data,
          target_kensa = input$kensa, 
          set_kubun    = set_kubun, 
          set_nendo    = set_nendo
        ) 
        
        lowlev <- extract_levlab(input$kensa, set_kubun)$label %>% 
          unique()
        
        low_position <- which(lowlev == input$kubun_d)
        
        dat2 <- dat %>%
          add_dichotomous(
            target_kensa = input$kensa, 
            low_lev      = lowlev[1:low_position]
          )
        
        return(dat2)
      })
      
      #表の描画部分-------------------------
      hyou_data <- reactive({
        if(input$kensa_kubun == "複数区分") ktgt <- "m"
        if(input$kensa_kubun == "2値区分")  ktgt <- "d"
        
        if(length(input$groupvar) > 0){
          
          hyou <- dat() %>% 
            make_percent(kaisou=ktgt, input$groupvar) %>% 
            rename("NDB集計値" = value, "集計変数の総数" = total,　"割合" = perc)  
        }else{
          hyou <- tibble(ERROR = "何か集計する変数を指定してください")
        }
        
        names(hyou)[names(hyou)=="sex"] <- "性別"
        names(hyou)[names(hyou)=="age"] <- "年齢区分"
        names(hyou)[names(hyou)=="kaisou_di"] <- "検査値階層"
        names(hyou)[names(hyou)=="kaisou"] <- "検査値階層"
        
        return(hyou)
      })
      
      output$hyou <- renderDataTable({
        hyou_data()
      })
      
      output$dlexcel <- downloadHandler(
        filename = function(){
          
          if(input$kensa_kubun == "2値区分"){
            txt_kubun <- str_c("2値",str_replace(dich_label(),"\\.",""))
          }
          
          if(input$kensa_kubun == "複数区分"){
            txt_kubun <- "複数区分"
          }
          
          gv <- input$groupvar
          
          gv[gv == "都道府県"] <- "pref"
          
          txt_gv <- str_c(gv,collapse="_")
          
          fn <- str_c("table","_",Sys.Date(),"_", input$kensa,"_", txt_kubun,"_", txt_gv,".xlsx")
          
          return(fn)  
        },
        content = function(file){
          wb <- createWorkbook()
          addWorksheet(wb,"table")
          writeData(wb,"table",hyou_data())
          saveWorkbook(wb,file=file)
        }
      )
      
      #グラフの描画部分--------------------------------
      func_g_bar_all_pref <- function(.data,percent_range){
        gt <- .data %>% 
          mutate(fill_this = if_else(pref %in% input$pref, pref, NA_character_))
        
        gg <- ggplot(.data) +
          geom_col(aes(y = reorder(pref, perc),x=perc), fill="grey80") +
          geom_col(aes(y = reorder(pref, perc),x=perc, fill=fill_this), data=gt) +
          scale_x_continuous(labels=scales::percent) +
          scale_y_discrete(guide = guide_axis(n.dodge=3)) +
          scale_fill_manual(values = rainbow_hcl(length(input$pref)), guide = guide_none()) +
          labs(x = "[%]", y=NULL) +
          coord_cartesian(xlim = percent_range/100) +
          theme_classic(base_size = 14) +
          theme(plot.title.position = "plot")
      }
      
      func_data_g_bar_all_pref <- function(.data, ...){
        
        gdat1 <- .data %>% 
          make_percent(kaisou="d", "seireki", "pref", ...)
        
        gdat2 <- gdat1 %>%
          filter(seireki == input$seireki) %>%
          filter(kaisou_di == levels(kaisou_di)[2])
        
        return(gdat2)
      }
      
      gen_bar_all_pref <- function(){
        
        gdat1 <- func_data_g_bar_all_pref(dat())
        
        if(is.null(input$pref)){
          gg <- ggplot() + 
            geom_text(aes(x=1,y=1,label="都道府県を選択してください")) +
            theme_void(base_size = 24)
        }else{
          gg <- func_g_bar_all_pref(gdat1, input$perc_range) +
            labs(title=str_c(input$seireki,"年度:",input$kensa,dich_label(),"の割合"))
        }
        return(gg)
      }
      
      gen_bar_all_pref_male <- function(){
        gdat1 <- func_data_g_bar_all_pref(dat(), "sex") %>% 
          filter(sex == "男")
        
        if(is.null(input$pref)){
          gg <- ggplot() +
            geom_text(aes(x=1, y=1, label="都道府県を選択してください")) +
            theme_void(base_size=24)
        }else{
          gg <- func_g_bar_all_pref(gdat1,input$perc_range) +
            labs(title=str_c(input$seireki,"年度:男性の",input$kensa,dich_label(),"の割合"))
        }
        return(gg)
      }
      
      gen_bar_all_pref_female <- function(){
        gdat1 <- func_data_g_bar_all_pref(dat(), "sex") %>% 
          filter(sex == "女")
        
        if(is.null(input$pref)){
          gg <- ggplot() +
            geom_text(aes(x=1, y=1, label="都道府県を選択してください")) +
            theme_void(base_size=24)
        }else{
          gg <- func_g_bar_all_pref(gdat1,input$perc_range) +
            labs(title=str_c(input$seireki,"年度:女性の",input$kensa,dich_label(),"の割合"))
        }
        
        return(gg)
      }
      
      gen_bar_all_pref_age <- function(){
        gdat1 <- func_data_g_bar_all_pref(dat(), "age") %>% 
          filter(age == input$agegrp)
        
        if(is.null(input$pref)){
          gg <- ggplot() +
            geom_text(aes(x=1, y=1, label="都道府県を選択してください")) +
            theme_void(base_size=24)
        }else{
          gg <- func_g_bar_all_pref(gdat1,input$perc_range) +
            labs(title=str_c(input$seireki,"年度:",input$agegrp,"の",input$kensa,dich_label(),"の割合"))
        }
        
        return(gg)
      }
      
      #output$graph_bar_all_pref ------
      output$graph_bar_all_pref <- renderPlot({
        return(gen_bar_all_pref())
      })
      
      #output$graph_bar_all_pref_male -----------------------
      output$graph_bar_all_pref_male <- renderPlot({
        return(gen_bar_all_pref_male())
      })
      
      #output$graph_bar_all_pref_female -----------------------
      output$graph_bar_all_pref_female <- renderPlot({
        return(gen_bar_all_pref_female())
      })
      
      #output$graph_bar_all_pref_age -----------------------
      output$graph_bar_all_pref_age <- renderPlot({
        return(gen_bar_all_pref_age())
      })
      
      
      func_data_g_line_all_pref <- function(.data, ...){
        res <- .data %>%
          make_percent(kaisou="d","seireki","pref", ...) %>%
          filter(kaisou_di == levels(kaisou_di)[2])
        
        return(res)
      }
      
      func_g_line_all_pref <- function(.data,percent_range){
        gdat3 <- .data %>% 
          mutate(color_this = pref %in% input$pref)
        
        gbg <- gdat3 %>% filter(!color_this)
        gfr <- gdat3 %>% filter(color_this)
        
        gg <- ggplot() +
          geom_line(aes(x = seireki, y = perc, group = pref),alpha=0.2, data=gbg) +
          geom_line(aes(x = seireki, y = perc, group = pref, color=pref), data=gfr, size=1) +
          geom_point(aes(x = seireki, y = perc, color=pref), data=gfr, size=2) +
          scale_y_continuous(labels=scales::percent) +
          scale_color_manual(values=rainbow_hcl(length(input$pref))) +
          coord_cartesian(ylim = percent_range/100) +
          labs(x = "西暦[年]",y = NULL) +
          theme_classic(base_size=14) 
        
        return(gg)
      }
      
      gen_line_all_pref <-function(){
        gdat2 <- func_data_g_line_all_pref(dat())
        
        if(is.null(input$pref)){
          gg <- ggplot() + 
            geom_text(aes(x=1,y=1,label="都道府県を選択してください")) +
            theme_void(base_size = 24)
        }else{
          gg <- func_g_line_all_pref(gdat2, input$perc_range) +
            labs(title=str_c(input$kensa,dich_label(),"の割合の経年変化"))
        }
        
        capture_inputs(input=input)
        return(gg)
      }
      
      gen_line_all_pref_male <- function(){
        gdat2 <- func_data_g_line_all_pref(dat(), "sex") %>% 
          filter(sex == "男")
        
        if(is.null(input$pref)){
          gg <- ggplot() + 
            geom_text(aes(x=1,y=1,label="都道府県を選択してください")) +
            theme_void(base_size = 24)
        }else{
          gg <- func_g_line_all_pref(gdat2,input$perc_range)  +
            labs(title=str_c(input$kensa,dich_label(),"の男性の割合の経年変化"))
        }
        capture_inputs(input=input)
        return(gg)
      }
      
      gen_line_all_pref_female <- function(){
        gdat2 <- func_data_g_line_all_pref(dat(), "sex") %>% 
          filter(sex == "女")
        
        if(is.null(input$pref)){
          gg <- ggplot() + 
            geom_text(aes(x=1,y=1,label="都道府県を選択してください")) +
            theme_void(base_size = 24)
        }else{
          gg <- func_g_line_all_pref(gdat2,input$perc_range) +
            labs(title=str_c(input$kensa,dich_label(),"の女性の割合の経年変化"))
        }
        capture_inputs(input=input)
        return(gg)
      }
      
      gen_line_all_pref_age <- function(){
        gdat2 <- func_data_g_line_all_pref(dat(), "age") %>% 
          filter(age == input$agegrp)
        
        if(is.null(input$pref)){
          gg <- ggplot() + 
            geom_text(aes(x=1,y=1,label="都道府県を選択してください")) +
            theme_void(base_size = 24)
        }else{
          gg <- func_g_line_all_pref(gdat2,input$perc_range) +
            labs(title=str_c(input$kensa,dich_label(),"の年齢",input$agegrp,"の割合の経年変化"))
        }
        capture_inputs(input=input)
        return(gg)
      }
      
      #graph_line_all_pref--------------------------
      output$graph_line_all_pref <- renderPlot({
        gen_line_all_pref()
      })
      
      #graph_line_all_pref_male-----------------------
      output$graph_line_all_pref_male <- renderPlot({
        gen_line_all_pref_male()
      })
      
      #graph_line_all_pref_female-------------------------
      output$graph_line_all_pref_female <- renderPlot({
        gen_line_all_pref_female()
      })
      
      #graph_line_all_pref_age------------------------
      output$graph_line_all_pref_age <- renderPlot({
        gen_line_all_pref_age()
      })
      
      g_age_sex <- reactive({
        gdat <- dat()
        
        if(is.null(input$pref)){
          gg <- ggplot() + 
            geom_text(aes(x=1,y=1,label="都道府県を選択してください")) +
            theme_void(base_size = 24)
        }else{
          
          gdat2 <- gdat %>% 
            make_percent(kaisou="d","seireki","pref","age","sex") %>%
            filter(kaisou_di == levels(kaisou_di)[2])
          
          gf <- gdat2 %>% filter(pref %in% input$pref)
          gt <- gf %>% filter(age == input$agegrp)
          
          #
          length_prefs <- length(input$pref)
          
          row_n_by_prefs <- length_prefs %/% 2
          
          
          ggplot(gf) +
            geom_line(aes(x = seireki, y = perc, color=age, group=age),alpha=0.5) +
            geom_point(aes(x = seireki, y = perc, color = age), size=2, data=gt) +
            labs(x = NULL, y = NULL, title=str_c("年代別、性別別",input$kensa,dich_label(),"の割合")) +
            scale_y_continuous(labels=scales::percent) +
            scale_color_discrete(name="年代") +
            facet_wrap(pref~sex, nrow=row_n_by_prefs) +
            theme_classic(base_size = 14) +
            theme(legend.position = "bottom")
        } 
      })
      
      output$graph_age_sex <- renderPlot({
        g_age_sex()
      })
      
      output$dlppt <- downloadHandler(
        filename = function(){
          
          c(
            input$seireki,
            input$kensa,
            str_c(str_remove(input$pref,"県|府|都"),collapse="_"),
            input$agegrp,
            str_remove(dich_label(),"\\.")
          ) %>% str_c(collapse="_") %>% 
            str_c(.,".pptx") %>% 
            return()
          
        },
        content = function(file){
          textsize <- 48
          
          g1 <- gen_bar_all_pref() + theme_classic(base_size=textsize)
          g2 <- gen_line_all_pref() + theme_classic(base_size=textsize)
          g3 <- gen_bar_all_pref_male() + theme_classic(base_size=textsize)
          g4 <- gen_line_all_pref_male() + theme_classic(base_size=textsize)
          g5 <- gen_bar_all_pref_female() + theme_classic(base_size=textsize)
          g6 <- gen_line_all_pref_female() + theme_classic(base_size=textsize)
          g7 <- gen_bar_all_pref_age() + theme_classic(base_size=textsize)
          g8 <- gen_line_all_pref_age() + theme_classic(base_size=textsize)
          g9 <- g_age_sex() + theme_classic(base_size=textsize) +
            theme(legend.position="right")
          
          pres <- read_pptx() %>% 
            add_slide() %>% ph_with(value = g1, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g2, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g3, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g4, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g5, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g6, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g7, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g8, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g9, location=ph_location_fullsize())
          
          print(pres, file)
        }
      )
    }
  )
}
