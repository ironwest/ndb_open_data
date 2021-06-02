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

#ModuleUI-------------------
monsinUI <- function(id){
  ns <- NS(id)
  #data and setting file------------------------
  
  set_pref <- read_rds("setting/ndb_prefecture.rds")
  set_age <- read_rds("setting/ndb_age_kubun.rds")
  set_kubun <- read_rds("setting/ndb_kensa_kubun.rds")
  set_nendo <- read_rds("setting/ndb_nendo.rds")
  set_monsin <- read_rds("setting/ndb_monsin.rds")
  
  help_content <- set_monsin %>% 
    pull(short) 
  
  ui_monsin <- fluidRow(
    column(width=10, pickerInput(ns("monsin"), "問診項目",choices = set_monsin$qnum) ),
    column(width=2, actionBttn(ns("help"), "", icon=icon("info"),
                               style="bordered", size="sm", color="primary"))
  )
  
  
  #ui side panel ----------------------------------
  
  side_panel <- sidebarPanel(
    h2("設定:"),
    pickerInput(ns("seireki"),"西暦年",choices=2014:2017,selected = 2017),
    pickerInput(ns("pref"), "都道府県", choices = set_pref$pref, multiple = TRUE),
    pickerInput(ns("agegrp"),"年齢",choices=set_age$age,selected=1),
    ui_monsin,
    uiOutput(ns("kubun")),
    sliderInput(ns("perc_range"), "グラフの%の表示範囲",min=0, max=100,value=c(0,100),step=1,post="%"),
    hr(),
    downloadButton(ns("dlppt"),"ダウンロード(PPT)"),
    downloadButton(ns("dlexcel"),"ダウンロード(EXCEL)"),
    p("使い方"),
    p("ダウンロード(PPT)で、表示されているグラフを含んだスライドをダウンロードできます。"),
    p("ダウンロード(EXCEL)で、データの表に表示されているテーブルが保存されたエクセルファイルをダウンロードできます。"),
    width=3
  )
  
  #tab panel structure ------------------------------------
  ## each panel -------------------------
  panel1 <- 
    tribble(
      ~posr, ~posc, ~grids, ~height, ~id,      
      1    ,1     ,6      , 800    , "graph_bar" ,
      1    ,2     ,6      , 800    , "graph_line",
    )
  
  panel2 <- 
    tribble(
      ~posr, ~posc, ~grids, ~height, ~id,      
      1    ,1     ,6      , 400    , "graph_bar_male" ,
      1    ,2     ,6      , 400    , "graph_line_male",
      2    ,1     ,6      , 400    , "graph_bar_female" ,
      2    ,2     ,6      , 400    , "graph_line_female",
    )
  
  panel3 <- 
    tribble(
      ~posr, ~posc, ~grids, ~height, ~id,      
      1    ,1     ,6      , 400    , "graph_bar_age" ,
      1    ,2     ,6      , 400    , "graph_line_age"
    )
  
  panel4 <- 
    tribble(
      ~posr, ~posc, ~grids, ~height, ~id,      
      1    ,1     ,12      , 800    , "graph_age_gender"
    )
  
  
  
  ##panel structure---------------------
  panel_tibble <- tribble(
    ~panel_title, ~panel_structure,
    "全都道府県", panel1,
    "性別別", panel2,
    "年齢指定", panel3,
    "性別・年齢別", panel4
  )
  
  tabset_panel <- 
    tabsetPanel(
      tabPanel(
        title = "全都道府県",
        h3(textOutput(ns("title_allpref"))),
        fluidRow(
          column(width=6, withSpinner( plotOutput(ns("graph_bar") ,height=800)) ),
          column(width=6, withSpinner( plotOutput(ns("graph_line"),height=800)) ),
        )
      ),
      tabPanel(
        title = "性別別",
        h3(textOutput( ns("title_gender") )),
        fluidRow(
          column(width=6, plotOutput( ns("graph_bar_male")   ,height=400)),
          column(width=6, plotOutput( ns("graph_line_male")  ,height=400)),
          column(width=6, plotOutput( ns("graph_bar_female") ,height=400)),
          column(width=6, plotOutput( ns("graph_line_female"),height=400)),
        )
      ),
      tabPanel(
        title = "年齢指定",
        h3(textOutput( ns("title_age") )),
        fluidRow(
          column(width=6, plotOutput( ns("graph_bar_age")  ,height=400)),
          column(width=6, plotOutput( ns("graph_line_age") ,height=400)),
        )
      ),
      tabPanel(
        title = "性別・年齢別",
        h3(textOutput( ns("title_age_gender") )),
        fluidRow(
          column(width=12, plotOutput( ns("graph_age_gender"),height=800))
        )
      ),
      tabPanel(
        title = "表",
        h3(textOutput( ns("title_hyou")) ),
        fluidRow(
          column(width=6,
                 checkboxGroupButtons(
                   inputId = ns("groupvar"),
                   label = "集計する変数を指定",
                   choices=c("年度" = "seireki","都道府県" = "pref", "性別"="sex","年齢区分"="age"),
                   select = "都道府県")
          ),
          column(width=12, dataTableOutput(ns("hyou")))
        )
      )
    )
  
  
  
  #ui----------------------------------------
  ui <- fluidPage(
    title = "NDBオープンデータ可視化:特定健診",
    titlePanel("NDBオープンデータ可視化:特定健診検査結果"),
    sidebarLayout(
      sidebarPanel = side_panel,
      mainPanel = mainPanel(tabset_panel)
    )
  )
  
  return(ui)
}      

monsinServer <- function(id){
  moduleServer(
    id,
    function(input,output,session) {
      
      ns <- session$ns
      
      raw_data <- read_rds("data/processed/tokutei_monsin/data.rds")
      set_pref <- read_rds("setting/ndb_prefecture.rds")
      set_age <- read_rds("setting/ndb_age_kubun.rds")
      set_nendo <- read_rds("setting/ndb_nendo.rds")
      set_monsin <- read_rds("setting/ndb_monsin.rds")
      set_kubun <- read_rds("setting/ndb_monsin_answer.rds")
      
      
      #UIを操作するロジック---------------------------
      output$kubun <- renderUI({
        
        kubun_choices <- unique(set_kubun$lab[str_c("q", formatC(as.numeric(input$monsin),width=2,flag=0))])[[1]]
        
        if(length(kubun_choices) == 2){
          ret <- radioGroupButtons(ns("kubun"), "問診の回答", choices=kubun_choices)
        }else{
          ret <- pickerInput(ns("kubun"),"問診の回答:",choices=kubun_choices, selected=kubun_choices[1])
        }
        
        return(ret)
      })
      
      #問診のヘルプを表示する---------------------
      observeEvent(input$help, {
        showModal(modalDialog(
          title = "問診の内容",
          tableOutput(ns("help_table")),
          easyClose=TRUE,
          footer=NULL
        ))
      })
      
      output$help_table <- renderTable({
        #UTF-8の問題で手打ちをする。仕方ない・・・
        #set_monsin %>% select(` ` = short)
        
        tibble(
          `設問` = 1:22,
          `問診内容` = c(
            "内服(血圧)","内服(血糖)","内服(脂質)","既往歴(脳卒中)",
            "既往歴(心臓病)","既往歴(CKD透析)","既往歴(貧血)","喫煙習慣",
            "20歳から10kg増加","汗をかく運動","身体活動","歩く速度",
            "1年で3kg増加","たべる速度","寝る前の食事","夕食後の間食",
            "朝食欠食","飲酒頻度","飲酒量","睡眠での休養",
            "生活習慣の改善意思" ,"保健指導の利用"
          )
        )
        
      })
      
      #データをUIから絞り込んで作成する-----------------------
      dat <- reactive({
        
        dat <- raw_data %>% 
          filter(as.numeric(file_qnum) == as.numeric(input$monsin)) %>% 
          factorize_monsin(., input$monsin, set_kubun) %>% 
          convert_ndb_to_seireki(., set_nendo)
        return(dat)
      })
      
      #タイトル------------------------
      call_title <- reactive({pull_monsin_long(input$monsin, set_monsin)})
      output$title_allpref <- renderText({call_title()})
      output$title_gender <- renderText({call_title()})
      output$title_age <- renderText({call_title()})
      output$title_age_gender <- renderText({call_title()})
      output$title_hyou <- renderText({call_title()})
      
      #棒グラフ-----------------------
      gen_data <- function(.data, ...){
        .data %>% make_monsin_percent( ...)
      }
      
      gen_bar_graph <- function(.gdata,textsize=14){
        
        gt <- .gdata %>% 
          mutate(fill_this = if_else(pref %in% input$pref, pref, NA_character_))
        
        gg <- ggplot(.gdata) +
          geom_col(aes(y = reorder(pref, perc),x=perc), fill="grey80") +
          geom_col(aes(y = reorder(pref, perc),x=perc, fill=fill_this), data=gt) +
          scale_x_continuous(labels=scales::percent) +
          scale_y_discrete(guide = guide_axis(n.dodge=2)) +
          scale_fill_manual(values = rainbow_hcl(length(input$pref)), guide = guide_none()) +
          labs(x = "[%]", y=NULL) +
          coord_cartesian(xlim = input$perc_range/100) +
          theme_bw(base_size = textsize) +
          theme(plot.title.position = "plot")
        
        return(gg)
      }
      
      fun_graph_bar <- function(title, tgtcol=NA, tgtval=NA, textsize=14){
        
        if( is.na(tgtcol) ){
          base_dat <- gen_data(dat(),"seireki","pref")  
        }else{
          enqcol <- rlang::sym(tgtcol)
          base_dat <- gen_data(dat(),"seireki","pref",tgtcol) %>% 
            filter(!!enqcol == tgtval)
        }
        
        gdat <- base_dat %>% 
          filter(answer == input$kubun) %>%
          filter(seireki == input$seireki) 
        
        return(gen_bar_graph(gdat, textsize) + labs(title = title))
      }
      
      output$graph_bar <- renderPlot({
        fun_graph_bar("全都道府県比較")
      })
      
      output$graph_bar_male <- renderPlot({
        fun_graph_bar(
          str_c("全都道府県比較:男性(", input$seireki,"年度)"),
          "sex",
          "男"
        )
      })
      
      output$graph_bar_female <- renderPlot({
        fun_graph_bar(
          str_c("全都道府県比較:女性(", input$seireki,"年度)"),
          "sex",
          "女"
        )
      })
      
      output$graph_bar_age <- renderPlot({
        fun_graph_bar(
          str_c("都道府県比較:年代別[",input$agegrp,"]","(", input$seireki,"年度)"),
          "age",
          input$agegrp
        )
      })
      
      #経年変化グラフ-------------------
      gen_line_graph <- function(.gdata, textsize=14){
        gtrue  <<- .gdata %>% filter(pref %in% input$pref)
        gfalse <<- .gdata %>% filter(!pref %in% input$pref)
        
        ggplot(mapping = aes(x = seireki, y = perc)) +
          geom_line(data = gfalse, aes(group=pref), alpha=0.5) +
          geom_line(data = gtrue , aes(group=pref, color = pref), size=1) +
          geom_point(data = gtrue , aes(group=pref, color = pref),size=3) +
          scale_y_continuous(labels=scales::percent) +
          labs(x = NULL, y = "[%]") +
          coord_cartesian(ylim = input$perc_range/100) +
          scale_color_manual(name="都道府県",values = rainbow_hcl(length(input$pref))) +
          theme_bw(base_size=textsize)
      }
      
      fun_line_graph <- function(title, tgtcol=NA, tgtval=NA, textsize=14){
        if(is.na(tgtcol)){
          base_dat <- gen_data(dat(), "seireki","pref")
        }else{
          enqcol <- rlang::sym(tgtcol)
          base_dat <- gen_data(dat(), "seireki","pref",tgtcol) %>% 
            filter(!!enqcol == tgtval)
        }
        
        gdat <- base_dat %>% filter(answer == input$kubun)
        
        return(
          gen_line_graph(gdat,textsize) +labs(title = title)
        )
      }
      
      output$graph_line <- renderPlot({
        fun_line_graph("全都道府県:経年変化")
      })
      
      output$graph_line_male <- renderPlot({
        fun_line_graph("全都道府県:経年変化(男性)","sex","男")
      })
      
      output$graph_line_female <- renderPlot({
        fun_line_graph("全都道府県:経年変化(女性)","sex","女")
      })
      
      output$graph_line_age <- renderPlot({
        fun_line_graph("全都道府県:経年変化(年代)","age",input$agegrp)
      })
      
      fun_age_gender_graph <- function(textsize=14){
        
        gdat <- gen_data(dat(), "seireki", "pref", "age","sex") %>% 
          filter(answer == input$kubun)
        
        gpref  <- gdat %>% filter(pref %in% input$pref)
        gage   <- gpref %>% filter(age == input$agegrp)
        
        if(length(input$pref)==0){
          gg <- ggplot()+geom_text(aes(x=1,y=1,label="都道府県を選択してください"))
        }else{
          gg <- ggplot() +
            geom_line(data = gpref , aes(x = seireki, y = perc, group = age, color=age), size=1) +
            geom_point(data=gage, aes(x = seireki, y = perc, color = age), size = 3) +
            facet_wrap(sex~pref) +
            scale_y_continuous(labels=scales::percent) +
            scale_color_discrete(name="年代") +
            theme_bw(base_size=textsize)  
        }
        
        return(gg)
      }
      
      output$graph_age_gender <- renderPlot({
        fun_age_gender_graph()    
      })
      
      #表の描画部分---------------------------
      hyou_data <- reactive({
        if(length(input$groupvar) > 0){
          hyou <- gen_data(dat(), input$groupvar) %>% 
            rename("NDB集計値" = value, "集計変数の総数" = total,　"割合" = perc)  
        }else{
          hyou <- tibble(ERROR = "何か集計する変数を指定してください")
        }
        names(hyou)[names(hyou)=="sex"] <- "性別"
        names(hyou)[names(hyou)=="age"] <- "年齢区分"
        names(hyou)[names(hyou)=="answer"] <- "問診回答"
        
        return(hyou)
      })
      
      output$hyou <- renderDataTable({
        hyou_data()
      })
      
      #ダウンロードロジック------------------------
      ## 表------------------------------
      output$dlexcel <- downloadHandler(
        filename = function(){
          gv <- input$groupvar
          
          gv[gv == "都道府県"] <- "pref"
          
          txt_gv <- str_c(gv,collapse="_")
          
          fn <- str_c("table","_",Sys.Date(),"_q", input$monsin,"_", txt_gv,".xlsx")
          
          return(fn)  
        },
        content = function(file){
          
          wb <- createWorkbook()
          addWorksheet(wb,"table")
          writeData(wb,"table",hyou_data())
          saveWorkbook(wb,file=file)
        }
      )
      
      ## PPT---------------------------
      output$dlppt <- downloadHandler(
        filename = function(){
          c(
            input$seireki,
            str_c("q",input$monsin,collapse=""),
            input$kubun,
            str_c(str_remove(input$pref,"県$|府$|都$"),collapse="_"),
            input$agegrp
          ) %>% str_c(collapse="_") %>% 
            str_c(.,".pptx") %>% 
            return()
        },
        content = function(file){
          ppttextsize <- 36
          
          g1 <- fun_graph_bar("全都道府県比較",textsize = ppttextsize)
          g2 <- fun_line_graph("全都道府県:経年変化",textsize = ppttextsize)
          g3 <- fun_graph_bar(str_c("全都道府県比較:男性(", input$seireki,"年度)"),"sex","男",textsize = ppttextsize)
          g4 <- fun_line_graph("全都道府県:経年変化(男性)","sex","男",textsize = ppttextsize)
          g5 <- fun_graph_bar(str_c("全都道府県比較:女性(", input$seireki,"年度)"),"sex","女",textsize = ppttextsize)
          g6 <- fun_line_graph("全都道府県:経年変化(女性)","sex","女",textsize = ppttextsize)
          g7 <- fun_graph_bar(str_c("都道府県比較:年代別[",input$agegrp,"]","(", input$seireki,"年度)"),"age",input$agegrp,textsize = ppttextsize)
          g8 <- fun_line_graph("全都道府県:経年変化(年代)","age",input$agegrp,textsize = ppttextsize)
          g9 <- fun_age_gender_graph(textsize = ppttextsize)
          
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
