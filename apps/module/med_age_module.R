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
library(plotly)
library(cowplot)
library(sf)

showtext_auto()

med_age_UI <- function(id){
  ns <- NS(id)
  #data and setting file------------------------
  
  #uis---------------
  ui_nendo <- sliderInput(ns("nendo"),"年度を選択",min=2014, max=2018, step=1, sep="",value=2018)
  ui_yj_keta <- sliderInput(ns("keta"), "同時に表示する医薬品のYJコードの桁数を指定", min=7,max=9,step=1,value=9)
  
  
  #ui side panel ----------------------------------
  side_panel <- sidebarPanel(
    ui_nendo,
    ui_yj_keta,
    tags$b("医薬品を選択"),
    DT::dataTableOutput(ns("iyakuhin")),
    hr(),
    downloadButton(ns("dlppt"),"ダウンロード(PPT)"),
    downloadButton(ns("dlexcel"),"ダウンロード(EXCEL)"),
    p("使い方"),
    p("ダウンロード(PPT)で、表示されているグラフを含んだスライドをダウンロードできます。"),
    p("ダウンロード(EXCEL)で、都道府県別ランキングが保存されたエクセルファイルをダウンロードできます。"),
    width=3
  )
  
  ui_meds <- fluidRow(
    column(width=3, radioGroupButtons(
      inputId = ns("med_type"),
      label = "医薬品種類 :", 
      choices = c("内服","外用","注射"), 
      justified = TRUE
    )),
    column(width=3, radioGroupButtons(
      inputId = ns("in_out"),
      label = "外来/入院:",
      choices = c("外来","入院"),
      justified = TRUE
    )),
    column(width=3, radioGroupButtons(
      inputId = ns("prescription_in_out"),
      label = "院内/院外:",
      choices = c("院内","院外"),
      justified = TRUE
    ))
  )
  
  #tab panel structure ------------------------------------
  tabset_panel <- 
    tabsetPanel(
      tabPanel(
        title = "男性(処方回数)",
        h3("男性への医薬品処方回数:"),
        fluidRow(column(width=12, withSpinner( plotOutput( ns("graph_male_comparison"),height=800)))),
        fluidRow(column(width=12, withSpinner( plotOutput( ns("graph_male_change")    ,height=800))))
      ),
      tabPanel(
        title = "女性(処方回数)",
        h3("女性への医薬品処方回数:"),
        fluidRow(column(width=12, withSpinner( plotOutput( ns("graph_female_comparison"),height=800)))),
        fluidRow(column(width=12, withSpinner( plotOutput( ns("graph_female_change")    ,height=800))))
      ),
      tabPanel(
        title = "年齢別(処方回数)",
        h3("年齢別の医薬品処方回数:"),
        #ui_meds,
        fluidRow(column(width=12, withSpinner( plotOutput( ns("graph_age_comparison"),height=800)))),
        fluidRow(column(width=12, withSpinner( plotOutput( ns("graph_age_change")    ,height=800))))
      ),
      tabPanel(
        title = "男性：年齢別(処方回数)",
        h3("男性の年齢別の医薬品処方回数:"),
        fluidRow(column(width=12, withSpinner( plotOutput( ns("graph_agemale_comparison"),height=800)))),
        fluidRow(column(width=12, withSpinner( plotOutput( ns("graph_agemale_change")    ,height=800))))
      ),
      tabPanel(
        title = "女性：年齢別(処方回数)",
        h3("女性の年齢別の医薬品処方回数:"),
        fluidRow(column(width=12, withSpinner( plotOutput( ns("graph_agefemale_comparison"),height=800)))),
        fluidRow(column(width=12, withSpinner( plotOutput( ns("graph_agefemale_change")    ,height=800))))
      ),
      tabPanel(
        title = "ランキング",
        h3("年齢別のランキング"),
        fluidRow(column(width = 12, withSpinner( tableOutput( ns("ranking_age") )))),
        hr(),
        h3("年齢別：男性のランキング"),
        fluidRow(column(width = 12, withSpinner( tableOutput( ns("ranking_agemale"))))),
        hr(),
        h3("年齢別：女性のランキング"),
        fluidRow(column(width = 12, withSpinner( tableOutput( ns("ranking_agefemale")))))
      )
    )
  
  
  #ui----------------------------------------
  ui <- fluidPage(
    title = "NDBオープンデータ可視化:医薬品",
    titlePanel("NDBオープンデータ可視化:医薬品(性別・年齢別)"),
    sidebarLayout(
      sidebarPanel = side_panel,
      mainPanel = mainPanel(
        ui_meds,
        tabset_panel
      )
    )
  )
  
}


med_age_Server <- function(id){
  moduleServer(
    id,
    function(input,output,session) {
      ns <- session$ns
      
      age_data         <- read_rds("data/processed/medicine/age_data_light.rds")
      med_tensu_master <- read_rds("setting/iyakuhin_tensu_master.rds")
      med_master       <- read_rds("setting/iyakuhin_master.rds")
      set_nendo        <- read_rds("setting/meidicine_nendo.rds")
      
      warning(str_c("med_tensu_master", med_tensu_master %>% slice(1:2)))
      
      #医薬品の選択UI用DT-----------------------------------
      output$iyakuhin <- DT::renderDataTable(
        {med_master},
        server = TRUE,
        extensions='Scroller',
        selection = 'single',
        options=list(
          deferRender=TRUE,
          scrollY = 200,
          scroller = TRUE
        )
      )
     
      #グラフ用フィルターデータ--------------------------------
      target_med <- reactive({
        if(!is.null(input$iyakuhin_rows_selected)){
          tgt_med <- med_master %>% slice(input$iyakuhin_rows_selected)
        }else{
          tgt_med <- med_master %>% slice(0)
        }
        
        warning(str_c("from target_med()",tgt_med))
        
        return(tgt_med)
      })
      
      similar_yj_med <- reactive({
        if(nrow(target_med()) == 0){
          return(tibble())
        }else{
          tgt_yj_keta <- target_med()$yj_code %>% str_sub(.,1,input$keta)
          
          res <- med_tensu_master %>% filter(str_detect(yj_code,str_c("^",tgt_yj_keta)))
          
          res <- res %>% 
            left_join(set_nendo, by="ndb") %>% 
            mutate(yakka = as.numeric(yakka))
          
          warning("from: similar_yj_med")
          warning(str_c("smilar_yj_med:",res %>% slice(1:2)))
          return(res)
        }
        
      })
      
      dat_value <- reactive({
        
        dat <- pref_data %>% 
          semi_join(target_med(), by = c("iyakuhin_code","yj_code"))
        
        dat2 <- dat %>% 
          left_join(med_tensu_master, by = c("ndb","med_type","iyakuhin_code","yj_code")) %>% 
          left_join(set_nendo, by = c("ndb"))
        
        dat3 <- dat2 %>% 
          mutate(nendo = as.character(nendo))
        
        if(nrow(dat) != nrow(dat2)){warning("filtering Warning!")}
        
        warning("FROM:dat_value()")
        warning(dat3 %>% slice(2))
        
        return(dat3)
      })
      
      dat_value_filtered <- reactive({
        dat <- dat_value()
        warning("from dat_value_filtered()")
        warning(dat %>% slice(1:2))
        res <- dat %>% 
          filter(
            nendo == input$nendo,
            med_type == input$med_type, 
            inout    == input$in_out, 
            out_ishp == input$prescription_in_out)
        
        return(res)
      })

      dat_similar <- reactive({
        sim_med <- similar_yj_med()
        
        warning(str_c("1)", sim_med))
        
        if(nrow(sim_med)==0){
          res <- tibble()
        }else{
          warning(str_c("2)",age_data %>% slice(1:2)))
          dat <- age_data %>% 
            semi_join(sim_med, by = c("iyakuhin_code","yj_code"))
          warning(str_c("3)",dat %>% slice(1:2)))
          dat2 <- dat %>% 
            left_join(med_tensu_master, by = c("ndb","med_type","iyakuhin_code","yj_code")) %>% 
            left_join(set_nendo, by = c("ndb"))
          warning(str_c("4)",dat2 %>% slice(1:2)))
          dat3 <- dat2 %>% 
            mutate(nendo = as.character(nendo))
          warning(str_c("4)",dat3 %>% slice(1:2)))
          if(nrow(dat) != nrow(dat2)){warning("filtering Warning!")}
          
          res <- dat3 %>% 
            filter(
              med_type == input$med_type, 
              inout    == input$in_out, 
              out_ishp == input$prescription_in_out)
          
          warning(str_c("dat_similar():", res %>% slice(1:2)))
          
          
          
        }
        
        return(res)
      })
      
      dat_similar_by_gender <- reactive({
        res <- dat_similar() %>% 
          group_by(ndb, med_type, inout, out_ishp, iyakuhin_code, yj_code, sex, 
                   yakko_bunrui, iyakuhin_name, unit, yakka, is_generic, nendo) %>%
          summarise(value = sum(value, na.rm=TRUE)) %>% 
          ungroup()
        warning(str_c("dat_similar_by_gender", res %>% slice(1:2)))
        return(res)
      })
      
      dat_similar_by_age <- reactive({
        res <- dat_similar() %>% 
          group_by(ndb, med_type, inout, out_ishp, iyakuhin_code, yj_code, age, 
                   yakko_bunrui, iyakuhin_name, unit, yakka, is_generic, nendo) %>%
          summarise(value = sum(value, na.rm=TRUE)) %>% 
          ungroup()
        
        return(res)
      })
      
      dat_similar_by_gender_age <- reactive({
        res <- dat_similar() %>% 
          group_by(ndb, med_type, inout, out_ishp, iyakuhin_code, yj_code, sex, age, 
                   yakko_bunrui, iyakuhin_name, unit, yakka, is_generic, nendo) %>%
          summarise(value = sum(value, na.rm=TRUE)) %>% 
          ungroup()
        
        return(res)
      })

      #graph 作成---------------------
      gen_no_data_plot <- function(title_set,fontsize=14){
        ggplot() +
          geom_text(aes(1,1,label="医薬品を選択してください。")) +
          labs(title = title_set) +
          theme_void(base_size=fontsize)
      }
      
      gen_title_by_med_types <- function(mt,io,pio){
        if(mt == "注射"){
          res <- mt
        }else{
          if(io == "入院"){
            res <- str_c(mt,"(",io,")")
          }else{
            res <- str_c(mt,"(", io, ":", pio,")")
          }
        }
        return(res)
      }
      
      gen_graph_gender_comparison <- function(.gdat, tgt_nendo, tgt_gender, graph_font_size){
        gdat <- .gdat %>% mutate(med_id = str_c(iyakuhin_code,":",iyakuhin_name))
        
        title_text <- str_c(
          target_med()$yj_code, ":",target_med()$iyakuhin_name,
          gen_title_by_med_types(input$med_type, input$in_out, input$prescription_in_out)
        )
        
        if(nrow(.gdat)==0){
          gg <- gen_no_data_plot(
            str_c(tgt_nendo,"年度、",tgt_gender,"への処方回数比較\n医薬品を選択してください"),
            graph_font_size
          )
        }else{
          gg <- ggplot(gdat) + 
            geom_col(aes(x = value, y = reorder(med_id, value), fill=color_this)) +
            scale_x_continuous(labels = scales::comma) +
            scale_fill_manual(values = rainbow_hcl(2), guide = guide_none()) +
            labs(x = "処方回数", y = "医薬品コード:医薬品名",
                 title = str_c(tgt_nendo,"年度、",tgt_gender,"への処方回数比較\n",title_text)) +
            theme_bw(base_size = graph_font_size) +
            theme(plot.title.position = "plot")  
        }

        return(gg)
      }
      
      gen_graph_gender_change <- function(.gdat, tgt_gender, graph_font_size){
        gdat <- .gdat %>% mutate(med_id = str_c(iyakuhin_code,":",iyakuhin_name))
        
        title_text <- str_c(
          target_med()$yj_code, ":",target_med()$iyakuhin_name,
          gen_title_by_med_types(input$med_type, input$in_out, input$prescription_in_out)
        )
        
        if(nrow(.gdat)==0){
          gg <- gen_no_data_plot(
            str_c(tgt_nendo,"年度、",tgt_gender,"への処方回数比較\n医薬品を選択してください"),
            graph_font_size
          )
        }else{
          gg <- ggplot(gdat) + 
            geom_point(aes(x = nendo, y = value, color = med_id, shape = color_this), size=4) +
            geom_line(aes(x = nendo,y = value, color = med_id, group = med_id), size=1) +
            scale_y_continuous(labels = scales::comma) +
            scale_color_discrete(name = NULL) +
            scale_shape_discrete(guide = guide_none()) +
            labs(x = "年度", y = "処方回数",
                 title = str_c(tgt_gender,"への処方回数の経年変化\n",title_text)) +
            theme_bw(base_size = graph_font_size) +
            theme(plot.title.position = "plot")
        }
        
        return(gg)
      }
      #outputs:graph_gender_comparison-----------------------------
      output$graph_male_comparison <- renderPlot({
        
        color_this_med <- target_med() %>% pull(iyakuhin_code)
        
        gdat <- dat_similar_by_gender()
        gdat2 <- gdat %>% 
          filter(sex == "男") %>% 
          filter(nendo == input$nendo) %>% 
          mutate(color_this = iyakuhin_code == color_this_med)
        
        gen_graph_gender_comparison(gdat2, input$nendo, "男性", 18)
      })
      
      output$graph_female_comparison <- renderPlot({
        color_this_med <- target_med() %>% pull(iyakuhin_code)
        
        gdat <- dat_similar_by_gender()
        gdat2 <- gdat %>% 
          filter(sex == "女") %>% 
          filter(nendo == input$nendo) %>% 
          mutate(color_this = iyakuhin_code == color_this_med)
        
        gen_graph_gender_comparison(gdat2, input$nendo, "女性", 18)
      })
      
      #outputs:graph_gender_change--------------------------
      output$graph_male_change <- renderPlot({
        color_this_med <- target_med() %>% pull(iyakuhin_code)
        
        gdat <- dat_similar_by_gender()
        gdat2 <- gdat %>% 
          filter(sex == "男") %>%
          mutate(color_this = iyakuhin_code == color_this_med)
        
        gen_graph_gender_change(gdat2,  "男性", 18)
      })
      
      output$graph_female_change <- renderPlot({
        color_this_med <- target_med() %>% pull(iyakuhin_code)
        
        gdat <- dat_similar_by_gender()
        gdat2 <- gdat %>% 
          filter(sex == "女") %>%
          mutate(color_this = iyakuhin_code == color_this_med)
        
        gen_graph_gender_change(gdat2,  "女性", 18)
      })
      
      #outputs:graph_age_comparison---------------------------
      #outputs:graph_agemale_comparison---------------------------
      #outputs:graph_agefemale_comparison---------------------------
      gen_graph_age_comparison <- function(.gdat, tgtnendo, graph_font_size,extra_title_text=""){
        
        title_text <- str_c(
          target_med()$yj_code, ":",target_med()$iyakuhin_name,
          gen_title_by_med_types(input$med_type, input$in_out, input$prescription_in_out)
        )
        
        tgt_med_data     <- .gdat %>% filter(color_this)
        non_tgt_med_data <- .gdat %>% filter(!color_this)
        
        gg <- ggplot() +
          geom_point(aes(x = age, y = value), color = "skyblue", data=tgt_med_data, size = 3) +
          geom_point(aes(x = age, y = value), color = "grey75",data = non_tgt_med_data) +
          geom_line(aes(x = age, y = value, group=iyakuhin_code), color = "skyblue", data = tgt_med_data, size =1) +
          geom_line(aes(x = age, y = value, group=iyakuhin_code), color = "grey75", data = non_tgt_med_data) +
          scale_x_discrete(name=NULL,guide = guide_axis(angle = 90)) +
          scale_y_continuous(name="処方回数",labels=scales::comma) +
          labs(title = str_c(title_text,"\n",extra_title_text,"処方回数の年齢別分布")) +
          theme_bw(base_size=graph_font_size)
        
        return(gg)
      }
      
      output$graph_age_comparison <- renderPlot({
        color_this_med <- target_med() %>% pull(iyakuhin_code)
        
        gdat <- dat_similar_by_age()
        gdat2 <- gdat %>%
          filter(nendo == input$nendo) %>% 
          mutate(color_this = iyakuhin_code == color_this_med)
        
        gen_graph_age_comparison(gdat2, input$nendo, 12,"")
      })
      
      output$graph_agemale_comparison <- renderPlot({
        color_this_med <- target_med() %>% pull(iyakuhin_code)
        
        gdat <- dat_similar_by_gender_age()
        gdat2 <- gdat %>% 
          filter(nendo == input$nendo) %>% 
          filter(sex == "男") %>% 
          mutate(color_this = iyakuhin_code == color_this_med)
        
        gen_graph_age_comparison(gdat2, input$nendo, 12,"男性の")
      })
      
      output$graph_agefemale_comparison <- renderPlot({
        color_this_med <- target_med() %>% pull(iyakuhin_code)
        
        gdat <- dat_similar_by_gender_age()
        gdat2 <- gdat %>% 
          filter(nendo == input$nendo) %>% 
          filter(sex == "女") %>% 
          mutate(color_this = iyakuhin_code == color_this_med)
        
        gen_graph_age_comparison(gdat2, input$nendo, 12,"女性の")
      })
      
      
      #outputs:graph_age_change---------------------------
      #outputs:graph_agemale_change---------------------------
      #outputs:graph_agefemale_change---------------------------
      gen_graph_age_change <- function(.gdat, graph_font_size, extra_title_text=""){
        title_text <- str_c(
          target_med()$yj_code, ":",target_med()$iyakuhin_name,
          gen_title_by_med_types(input$med_type, input$in_out, input$prescription_in_out)
        )
        
        gg <- ggplot(.gdat) +
          geom_col(aes(x = age, y = value), position="dodge", fill="skyblue") +
          scale_x_discrete(guide = guide_axis(angle = 90)) +
          scale_y_continuous(labels = scales::comma) +
          labs(x = NULL, y = "処方回数", 
               title = str_c(title_text,"\n",extra_title_text,"処方回数の年齢別分布の経年変化")) +
          facet_wrap(~nendo) +
          theme_bw(base_size = graph_font_size)
        
        return(gg)
      }
      
      output$graph_age_change <- renderPlot({
        color_this_med <- target_med() %>% pull(iyakuhin_code)
        
        gdat <- dat_similar_by_gender_age()
        gdat2 <- gdat %>% 
          filter(iyakuhin_code == color_this_med) %>% 
          filter(age != "90歳以上")
      
        gen_graph_age_change(gdat2, 12,"")
      })
      
      output$graph_agemale_change <- renderPlot({
        color_this_med <- target_med() %>% pull(iyakuhin_code)
        
        gdat <- dat_similar_by_gender_age()
        gdat2 <- gdat %>% 
          filter(sex == "男") %>% 
          filter(iyakuhin_code == color_this_med) %>% 
          filter(age != "90歳以上")
        
        gen_graph_age_change(gdat2,12,"男性の")
      })
      
      output$graph_agefemale_change <- renderPlot({
        color_this_med <- target_med() %>% pull(iyakuhin_code)
        
        gdat <- dat_similar_by_gender_age()
        gdat2 <- gdat %>% 
          filter(sex == "女") %>% 
          filter(iyakuhin_code == color_this_med) %>% 
          filter(age != "90歳以上")
        
        gen_graph_age_change(gdat2, 12,"女性の")
      })
      
      gen_rank_table <- function(dat, tgt_med, add_html=TRUE){
        
        dat2 <- dat %>% 
          filter(nendo == input$nendo)
        
        hh <- dat2 %>% 
          rowwise() %>% 
          mutate(is_selected_tgt = all(
            iyakuhin_code == tgt_med$iyakuhin_code,
            yj_code == tgt_med$yj_code,
            iyakuhin_name == tgt_med$iyakuhin_name
          )) %>% 
          ungroup()
        
        if(add_html){
          hh <- hh %>% 
            mutate(iyakuhin = str_c(iyakuhin_code,":<br>",iyakuhin_name,"<br>",value)) %>% 
            mutate(iyakuhin = if_else(is_selected_tgt, str_c("<strong>",iyakuhin,"</strong>"),iyakuhin))
        }else{
          hh <- hh %>% 
            mutate(iyakuhin = str_c(iyakuhin_code,":", iyakuhin_name, ":",value))
        }
        
        hh <- hh %>% 
          select(age, iyakuhin,value) %>% 
          group_by(age) %>% 
          arrange(value) %>% 
          filter(!is.na(value)) %>% 
          mutate(rank = rank(rev(value), ties.method = "first"))
        
        hh2 <- hh %>% 
          filter(rank <= 3) %>% 
          select(age, iyakuhin, rank)
        
        hh3 <- hh2 %>% 
          pivot_wider(id_cols = age, names_from = rank, values_from = iyakuhin) %>% 
          select(`年齢区分` = age, `1位` = `1`, `2位` = `2`, `3位` = `3`) %>% 
          ungroup() %>% 
          arrange(`年齢区分`)
        
        return(hh3)
      }
      
      output$ranking_age <- renderTable({
        gen_rank_table(
          dat = dat_similar_by_age(), 
          tgt_med = target_med()
        )
      },bordered = TRUE, sanitize.text.function=identity)
      
      output$ranking_agemale <- renderTable({
        gen_rank_table(
          dat = dat_similar_by_gender_age() %>% filter(sex == "男"), 
          tgt_med = target_med()
        )
      },bordered = TRUE, sanitize.text.function=identity)
      
      output$ranking_agefemale <- renderTable({
        
        gen_rank_table(
          dat = dat_similar_by_gender_age() %>% filter(sex == "女"), 
          tgt_med = target_med()
        )
      },bordered = TRUE, sanitize.text.function=identity)
      
      
      #ダウンロードロジック------------------------
      ## 表------------------------------
      output$dlexcel <- downloadHandler(
        filename = function(){
          
          tmed <- target_med()
          
          nam <- tmed$iyakuhin_name
          yjc <- tmed$yj_code
          
          fn <- str_c("table_age_gender_",nam,"_",Sys.Date(),"_", yjc,".xlsx")
          
          return(fn)  
        },
        content = function(file){
          
          dat_sim <- dat_similar()
          if(nrow(dat_sim)==0){
            hyou_age       <- tibble("医薬品を選択")
            hyou_agemale   <- tibble("医薬品を選択")
            hyou_agefemale <- tibble("医薬品を選択")
          }else{
            hyou_age <- gen_rank_table(
              dat = dat_similar_by_age(), 
              tgt_med = target_med(),
              add_html = FALSE
            )
            
            hyou_agemale <- gen_rank_table(
              dat = dat_similar_by_gender_age() %>% filter(sex == "男"), 
              tgt_med = target_med(),
              add_html = FALSE
            )
            
            hyou_agefemale <- gen_rank_table(
              dat = dat_similar_by_gender_age() %>% filter(sex == "女"), 
              tgt_med = target_med(),
              add_html = FALSE
            )

            
          }
          
          wb <- createWorkbook()
          addWorksheet(wb,"table_age")
          addWorksheet(wb,"table_age_male")
          addWorksheet(wb,"table_age_female")
          writeData(wb,"table_age"     ,hyou_age)
          writeData(wb,"table_age_male",hyou_agemale)
          writeData(wb,"table_age_female"  ,hyou_agefemale)
          saveWorkbook(wb,file=file)
          
        }
      )
      
      ## PPT---------------------------
      output$dlppt <- downloadHandler(
        filename = function(){
          
          tmed <- target_med()
          
          nam <- tmed$iyakuhin_name
          yjc <- tmed$yj_code
          
          fn <- str_c("ppt_age_gender",nam,"_",Sys.Date(),"_", yjc,".pptx")
          
          return(fn)  
        },
        content = function(file){
          
          ppfont_size = 36
          tmed <- target_med()
          
          nam <- tmed$iyakuhin_name
          yjc <- tmed$yj_code
          yr <- input$nendo
          color_this_med <- target_med() %>% pull(iyakuhin_code)
          gdat_gender <- dat_similar_by_gender()
          
          gdat_comparison_male <- gdat_gender %>% 
            filter(sex == "男") %>% 
            filter(nendo == input$nendo) %>% 
            mutate(color_this = iyakuhin_code == color_this_med)
          
          gdat_comparison_female <- gdat_gender %>% 
            filter(sex == "女") %>% 
            filter(nendo == input$nendo) %>% 
            mutate(color_this = iyakuhin_code == color_this_med)
          
          gdat_male_change <- gdat_gender %>% 
            filter(sex == "男") %>%
            mutate(color_this = iyakuhin_code == color_this_med)
          
          gdat_female_change <- gdat_gender %>% 
            filter(sex == "女") %>%
            mutate(color_this = iyakuhin_code == color_this_med)
          
          
          g1 <- gen_graph_gender_comparison(gdat_comparison_male, input$nendo, "男性", ppfont_size)
          g2 <- gen_graph_gender_comparison(gdat_comparison_female, input$nendo, "女性", ppfont_size)
          g3 <- gen_graph_gender_change(gdat_male_change  , "男性", ppfont_size)
          g4 <- gen_graph_gender_change(gdat_female_change, "女性", ppfont_size)
          
          gdat_age <- dat_similar_by_age()
          
          gdat_comparison_age <- gdat_age %>%
            filter(nendo == input$nendo) %>% 
            mutate(color_this = iyakuhin_code == color_this_med)
          
          g5 <- gen_graph_age_comparison(gdat_comparison_age, input$nendo, ppfont_size,"")
          
          gdat_age_gender <- dat_similar_by_gender_age()
          
          gdat_comparison_agemale <- gdat_age_gender %>% 
            filter(nendo == input$nendo) %>% 
            filter(sex == "男") %>% 
            mutate(color_this = iyakuhin_code == color_this_med)
          
          gdat_comparison_agefemale <- gdat_age_gender %>% 
            filter(nendo == input$nendo) %>% 
            filter(sex == "女") %>% 
            mutate(color_this = iyakuhin_code == color_this_med)
          
          gdat_age_change <- gdat_age_gender %>% 
            filter(iyakuhin_code == color_this_med) %>% 
            filter(age != "90歳以上")
          
          g6 <- gen_graph_age_comparison(gdat_comparison_agemale, input$nendo, ppfont_size,"男性の")
          g7 <- gen_graph_age_comparison(gdat_comparison_agefemale, input$nendo, ppfont_size,"女性の")
          g8 <- gen_graph_age_change(gdat_age_change, ppfont_size,"")
          
          
          gdat_agemale_change <- gdat_age_gender %>% 
            filter(sex == "男") %>% 
            filter(iyakuhin_code == color_this_med) %>% 
            filter(age != "90歳以上")
          
          gdat_agefemale_change <- gdat_age_gender %>% 
            filter(sex == "女") %>% 
            filter(iyakuhin_code == color_this_med) %>% 
            filter(age != "90歳以上")
          
          g9 <- gen_graph_age_change(gdat_agemale_change,ppfont_size,"男性の")
          g10<- gen_graph_age_change(gdat_agefemale_change,ppfont_size,"男性の")
          
          
          title_string <- str_c(yjc,":\n",nam,"\n",yr,"年度の年齢性別別集計結果")
          
          ppt <- read_pptx() %>% 
            add_slide(layout = "Title Slide") %>% 
            ph_with(value = title_string, location = ph_location_type(type="ctrTitle")) %>% 
            add_slide() %>% ph_with(value=g1, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g2, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g3, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g4, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g5, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g6, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g7, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g8, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g9, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g10, location=ph_location_fullsize()) 
          
          print(ppt, file)
        }
      )
    }
  )
}
