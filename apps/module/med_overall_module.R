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

showtext_auto()

med_overall_UI <- function(id){
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
    p("ダウンロード(EXCEL)で、データの表に表示されているテーブルが保存されたエクセルファイルをダウンロードできます。"),
    width=4
  )
  
  #tab panel structure ------------------------------------
  tabset_panel <- 
    tabsetPanel(
      tabPanel(
        title = "経年変化(処方回数)",
        h3("処方回数:"),
        fluidRow(
          column(width=12, withSpinner( plotOutput( ns("graph_total"),height=800)) )
        )
      ),
      tabPanel(
        title = "経年変化(薬価)",
        h3("薬価の変化:"),
        p("赤線が選択した医薬品、棒グラフはYJコードで推計した同一薬効の医薬品。"),
        fluidRow(
          column(width=12, withSpinner( plotOutput(ns("graph_tensu"), height=800)))
        )
      ),
      tabPanel(
        title = "ランキング(内服)",
        fluidRow(column(width=12, withSpinner( plotOutput( ns("rank_naihuku")  ,height=800))))
      ),
      tabPanel(
        title = "ランキング(外用)",
        fluidRow(column(width=12, withSpinner( plotOutput( ns("rank_gaiyou")  ,height=800))))
      ),
      tabPanel(
        title = "ランキング(注射)",
        fluidRow(column(width=12, withSpinner( plotOutput( ns("rank_injection")  ,height=800))))
      ),
      tabPanel(
        title = "表",
        fluidRow(
          column(width=12, dataTableOutput(ns("hyou")))
        )
      )
    )
  
  
  #ui----------------------------------------
  ui <- fluidPage(
    title = "NDBオープンデータ可視化:医薬品",
    titlePanel("NDBオープンデータ可視化:医薬品(総計)"),
    sidebarLayout(
      sidebarPanel = side_panel,
      mainPanel = mainPanel(tabset_panel)
    )
  )
  
}


med_overall_Server <- function(id){
  moduleServer(
    id,
    function(input,output,session) {
      ns <- session$ns
      
      oa_data <- read_rds("data/processed/medicine/overall.rds")
      med_tensu_master <- read_rds("setting/iyakuhin_tensu_master.rds")
      med_master     <- read_rds("setting/iyakuhin_master.rds")
      set_nendo <- read_rds("setting/meidicine_nendo.rds")
      
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
        
        return(tgt_med)
      })
      
      similar_yj_med <- reactive({
        tgt_yj_keta <- target_med()$yj_code %>% str_sub(.,1,input$keta)
        similar_yj_med <- med_tensu_master %>% filter(str_detect(yj_code,str_c("^",tgt_yj_keta)))
        
        similar_yj_med <- similar_yj_med %>% 
          left_join(set_nendo, by="ndb") %>% 
          mutate(yakka = as.numeric(yakka))
        return(similar_yj_med)
      })
      
      dat_value <- reactive({

        dat <- oa_data %>% 
          semi_join(target_med(), by = c("iyakuhin_code","yj_code"))
        
        dat2 <- dat %>% 
          left_join(med_tensu_master, by = c("ndb","med_type","iyakuhin_code","yj_code")) %>% 
          left_join(set_nendo, by = c("ndb"))
        
        dat3 <- dat2 %>% 
          mutate(overall_total = as.numeric(overall_total)) %>% 
          mutate(nendo = as.character(nendo))
        
        if(nrow(dat) != nrow(dat2)){warning("filtering Warning!")}
        
        return(dat3)
      })
      
      dat_rank <- reactive({
        
        dat <- oa_data %>% 
          semi_join(similar_yj_med(), by = c("iyakuhin_code","yj_code"))
        
        dat2 <- dat %>% 
          left_join(med_tensu_master, 
                    by = c("ndb","yakko_bunrui","unit","med_type",
                           "iyakuhin_code","yj_code","iyakuhin_name")) %>% 
          left_join(set_nendo, by = c("ndb"))
        
        dat3 <- dat2 %>% 
          mutate(overall_total = as.numeric(overall_total)) %>% 
          mutate(nendo = as.character(nendo))
        
        if(nrow(dat) != nrow(dat2)){warning("filtering Warning!")}
        
        return(dat3)
      })

      gen_no_data_plot <- function(title_set,fontsize=14){
        ggplot() +
          geom_text(aes(1,1,label="医薬品を選択してください。")) +
          labs(title = title_set) +
          theme_void(base_size=fontsize)
      }
      
      gen_graph_total <- function(.gdat, tgt_med, fontsize = 18){
        gdat   <- .gdat
        tgt_med <- tgt_med
        
        naihuku_gairai <- gdat %>% filter(med_type == "内服", inout == "外来")
        naihuku_nyuin  <- gdat %>% filter(med_type == "内服", inout == "入院")
        gaiyou_gairai  <- gdat %>% filter(med_type == "外用", inout == "外来")
        gaiyou_nyuin   <- gdat %>% filter(med_type == "外用", inout == "入院")
        inject_gairai  <- gdat %>% filter(med_type == "注射", inout == "外来")
        inject_nyuin   <- gdat %>% filter(med_type == "注射", inout == "入院")
        
        
        gen_plot_total_nyuin <- function(.data, title_set){
          gg <- ggplot(.data) +
            geom_col(aes(x = nendo, y = overall_total), fill = rainbow_hcl(3)[3]) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = NULL, y = "処方数", title = title_set) +
            theme_bw(base_size =fontsize)
          return(gg)
        }
        
        gen_plot_total_gairai <- function(.data, title_set){
          gg <- ggplot(.data) +
            geom_col(aes(x = nendo, y = overall_total, fill = out_ishp), position = "stack") +
            scale_y_continuous(labels = scales::comma) +
            scale_fill_manual(name = NULL,values = rainbow_hcl(2)) +
            labs(x = NULL, y = "処方数", title = title_set) +
            theme_bw(base_size =fontsize)
          return(gg)
        }
        
        gen_no_data_plot <- function(title_set){
          ggplot() +
            geom_text(aes(1,1,label="データがありません")) +
            labs(title = title_set) +
            theme_void(base_size=fontsize)
        }
        
        g_leg <- NULL
        
        #内服　外来
        if(nrow(naihuku_gairai)==0){
          g_naihuku_gairai<- gen_no_data_plot("内服(外来)")
        }else{
          g_naihuku_gairai <- gen_plot_total_gairai(naihuku_gairai,"内服(外来)")
          g_leg <- get_legend(g_naihuku_gairai)
          g_naihuku_gairai <- g_naihuku_gairai + theme(legend.position = "none")
        }
        
        #内服　入院
        if(nrow(naihuku_nyuin) ==0){
          g_naihuku_nyuin <- gen_no_data_plot("内服(入院)")
        }else{
          g_naihuku_nyuin <- gen_plot_total_nyuin( naihuku_nyuin ,"内服(入院)")
        }
        
        #外用　外来
        if(nrow(gaiyou_gairai) ==0){
          g_gaiyou_gairai <- gen_no_data_plot("外用(外来)")
        }else{
          g_gaiyou_gairai <- gen_plot_total_gairai(gaiyou_gairai ,"外用(外来)")
          g_leg <- get_legend(g_gaiyou_gairai)
          g_gaiyou_gairai <- g_gaiyou_gairai + theme(legend.position = "none")
        }
        
        #外用　入院
        if(nrow(gaiyou_nyuin)  ==0){
          g_gaiyou_nyuin  <- gen_no_data_plot("外用(入院)")
        }else{
          g_gaiyou_nyuin  <- gen_plot_total_nyuin( gaiyou_nyuin  ,"外用(入院)")
        }
        
        #注射　外来
        if(nrow(inject_gairai) ==0){
          g_inject_gairai <- gen_no_data_plot("注射(外来)")
        }else{
          g_inject_gairai <- gen_plot_total_gairai(inject_gairai ,"注射(外来)")
          g_leg <- get_legend(g_inject_gairai)
          g_inject_gairai <- g_inject_gairai + theme(legend.position = "none")
        }
        
        #注射　入院
        if(nrow(inject_nyuin)  ==0){
          g_inject_nyuin  <- gen_no_data_plot("注射(入院)")
        }else{
          g_inject_nyuin  <- gen_plot_total_nyuin( inject_nyuin  ,"注射(入院)")
        }
        
        #legendの判定
        g_void_part <- ggplot()+theme_void()
        
        if(is.null(g_leg)){g_leg <- g_void_part}
        
        g_list <- list(
          g_naihuku_gairai,g_naihuku_nyuin,g_gaiyou_gairai,g_gaiyou_nyuin,g_inject_gairai,g_inject_nyuin
        )
        leg_list <- list(g_leg, g_void_part)
        
        ggg <- plot_grid(
          plot_grid(plotlist = g_list, nrow=2, byrow=FALSE), 
          plot_grid(plotlist = leg_list, nrow=2), 
        nrow=1, rel_widths = c(8,1))
        
        return(ggg)
        
      }
      
      gen_graph_rank <- function(.gdat, tgt_med, set_year,set_med_type, fontsize=12){
        
        gairai_gai <- .gdat %>% filter(med_type == set_med_type, inout == "外来", out_ishp == "院外")
        gairai_nai <- .gdat %>% filter(med_type == set_med_type, inout == "外来", out_ishp == "院内")
        nyuin      <- .gdat %>% filter(med_type == set_med_type, inout == "入院")
        
        gen_plot_rank <- function(.data, title_set, tgt_year, tgt_yj){
          
          gdat <- .data %>% 
            filter(nendo == tgt_year) %>% 
            mutate(yjname = str_c(yj_code,":\n",str_sub(iyakuhin_name,1,18),"\n",str_sub(iyakuhin_name,19,1000))) %>% 
            mutate(color_this = yj_code == tgt_yj)
          
          gg <- ggplot(gdat) +
            geom_col(aes(x = overall_total, 
                         y = reorder(yjname, overall_total),
                         fill = color_this)) +
            scale_x_continuous(labels = scales::comma, guide = guide_axis(angle=90)) +
            scale_fill_manual(values = c("grey80","skyblue")) +
            labs(x = "処方件数", y = NULL, title = title_set) +
            theme_bw(base_size=fontsize) +
            theme(legend.position = "none", plot.title.position = "plot")

          return(gg)
        }
        gen_no_data_plot <- function(title_set){
          ggplot() +
            geom_text(aes(1,1,label="データがありません")) +
            labs(title = title_set) +
            theme_void(base_size=14)
        }
        
        make_plot_rank <- function(tgt_data, title_set, tgt_year, tgt_yj){
          if(nrow(tgt_data) == 0){
            ggres <- gen_no_data_plot(title_set)
          }else{
            ggres <- gen_plot_rank(tgt_data, title_set, tgt_year, tgt_yj)
          }
          
          return(ggres)
        }
        
        g_gairai_gai<- make_plot_rank(gairai_gai,str_c(set_med_type, "(外来)-院外"),set_year,tgt_med$yj_code)
        g_gairai_nai<- make_plot_rank(gairai_nai,str_c(set_med_type, "(外来)-院内"),set_year,tgt_med$yj_code)
        g_nyuin     <- make_plot_rank(nyuin     ,str_c(set_med_type, "(入院)"     ),set_year,tgt_med$yj_code)
        
        g_list <- list(g_gairai_gai,g_gairai_nai,g_nyuin)
        
        ggg <- plot_grid(plotlist = g_list, ncol=3)
        return(ggg)
      }
      
      gen_graph_tensu <- function(fontsize=12){
        if(nrow(target_med()) == 0){
          fing <- gen_no_data_plot("医薬品の薬価の推移")
        }else{
          gdat <- similar_yj_med()
          gdat_col <- gdat %>% filter(yj_code != target_med()$yj_code)
          gdat_col <- gdat_col %>% mutate(fill_label = str_c(yj_code,":",iyakuhin_name))
          yj_length <- gdat_col$fill_label %>% unique() %>% length()
          
          gdat_point <- gdat %>% filter(yj_code == target_med()$yj_code) 
          fing <- ggplot() +
            geom_col(aes(x = as.character(nendo),
                         y = yakka, 
                         fill = fill_label), 
                     alpha = 0.7,
                     data = gdat_col, 
                     position = "dodge") +
            scale_fill_manual(name = "YJコード:医薬品名",values = rainbow_hcl(yj_length)) +
            geom_line(data = gdat_point, 
                      mapping = aes(x = as.character(nendo),
                                    y = yakka), color = "red",
                      group=1, size = 1) +
            geom_point(data = gdat_point,
                       mapping = aes(x = as.character(nendo),
                                     y = yakka), color = "red", size = 2) +
            labs(x = NULL, y = "薬価", title = str_c(target_med()$iyakuhin_name,"の薬価"),
                 caption = str_c("YJコード上",input$keta,"桁が同一の薬品を描画しています") )+
            theme_bw(base_size = fontsize) +
            # theme(legend.key.height = unit(0.75,"cm"),
            #       legend.text = element_text(size = fontsize, lineheight = unit(0.5,"cm"))) +
            facet_wrap(~is_generic, labeller = "label_both")
        }
      }
      
      #graph outputs-----------------------------
      output$graph_total <- renderPlot({
        gen_graph_total(dat_value(), target_med())
      })
      
      output$graph_tensu <- renderPlot({
        fing <- gen_graph_tensu()
        return(fing)
      })
      
      output$rank_naihuku <- renderPlot({
        gen_graph_rank(dat_rank(), target_med(), input$nendo,"内服")
      })
      
      output$rank_gaiyou <- renderPlot({
        gen_graph_rank(dat_rank(), target_med(), input$nendo,"外用")
      })
      
      output$rank_injection <- renderPlot({
        gen_graph_rank(dat_rank(), target_med(), input$nendo,"注射")
      })
      
      #output$hyou-------------------
      hyou_data <-reactive({
        if(nrow(dat_rank())==0){
          return(tibble("医薬品を選択してください"))  
        }else{
          dat_rank()
        }
      })
      
      output$hyou <- renderDataTable({
        hyou_data()
      })
      
      #ダウンロードロジック------------------------
      ## 表------------------------------
      output$dlexcel <- downloadHandler(
        filename = function(){
          tmed <- target_med()
          
          nam <- tmed$iyakuhin_name
          yjc <- tmed$yj_code
          
          fn <- str_c("table_",nam,"_",Sys.Date(),"_", yjc,".xlsx")
          
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
          tmed <- target_med()
          
          nam <- tmed$iyakuhin_name
          yjc <- tmed$yj_code
          
          fn <- str_c("ppt_",nam,"_",Sys.Date(),"_", yjc,".pptx")
          
          return(fn)  
        },
        content = function(file){
          pptfontsize = 24
          tmed <- target_med()
          
          nam <- tmed$iyakuhin_name
          yjc <- tmed$yj_code
          yr <- input$nendo
          
          g1 <- gen_graph_total(dat_value(), target_med(), fontsize=pptfontsize)
          g2 <- gen_graph_tensu(fontsize=pptfontsize)
          g3 <- gen_graph_rank(dat_rank(), target_med(), input$nendo,"内服", fontsize=pptfontsize)
          g4 <- gen_graph_rank(dat_rank(), target_med(), input$nendo,"外用", fontsize=pptfontsize)
          g5 <- gen_graph_rank(dat_rank(), target_med(), input$nendo,"注射", fontsize=pptfontsize)
          
          title_string <- str_c(yjc,":\n",nam,"\n",yr,"年度の全体集計結果")
          
          ppt <- read_pptx() %>% 
            add_slide(layout = "Title Slide") %>% 
            ph_with(value = title_string, location = ph_location_type(type="ctrTitle")) %>% 
            add_slide() %>% ph_with(value=g1, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g2, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g3, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g4, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value=g5, location=ph_location_fullsize())
          
          print(ppt, file)
        }
      )
    }
  )
}
