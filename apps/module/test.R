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
library(leaflet)

showtext_auto()

med_pref_UI <- function(id){
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
    p("ダウンロード(EXCEL)で、都道府県別ランキングが保存されたエクセルファイルをダウンロードできます。(HTMLファイルの形式そのままでしかダウンロードできず、未完成です。)"),
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
        title = "都道府県比較(処方回数)",
        h3("処方回数:"),
        fluidRow(
          column(width=4, withSpinner( plotOutput( ns("graph_total_bypref"),height=1000))),
          column(width=8, withSpinner( leafletOutput( ns("graph_pref_total_map"),height=1000)))
        )
      )
    )
  
  
  #ui----------------------------------------
  ui <- fluidPage(
    title = "NDBオープンデータ可視化:医薬品",
    titlePanel("NDBオープンデータ可視化:医薬品(都道府県別)"),
    sidebarLayout(
      sidebarPanel = side_panel,
      mainPanel = mainPanel(
        ui_meds,
        tabset_panel
      )
    )
  )
  
}


med_pref_Server <- function(id){
  moduleServer(
    id,
    function(input,output,session) {
      ns <- session$ns
      
      pref_data <- read_rds("data/processed/medicine/pref_data_light.rds")
      dat_map <- read_rds("data/processed/map.rds")
      med_tensu_master <- read_rds("setting/iyakuhin_tensu_master.rds")
      med_master     <- read_rds("setting/iyakuhin_master.rds")
      set_nendo <- read_rds("setting/meidicine_nendo.rds")
      
      #医薬品の選択UI用DT-----------------------------------
      output$iyakuhin <- DT::renderDataTable(
        expr = med_master,
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
        
        warning("from target_med()")
        warning(tgt_med)
        
        return(tgt_med)
      })
      
      similar_yj_med <- reactive({
        if(nrow(target_med()) == 0){
          return(tibble())
        }else{
          tgt_yj_keta <- target_med()$yj_code %>% str_sub(.,1,input$keta)
          similar_yj_med <- med_tensu_master %>% filter(str_detect(yj_code,str_c("^",tgt_yj_keta)))
          
          similar_yj_med <- similar_yj_med %>% 
            left_join(set_nendo, by="ndb") %>% 
            mutate(yakka = as.numeric(yakka))
          return(similar_yj_med)
          
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
        
        if(nrow(sim_med)==0){
          return(tibble())
        }else{
          dat <- pref_data %>% 
            semi_join(sim_med, by = c("iyakuhin_code","yj_code"))
          
          dat2 <- dat %>% 
            left_join(med_tensu_master, by = c("ndb","med_type","iyakuhin_code","yj_code")) %>% 
            left_join(set_nendo, by = c("ndb"))
          
          dat3 <- dat2 %>% 
            mutate(nendo = as.character(nendo))
          
          if(nrow(dat) != nrow(dat2)){warning("filtering Warning!")}
          
          res <- dat3 %>% 
            filter(
              med_type == input$med_type, 
              inout    == input$in_out, 
              out_ishp == input$prescription_in_out)
          
          return(res)
        }
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
      
      get_pref_name_from_click <- function(click_obj, dat_map){
      
        clicked_pt <- st_point_on_surface(
          st_point(c(click_obj$lng, click_obj$lat), dim="XY")
        )
        
        clicked_pref <- dat_map %>% 
          select(name_ja) %>% 
          st_filter(clicked_pt) %>% 
          pull(name_ja)
        
        return(clicked_pref)
      }
      
      gen_graph_total_bypref <- function(.gdat, tgt_med, pref_name = NA, title = FALSE, fontsize = 18){
        warning(str_c("tgt_med:",tgt_med))
        warning(str_c("pref_name:",pref_name))
        
        gdat   <- .gdat
        
        title_text <- str_c(
          tgt_med$yj_code, ":",tgt_med$iyakuhin_name,"\n",
          gen_title_by_med_types(input$med_type, input$in_out, input$prescription_in_out)
        )
        
        warning(str_c("title_text:",title_text))
        
        if(is.na(pref_name)){
          gdat2 <- gdat %>% mutate(color_this = FALSE)
        }else{
          gdat2 <- gdat %>% mutate(color_this = prefname == pref_name)
        }
        
        if(title){
          #do nothing
        }else{
          title_text <- NULL
        }
        
        gg <- ggplot(gdat2) + 
          geom_col(aes(x = value, y = reorder(prefname, value), fill=color_this)) +
          scale_x_continuous(label=scales::comma) +
          scale_fill_manual(guide = guide_none(), values = rainbow_hcl(2)) +
          labs(x = "処方回数", y = NULL, title = title_text) +
          theme_bw(base_size = fontsize)
        
        return(gg)
      }
      
      gen_graph_group_change <- function(.gdat, tgt_med, tgt_sim_med,  pref_name = NA, fontsize = 18){
        
        gdat   <- .gdat
        tgt_med <- tgt_med
        tgt_sim_med <- tgt_sim_med
        
        title_text <- str_c(
          tgt_med$iyakuhin_code, ":",tgt_med$iyakuhin_name,"\n",
          gen_title_by_med_types(input$med_type, input$in_out, input$prescription_in_out)
        )
        
        if(is.na(pref_name)){
          gdat2 <- gdat %>% mutate(color_this = FALSE)
        }else{
          gdat2 <- gdat %>% filter(prefname == pref_name)
          gdat3 <- gdat2 %>% 
            mutate(med_name = str_c(iyakuhin_code,":",iyakuhin_name))
        }
        
        ttt <- str_c(
          pref_name,"の", tgt_med$iyakuhin_name,"と類似医薬品の処方回数の経年変化"
        )
        
        mednum <- gdat3$med_name %>% unique() %>% length()
        
        gdat_tgtmed <- gdat3 %>% semi_join(tgt_med, by = c("iyakuhin_code", "yj_code", "iyakuhin_name", "is_generic"))
        
        g <-ggplot(gdat3) +
          geom_line(aes(x = nendo, y = value, color=med_name, group = med_name),size=1) +
          geom_point(aes(x = nendo,y = value, color = med_name),
                     size = 1.5,
                     data = gdat_tgtmed) +
          labs(x = NULL, y = "処方回数", title = ttt) +
          scale_y_continuous(labels = scales::comma) +
          scale_color_manual(name = NULL, values = rainbow_hcl(mednum)) +
          theme_bw(base_size = fontsize)
        
        return(g)
      }
      
      gen_graph_pref_total_map <- function(.gdat, tgt_med, fontsize = 18){
        gdat   <- .gdat
        tgt_med <- tgt_med
        
        title_text <- str_c(
          tgt_med$yj_code, ":",tgt_med$iyakuhin_name,"\n",
          gen_title_by_med_types(input$med_type, input$in_out, input$prescription_in_out)
        )
        
        gdat_with_map <- dat_map %>% 
          left_join(gdat, by = c("name_ja"="prefname"))
        
        pal <- colorBin("OrRd", domain = gdat$value, 10)
        warning(gdat_with_map %>% slice(2))
        leaflet(data = gdat_with_map) %>% 
          addPolygons(weight = 1,
                      opacity = 1,
                      fillColor = ~pal(value), 
                      color="white",
                      dashArray = "3",
                      fillOpacity = 1,
                      highlight = highlightOptions(
                        weight = 2,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 1,
                        bringToFront = TRUE
                      ),
                      label = ~{str_c(name_ja,"\n",scales::comma(value))},
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textSize = "15px",
                        direction = "auto"
                      )) %>% 
          addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL, position = "bottomright")
      }
      
      #graph outputs-----------------------------
      output$graph_total_bypref <- renderPlot({
      
        if(is.null(input$graph_pref_total_map_shape_click)){
          tgt_pref = NA
        }else{
          tgt_pref <- get_pref_name_from_click(
            input$graph_pref_total_map_shape_click, dat_map
          )  
        }
        warning(target_med())
        gg <- gen_graph_total_bypref(.gdat = dat_value_filtered(), 
                                     tgt_med = target_med(), 
                                     pref_name = tgt_pref)
        return(gg)
      })
      
      output$graph_pref_total_map <- renderLeaflet({
        return(gen_graph_pref_total_map(dat_value_filtered(), target_med()))
      })
      
    }
  )
}
