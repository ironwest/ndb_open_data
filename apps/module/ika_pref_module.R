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
library(cowplot)
library(geofacet)
library(plotly)

showtext_auto()

ika_pref_UI <- function(id){
  ns <- NS(id)
  #data and setting file------------------------
  
  set_pref <- read_rds("setting/ika_pref.rds")
  set_bunrui <- read_rds("setting/ika_bunrui.rds")
  set_inout <- read_rds("setting/ika_bunrui_in_out.rds")
  
  choice_pref <- as.list(set_pref$prefid)
  names(choice_pref) <- set_pref$prefname
  #ui side panel ----------------------------------
  
  side_panel <- sidebarPanel(
    h2("設定:"),
    sliderInput(ns("seireki"),"西暦年",min=2014, max=2017,value = 2017, sep=""),
    pickerInput(ns("pref"),"都道府県",choices=choice_pref, selected=1),
    pickerInput(ns("inout"), "入院/外来", choices="診療行為を選択してください",selected=1),
    tags$b("診療行為を選択"),
    DT::dataTableOutput(ns("sinryou_koui")),
    hr(),
    downloadButton(ns("dlppt"),"ダウンロード(PPT)"),
    downloadButton(ns("dlexcel"),"ダウンロード(EXCEL)"),
    p("使い方"),
    p("ダウンロード(PPT)で、表示されているグラフを含んだスライドをダウンロードできます。"),
    p("ダウンロード(EXCEL)で、データの表に表示されているテーブルが保存されたエクセルファイルをダウンロードできます。"),
    width=3
  )
  
  #tab panel structure ------------------------------------
  tabset_panel <- 
    tabsetPanel(id = ns("current_tab"),
      tabPanel(
        title = "単年度",
        value = "single",
        fluidRow(
          column(width=4, withSpinner( plotOutput( ns("single_all_pref"),height=800)) ),
          column(width=8, align="top", withSpinner( plotOutput( ns("map_single"),height=800)) ),
        )
      ),
      tabPanel(
        title = "経年変化(算定回数)",
        value = "cont_num",
        fluidRow(
          column(width=12, withSpinner( plotlyOutput( ns("cont_num_facet")  ,height=800) ) ),
        )
      ),
      tabPanel(
        title = "変化割合(年度間)",
        value = "change",
        fluidRow(
          column(width=4, sliderInput(ns("from_to"),"年度間の変化",min=2014,max=2017,value=c(2014,2017),sep=""))
        ),
        fluidRow(
          column(width=4, withSpinner( plotOutput( ns("change_all_pref"),height=800))),
          column(width=8, withSpinner( plotOutput( ns("map_change"), height=800)))
        )
      ),
      tabPanel(
        value = "hyou",
        title = "表",
        fluidRow(
          column(width=6,
                 checkboxGroupButtons(
                   inputId = ns("groupvar"),
                   label = "集計する変数を指定",
                   choices=c("年度" = "nendo", "都道府県"="prefname"),
                   selected = "nendo")
          ),
          column(width=12, dataTableOutput(ns("hyou")))
        )
      )
    )
  
  
  #ui----------------------------------------
  ui <- fluidPage(
    title = "NDBオープンデータ可視化:医科診療行為",
    titlePanel("NDBオープンデータ可視化:医科診療行為(年齢性別)"),
    sidebarLayout(
      sidebarPanel = side_panel,
      mainPanel = mainPanel(tabset_panel)
    )
  )
}


ika_pref_Server <- function(id){
  moduleServer(
    id,
    function(input,output,session) {
      ns <- session$ns
      
      pref_data <- read_rds("data/processed/ika_sinryou/pref_data_light.rds")
      map_dat <- read_rds("data/processed/map.rds") %>% 
        select(name_ja)
      set_age <- read_rds("setting/ika_age.rds")
      set_bunrui <- read_rds("setting/ika_bunrui.rds")
      set_inout <- read_rds("setting/ika_bunrui_in_out.rds")
      set_tensu <- read_rds("setting/ika_tensu.rds")
      set_pref <- read_rds("setting/ika_pref.rds")
      
      #診療行為の選択UI用DT--------------
      output$sinryou_koui <- DT::renderDataTable(
        {set_bunrui},
        server = TRUE,
        extensions='Scroller',
        selection = 'single',
        options=list(
          deferRender=TRUE,
          scrollY = 200,
          scroller = TRUE
        )
      )
      
      #入院/外来加算の選択を診療行為に応じてUpdate-------------
      observeEvent(input$sinryou_koui_rows_selected, {
        choices <- pref_dat()$in_out %>% unique()
        updatePickerInput(session, "inout", choices=choices)
      })
      
      #グラフ用フィルターデータ
      pref_dat <- reactive({
        if(!is.null(input$sinryou_koui_rows_selected)){
          selected_koui <- set_bunrui %>% slice(input$sinryou_koui_rows_selected)
        }else{
          selected_koui <- tibble(sinryou_code = NA)
        }
        
        res <- pref_data  %>%
          filter(sinryou_code %in% selected_koui$sinryou_code) %>%
          left_join(selected_koui, by=c("sinryou_code")) %>%
          left_join(
            tibble(ndb = as.character(2:5),
                   nendo = 2014:2017), by = "ndb"
          ) %>%
          left_join(
            set_tensu,
            by = c("ndb","in_out","sinryou_code")
          ) %>%
          left_join(set_pref, by=c("prefid")) %>% 
          mutate(tensu = as.numeric(tensu)) 
        
        return(res)
      })
      
      koui_name <- reactive({
        selected_koui <- set_bunrui %>% 
          slice(input$sinryou_koui_rows_selected) %>% 
          pull(sinryou)
        
        return(selected_koui)
      })
      
      filtered_dat <- reactive({
        pref_dat() %>% 
          filter(in_out == input$inout)
      })
      
      #グラフ生成関数--------------------
      gen_blank <- function(){
        ggplot() +
          geom_text(aes(1,1,label="診療行為を選択してください"))+
          theme_void(base_size=24)
      }
      
      gen_single_all_pref <- function(.data, tgt_seireki, tgt_pref){
        gdat <- .data %>% 
          filter(nendo == tgt_seireki) %>% 
          mutate(color_this = prefid == tgt_pref)
        
        ggplot(gdat) +
          geom_col(aes(x = value, y = reorder(prefname, value), fill = color_this)) +
          scale_x_continuous(labels=scales::comma,guide = guide_axis(angle=45)) +
          scale_y_discrete(guide = guide_axis(n.dodge=2)) +
          scale_fill_manual(values = c("grey60","skyblue")) +
          labs(x = "算定件数", y = "都道府県",title = str_c(tgt_seireki,"年度の算定件数")) +
          theme_bw(base_size = 18) +
          theme(legend.position = "none", 
                plot.title.position = "plot")
      }
      
      #単年度グラフ------------------------------------------
      output$single_all_pref <- renderPlot({
        
        if(is.null(input$sinryou_koui_rows_selected)){
          gg <- gen_blank()
        }else{
          gg <- gen_single_all_pref(filtered_dat(), input$seireki, input$pref)
        }
        
        return(gg)
      })
      
      gen_map_single <- function(.data, tgt_seireki, tgt_pref){
        gdat <- .data %>% filter(nendo == tgt_seireki)
        
        gdat2 <- gdat %>% 
          left_join(map_dat, by = c("prefname" = "name_ja")) %>% 
          mutate(color_this = prefid == tgt_pref)
        
        ggplot(gdat2) +
          geom_sf(aes(geometry = geometry, fill=as.numeric(value), color=color_this),size=1) +
          scale_fill_viridis_c(name = "算定回数", labels = scales::comma) +
          scale_color_manual(values = c(NA,"red"), guide = guide_none()) +
          coord_sf(xlim = c(127,146), ylim = c(25,46)) +
          theme_bw(base_size = 18)
      }
      
      output$map_single <- renderPlot({
        
        if(is.null(input$sinryou_koui_rows_selected)){
          gg <- gen_blank()
        }else{
          gg <- gen_map_single(filtered_dat(), input$seireki, input$pref)
        }
        
        return(gg)
      })
      
      cont_dat <- reactive({
        gdat <- filtered_dat()
        
        gdat2 <<- gdat %>% 
          left_join(map_dat, by = c("prefname" = "name_ja")) %>% 
          mutate(color_this = prefid == input$pref)
        
        return(gdat2)
      })

      gen_cont_num_facet <- function(.data){
        
        tgt_pref_name <- .data %>% 
          filter(color_this) %>%
          pull(prefname) %>% unique()
        
        gdat <- .data %>% 
          mutate(prefpos = as.numeric(prefid))
        
        
        gdat %>% 
        ggplot() +
          geom_line(aes(x = nendo, y = value, group = prefname, color = prefname)) +
          scale_y_continuous(labels = scales::comma) +
          scale_x_continuous(guide = guide_axis(angle=90)) +
          theme_bw(base_size = 18) +
          labs(x = NULL, y = NULL, title="都道府県別の算定回数の経年変化") +
          theme(plot.title.position = "plot")
        
      }
      
      output$cont_num_facet <- renderPlotly({
        
        if(is.null(input$sinryou_koui_rows_selected)){
          gg <- gen_blank()
        }else{
          gg <- gen_cont_num_facet(cont_dat())
        }
        
        return( ggplotly(gg) )
      })
      
      #変化---------------------------
      change_dat <- reactive({
        gdat <- filtered_dat()
        
        gdat2 <- gdat %>% 
          left_join(map_dat, by = c("prefname" = "name_ja")) %>% 
          mutate(color_this = prefid == input$pref)
        
        gdat3 <- gdat2 %>% 
          filter(nendo %in% input$from_to) %>% 
          select(prefid, prefname, nendo, value) %>% 
          mutate(fromto = if_else(nendo == input$from_to[1], "from", "to")) %>% 
          select(-nendo) %>% 
          pivot_wider(id_cols = c(prefid, prefname),
                      values_from = value, 
                      names_from = fromto) %>% 
          mutate(change = (to-from)/from ) %>% 
          select(prefid, prefname, change) %>% 
          mutate(color_this = prefid == input$pref)
        
        return(gdat3)
        
      })
      
      gen_change_all_pref <- function(.data, fromto){
        graph_title <- str_c("算定回数変化割合(%):\n\n",str_c(fromto,collapse="から"),"まで")
        
        ggplot(.data) +
          geom_col(aes(x = change, y = reorder(prefname, change), fill = color_this)) +
          scale_x_continuous(labels=scales::percent,guide = guide_axis(angle=45)) +
          scale_y_discrete(guide = guide_axis(n.dodge=2)) +
          scale_fill_manual(values = c("grey60","skyblue")) +
          labs(x = "算定件数", y = "都道府県",title = graph_title) +
          theme_bw(base_size = 16) +
          theme(legend.position = "none", 
                plot.title.position = "plot")
      }
      
      #OUTPUT$change_all_pref---------------------------
      output$change_all_pref <- renderPlot({
        if(is.null(input$sinryou_koui_rows_selected)){
          gg <- gen_blank()
        }else{
          gg <- gen_change_all_pref(change_dat(), input$from_to)
        }
       
        return(gg)
      })
      
      
      gen_map_change <- function(.data){
        gdat <- .data %>% 
          left_join(map_dat, by = c("prefname" = "name_ja"))
        
        ggplot(gdat) +
          geom_sf(aes(geometry = geometry, fill=as.numeric(change), color=color_this),size=1) +
          scale_fill_viridis_c(name = "年度間の変化割合", labels = scales::percent) +
          scale_color_manual(values = c(NA,"red"), guide = guide_none()) +
          coord_sf(xlim = c(127,146), ylim = c(25,46)) +
          theme_bw(base_size = 18)
      }
      
      #output$map_change-------------------------------
      output$map_change <-renderPlot({
        if(is.null(input$sinryou_koui_rows_selected)){
          gg <- gen_blank()
        }else{
          gg <- gen_map_change(change_dat())
        }
        
        return(gg)
      })
      
      #output$hyou-------------------
      hyou_data <-reactive({
        if(nrow(filtered_dat())==0){
          return(tibble("診療行為を選択してください"))  
        }else{
          
          grpthese <- rlang::syms(input$groupvar)
          
          filtered_dat() %>% 
            group_by(!!!grpthese) %>% 
            summarise(
              in_out = last(in_out),
              sinryou_code = last(sinryou_code),
              sinryou = last(sinryou),
              value = sum(value, na.rm=TRUE),
              tensu = last(tensu)
            )
        }
        
        
      })
      
      output$hyou <- renderDataTable({
        hyou_data()
      })
      
      #ダウンロードロジック------------------------
      ## 表------------------------------
      output$dlexcel <- downloadHandler(
        filename = function(){
          
          gv <- input$groupvar
          koui <- koui_name()
          txt_gv <- str_c(gv,collapse="_")
          
          fn <- str_c("table_pref_",koui,"_",Sys.Date(),"_", txt_gv,".xlsx")
          
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
          gv <- input$groupvar
          koui <- koui_name()
          txt_gv <- str_c(gv,collapse="_")
          
          fn <- str_c("ppt_pref_",koui,"_",Sys.Date(),"_", txt_gv,".pptx")
          
          return(fn)
        },
        content = function(file){
          ppttextsize <- 36
          
          g1 <- gen_single_all_pref(filtered_dat(), input$seireki, input$pref) +
            labs(title = str_c(input$seireki,"年度:",koui_name(),"の算定回数"))
          
          g2 <- gen_map_single(filtered_dat(), input$seireki, input$pref) +
            labs(title = str_c(input$seireki,"年度:",koui_name(),"の算定回数(地図)"))
          
          g3 <- gen_cont_num_facet(cont_dat()) +
            labs(title = str_c(koui_name(),"の算定回数(経年変化)"))
          
          g4 <- gen_change_all_pref(change_dat(), input$from_to) +
            labs(title = str_c(koui_name(),":算定回数変化割合(%)", str_c(input$from_to,collapse="-")))
          
          g5 <- gen_map_change(change_dat()) +
            labs(title = str_c(koui_name(),":算定回数変化割合(%,地図)", str_c(input$from_to,collapse="-")))
          
          g1 <- g1 + theme(text = element_text(size = ppttextsize))
          g2 <- g2 + theme(text = element_text(size = ppttextsize))
          g3 <- g3 + theme(text = element_text(size = ppttextsize))
          g4 <- g4 + theme(text = element_text(size = ppttextsize))
          g5 <- g5 + theme(text = element_text(size = ppttextsize))
          
          
          pres <- read_pptx() %>% 
            add_slide() %>% ph_with(value = g1, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g2, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g3, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g4, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g5, location=ph_location_fullsize())
          
          print(pres, file)
        }
      )
    }
  )
}
