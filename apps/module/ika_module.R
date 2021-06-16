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

showtext_auto()

ika_sex_age_UI <- function(id){
  ns <- NS(id)
  #data and setting file------------------------
  
  set_age <- read_rds("setting/ika_age.rds")
  set_sinryou <- read_rds("setting/ika_sinryou_code.rds")
  set_bunrui <- read_rds("setting/ika_bunrui.rds")
  set_inout <- read_rds("setting/ika_bunrui_in_out.rds")
  
  #ui side panel ----------------------------------
  
  side_panel <- sidebarPanel(
    h2("設定:"),
    pickerInput(ns("seireki"),"西暦年",choices=2015:2018,selected = 2018),
    pickerInput(ns("agegrp"),"年齢",choices=set_age,selected=1),
    pickerInput(ns("inout"), "入院/外来", choices="診療行為を選択してください",selected=1),
    tags$b("診療行為を選択"),
    DT::dataTableOutput(ns("sinryou_koui")),
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
        title = "単年度",
        fluidRow(
          column(width=4, withSpinner( plotOutput( ns("graph_total"),height=400)) ),
          column(width=8, withSpinner( plotOutput( ns("graph_total_age"),height=400)) ),
        ),
        fluidRow(
          column(width=12, withSpinner( plotOutput( ns("graph_total_sex_age"), height=400)))
        )
      ),
      tabPanel(
        title = "経年変化",
        fluidRow(
          column(width=6, withSpinner( plotOutput( ns("time_male")  ,height=200) ) ),
          column(width=6, withSpinner( plotOutput( ns("time_female"),height=200) ) )
        ),
        fluidRow(
          column(width=12, withSpinner( plotOutput( ns("time_precise")  , height=1000)))
        )
      ),
      tabPanel(
        title = "表",
        fluidRow(
          column(width=6,
                 checkboxGroupButtons(
                   inputId = ns("groupvar"),
                   label = "集計する変数を指定",
                   choices=c("年度" = "nendo", "性別"="sex","年齢区分"="age"),
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


ika_sex_age_Server <- function(id){
  moduleServer(
    id,
    function(input,output,session) {
      ns <- session$ns
      
      age_data <- read_rds("data/processed/ika_sinryou/age_data_light.rds")
      set_age <- read_rds("setting/ika_age.rds")
      set_sinryou <- read_rds("setting/ika_bunrui.rds")
      set_bunrui <- read_rds("setting/ika_bunrui.rds")
      set_inout <- read_rds("setting/ika_bunrui_in_out.rds")
      set_tensu <- read_rds("setting/ika_tensu.rds")
      
      
      #診療行為の選択UI用DT--------------
      output$sinryou_koui <- DT::renderDataTable(
        {set_sinryou},
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
        choices <- age_dat()$in_out %>% unique()
        updatePickerInput(session, "inout", choices=choices)
      })
      
      #グラフ用フィルターデータ
      age_dat <- reactive({
        if(!is.null(input$sinryou_koui_rows_selected)){
          selected_koui <- set_bunrui %>% slice(input$sinryou_koui_rows_selected)  
        }else{
          selected_koui <- tibble(sinryou_code = NA)
          
        }
        
        temp <- age_data %>% 
          filter(sinryou_code %in% selected_koui$sinryou_code) %>% 
          left_join(selected_koui, by=c("sinryou_code")) %>% 
          left_join(
            tibble(ndb = as.character(2:5),
                   nendo = 2015:2018), by = "ndb"
          ) %>% 
          left_join(
            set_tensu,
            by = c("ndb","in_out","sinryou_code")
          ) %>% 
          mutate(tensu = as.numeric(tensu))
        
        return(temp)
      })
      
      koui_name <- reactive({
        selected_koui <- set_bunrui %>% 
          slice(input$sinryou_koui_rows_selected) %>% 
          pull(sinryou)
        
        return(selected_koui)
      })
      
      func_table_total <- function(.data, tgt_inout, tgt_nendo){
        gdat <- .data %>% 
          filter(in_out == tgt_inout) %>%
          group_by(nendo, in_out, sinryou_code, sinryou) %>% 
          summarise(
            value = sum(value,na.rm=TRUE),
            value_tensu = sum(value * tensu, na.rm=TRUE)
          ) %>% 
          mutate(color_this = nendo == tgt_nendo)
        
        return(gdat)
      }
      
      func_graph_total <- function(.gdata, type){
        
        if(type == "tensu"){
          
          ylab <- "点数×算定回数"
          .gdata <- .gdata %>% 
            mutate(yval = value_tensu)
        }
        
        if(type == "kaisu"){
          ylab <- "算定回数"
          .gdata <- .gdata %>% 
            mutate(yval = value)
        }
        
        #全部の年度がそろっているかを確認
        if( any(!c(2015:2018) %in% .gdata$nendo) ){
          add_this <- tibble(nendo = c(2015:2018))
          
          .gdata <- add_this %>% left_join(.gdata,by="nendo") %>% 
            replace_na(list(yval=0))
        }
        
        gg <- ggplot(.gdata) +
          geom_col(aes(x = as.character(nendo), y = yval, fill=color_this)) +
          scale_y_continuous(labels=scales::comma) +
          scale_fill_manual(values = rainbow_hcl(6)[5:6], guide = guide_none()) +
          labs(x = "年度", y = ylab, title="全国の算定回数の集計") +
          theme_bw()  
        
        return(gg)
      }
      
      
      func_table_total_age <- function(.data, tgt_inout, tgt_age,tgt_seireki){
        .data %>% 
          filter(in_out == tgt_inout) %>% 
          group_by(nendo, age) %>%
          summarise(value = sum(value, na.rm=TRUE)) %>% 
          mutate(color_this = age == tgt_age) %>% 
          ungroup() %>% 
          filter(nendo == tgt_seireki)
      }
      
      func_table_total_sex_age <- function(.data, tgt_inout, tgt_age,tgt_seireki){
        .data %>% 
          filter(in_out == tgt_inout) %>% 
          group_by(nendo, age, sex) %>%
          summarise(value = sum(value, na.rm=TRUE)) %>% 
          mutate(color_this = age == tgt_age) %>% 
          ungroup() %>% 
          filter(nendo == tgt_seireki)
      }
      
      func_graph_total_age <- function(.gdata){
        ggplot(.gdata) +
          geom_col(aes(x = age, y = value, fill=color_this)) +
          scale_fill_manual(values = rainbow_hcl(2), guide = guide_none()) +
          scale_x_discrete(guide = guide_axis(angle = 90)) +
          scale_y_continuous(labels=scales::comma) +
          labs(x = NULL, y = "算定回数") +
          theme_bw(base_size=12)
      }
      
      
      #output$graph_total -------------------
      output$graph_total <- renderPlot({
        table <- func_table_total(age_dat(), input$inout, input$seireki)
        func_graph_total(table,"kaisu")
      })
      
      #output$graph_total_age-----------------
      output$graph_total_age <- renderPlot({
        gdat <- func_table_total_age(age_dat(),input$inout, input$agegrp, input$seireki)
        gg <- func_graph_total_age(gdat) +
          labs(title = str_c(input$seireki,"年度年齢別算定回数"))
        return(gg)
      })
      
      #output$graph_total_sex_age--------------------
      output$graph_total_sex_age <- renderPlot({
        gdat <- func_table_total_sex_age(age_dat(),input$inout, input$agegrp, input$seireki)
        
        func_graph_total_age(gdat) +
          labs(title = str_c(input$seireki,"年度:年齢、性別別算定回数")) +
          facet_wrap(~sex)
      })
      
      #age_dat_time()------------------
      age_dat_time <- reactive({
        
        age_dat() %>% 
          filter(in_out == input$inout) %>% 
          select(nendo, sex, age, value)
      })
      
      #age_dat_time_male()--------------------------
      age_dat_time_male <- reactive({
        
        age_dat_time() %>% 
          filter(sex == "male") %>% 
          mutate(color_this = age == input$agegrp)
      })
      
      #age_dat_time_female()------------------------
      age_dat_time_female <- reactive({
        age_dat_time() %>% 
          filter(sex == "female") %>% 
          mutate(color_this = age == input$agegrp)
      })
      
      func_graph_time <- function(gdat){
        color1 <- rainbow_hcl(2)[1]
        color2 <- rainbow_hcl(2)[2]
        
        gdat_t <- gdat %>% filter(color_this)
        gdat_f <- gdat %>% filter(!color_this)
        
        ggplot() +
          geom_line(
            data = gdat_f, 
            color = color1,
            mapping = aes(x = nendo, y = value, group=age), size=1) +
          
          geom_line(
            data = gdat_t, 
            color = color2,
            mapping = aes(x = nendo, y = value, group=age), size=2) +
          
          geom_point(
            data = gdat_t, 
            color = color2,  
            mapping =aes(x = nendo, y = value), size = 3) +
          
          scale_y_continuous(labels=scales::comma) +
          labs(x = NULL, y = "算定回数") +
          theme_bw()
      }
      
      gen_base_plot <- function(.gdata,tsize, facet_row,legrow=2){
        
        ggplot(.gdata) +
          geom_col(aes(x = nendo, y = value, fill=age, color = color_this)) +
          scale_fill_manual(name = NULL, values = rainbow_hcl(length(levels(.gdata$age)))) +
          scale_color_manual(values = c(NA,"red"), guide=guide_none()) +
          scale_x_continuous(guide = guide_axis(angle=90)) +
          scale_y_continuous(labels=scales::comma) +
          facet_wrap(~age,nrow=facet_row) +
          labs(x = NULL, y = "算定回数") +
          theme_bw(base_size=tsize) +
          guides(
            fill = guide_legend(nrow =legrow, 
                                direction = "horizontal" )) +
          theme(legend.position = "bottom", legend.spacing.x = unit(1,"cm"))
      }
      
      func_graph_time_precise <- function(gdat_male, gdat_female, tsize){
        
        g_male   <- gen_base_plot(gdat_male,tsize,1,legrow=2)
        g_female <- gen_base_plot(gdat_female, tsize,1,legrow=2)
        
        g_leg <- get_legend(g_male)
        g_male   <- g_male +theme(legend.position="none")+labs(title="男性")
        g_female <- g_female +theme(legend.position="none")+labs(title="女性")
        
        plot_grid(
          plotlist=list(g_leg, g_male, g_female),
          nrow = 3, 
          rel_heights = c(1,4,4)
        )
      }
      
      #output$time_male
      output$time_male <- renderPlot({
        
        gdat_male <- age_dat_time_male()
        func_graph_time(gdat_male) +labs(title = str_c(koui_name(),"の男性の算定回数の経年変化"))
        
      })
      
      #output$time_female
      output$time_female <- renderPlot({
        gdat_female <- age_dat_time_female()
        func_graph_time(gdat_female) +labs(title = str_c(koui_name(),"の女性の算定回数の経年変化"))
      })
      
      #output$time_precies------------------------
      output$time_precise <- renderPlot({
        suppressWarnings(
          func_graph_time_precise(age_dat_time_male(),age_dat_time_female(),12)
        )
      })
      
      #output$hyou-------------------
      hyou_data <-reactive({
        if(nrow(age_dat())==0){
          return(tibble("診療行為を選択してください"))  
        }else{
          
          
          grpthese <- rlang::syms(input$groupvar)
          
          age_dat() %>% 
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
          
          fn <- str_c("table_",koui,"_",Sys.Date(),"_", txt_gv,".xlsx")
          
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
          
          fn <- str_c("ppt_",koui,"_",Sys.Date(),"_", txt_gv,".pptx")
          
          return(fn)
        },
        content = function(file){
          ppttextsize <- 36
          
          g1 <- func_table_total(age_dat(), input$inout, input$seireki) %>% 
            func_graph_total(.,"kaisu")
          
          g2 <- func_table_total_age(age_dat(),input$inout, input$agegrp, input$seireki) %>% 
            func_graph_total_age() +
            labs(title = str_c(input$seireki,"年度年齢別算定回数"))
          
          g3 <- func_table_total_sex_age(age_dat(),input$inout, input$agegrp, input$seireki) %>% 
            func_graph_total_age() +
            labs(title = str_c(input$seireki,"年度:年齢、性別別算定回数")) +
            facet_wrap(~sex)
          
          g4 <-  age_dat_time_male() %>% 
            func_graph_time() +
            labs(title = str_c(koui_name(),"の男性の算定回数の経年変化"))
          
          g5 <- age_dat_time_female() %>% 
            func_graph_time() +
            labs(title = str_c(koui_name(),"の女性の算定回数の経年変化"))
          
          g6 <- gen_base_plot(age_dat_time_male(),ppttextsize,2,4) +
            theme(legend.position="none") +
            labs(title = str_c(koui_name(),":男性"))
          
          g7 <- gen_base_plot(age_dat_time_female(),ppttextsize,2,4) +
            theme(legend.position="none") +
            labs(title = str_c(koui_name(),"女性"))
          
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
            add_slide() %>% ph_with(value = g5, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g6, location=ph_location_fullsize()) %>% 
            add_slide() %>% ph_with(value = g7, location=ph_location_fullsize())
          
          print(pres, file)
        }
      )
    }
  )
}
