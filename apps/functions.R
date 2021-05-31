# ken <- read_rds("apps/data/processed/tokutei_kensin/data.rds")
# set_nendo <- read_rds("apps/setting/nbd_nendo.rds")
# set_age <- read_rds("apps/setting/ndb_age_kubun.rds")
# set_koumoku <- read_rds("apps/setting/ndb_kensa_koumoku.rds")
# set_kubun <- read_rds("apps/setting/ndb_kensa_kubun.rds")
# set_pref <- read_rds("apps/setting/ndb_prefecture.rds")
library(purrr)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)


#読み込んだ特定健診データから検査名でfilterする
filter_kensa <- function(.data,target_kensa,set_kubun,set_nendo){
  
  dat <- .data %>% 
    filter(str_detect(kensa_koumoku, target_kensa)) %>% 
    select(ndb = dir3, kensa_koumoku, long_data) %>% 
    unnest(c(long_data)) %>% 
    factorize_kensa(., target_kensa,set_kubun) %>% 
    convert_ndb_to_seireki(set_nendo)
  return(dat)
}

#target_kensaの値から因子型のためのレベルとラベルを作成する
extract_levlab <- function(target_kensa, set_kubun){
  abbreviate_kensa <- case_when(
    target_kensa == "BMI" ~ "bmi",
    target_kensa == "GOT（AST）" ~ "ast",
    target_kensa == "GPT（ALT）" ~ "alt",
    target_kensa == "HbA1c" ~ "a1c",
    target_kensa == "HDLコレステロール" ~ "hdl",
    target_kensa == "LDLコレステロール" ~ "ldl",
    target_kensa == "γ-GT（γ-GTP）" ~ "gtp",
    target_kensa == "拡張期血圧" ~ "dbp",
    target_kensa == "収縮期血圧" ~ "sbp",
    target_kensa == "中性脂肪" ~ "tg",
    target_kensa == "腹囲" ~ "huk"
  )
  
  tgt_lev <- set_kubun$lev[[abbreviate_kensa]]
  tgt_lab <- set_kubun$lab[[abbreviate_kensa]]
  
  return(list(level = tgt_lev, label = tgt_lab))
}


#絞り込んだ検査値から、検査値階層を因子化する
factorize_kensa <- function(.data, target_kensa, set_kubun){
  levlab <-  extract_levlab(target_kensa,set_kubun) 
 
 .data <- .data %>% 
   mutate(kaisou = factor(kaisou,levels = levlab$level, labels = levlab$label))
 
 return(.data)
}

#ndbの回に西暦データを追加する
convert_ndb_to_seireki <- function(.data, set_nendo){
  dat <- .data %>% 
    left_join(set_nendo, by = "ndb") %>% 
    select(ndb, seireki, everything())
  return(dat)
}

#dichotomousなアウトプットを付与する
add_dichotomous <- function(.data, target_kensa, low_lev){
  
  original_levels <- levels(.data$kaisou)
  high_lev <- original_levels[!(original_levels %in% low_lev)]
  
  low_lab  <- str_extract(low_lev[length(low_lev)],"((\\d+\\.\\d+)|(\\d+))(未満|以下)")
  high_lab <- str_extract(high_lev[1],"((\\d+\\.\\d+)|(\\d+))以上") 
  
  new_label <- c(rep(low_lab,length(low_lev)), rep(high_lab,length(high_lev)))
  
  .data <- .data %>% 
    mutate(kaisou_di = factor(kaisou,
                              levels = original_levels,
                              labels = new_label))
    
  return(.data)
  
}

#特定のグループ変数を利用して割合を計算
make_total <- function(ken, ...){
  group_these <- syms(c(...))
  
  ken %>% 
    group_by(!!!group_these) %>% 
    summarise(total = sum(value,na.rm=TRUE)) %>% 
    ungroup()
  
}

make_percent <- function(ken, kaisou = "m",...){
  
  if(kaisou == "m") kaisou_grp <- "kaisou"
  if(kaisou == "d") kaisou_grp <- "kaisou_di"
  
  group_these <- syms(c(...,kaisou_grp))
  
  total_data <- make_total(ken, ...)
  
  res <- ken %>% 
    group_by(!!!group_these) %>% 
    summarise(value = sum(value,na.rm=TRUE)) %>% 
    left_join(total_data, by = c(...)) %>% 
    mutate(perc = value/total) %>% 
    ungroup()
  
  return(res)
}

#shinyのデバッグ用
capture_inputs <- function(prefix="i__",input){
  map(names(input),~{
    assign(str_c(prefix,.), input[[.]],envir = .GlobalEnv)
  })
}
