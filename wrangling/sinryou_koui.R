#診療行為の加工のためのスクリプト

library(tidyverse)
library(readxl)
source("functions/excel_reshaper.R", encoding = "UTF-8")
source("functions/utility.R", encoding = "UTF-8")


#ひとまず、ファイルの種類毎に後の処理が分けられるように、ファイル名から
#フラグ（type）を作成しておく
files <- util_list_all_files_in_data() %>% 
  filter(str_detect(dir4,"診療行為$")) %>% 
  mutate(type2 = str_extract(filename, "^.+(?=_)")) %>% 
  mutate(type1 = str_extract(filename, "都道府県別|性年齢別"))

#一つのエクセルに複数シートがある場合があるので、シート名を取得する
files <- files %>% 
  mutate(sheet_names = map(full_path, excel_sheets))



#年齢区分の一般的な形をTidyにする関数
wrangle_sinryou_koui_agekubun <- function(target_data){
  browser()
  #ぜんぶNAの行は削除する--------------------
  target_data <- target_data %>% 
    rowwise() %>% 
    filter(!all(is.na(c_across()))) %>% 
    ungroup()
  
  #ぜんぶNAの列は削除する-----------------------
  target_col <- target_data %>% 
    summarise(across(everything(), ~{
      all(is.na(.))
    }))
  
  target_col2 <- colnames(target_col)[unlist(target_col[1,])]
  
  target_data <- target_data %>% 
    select(!all_of(target_col2))
  
  #列名を作成する
  ##表が開始する行を取得
  
  ###改行コード\r\nが含まれている場合に除去
  first_column <- str_remove(target_data[[1]],"\\r\\n")
  target_data[[1]] <- first_column
  print(first_column)
  row_hyou_start <- which(str_detect(first_column,"^分類コード$|^加算$|^款$"))
  row_data_start <- which(!is.na(first_column))[2]
  dat <- target_data %>% slice(row_hyou_start:nrow(.))
  colname <- column_concatenator(dat,row_data_start - 1)
  
  
  #データのみにする
  dat_body <- dat %>% slice(row_data_start:nrow(.))
  
  pivot_longer_targets <- colname[1: (min(which(str_detect(colname,"男性_")==TRUE))-1)]
  
  #列名とデータをくっつけて縦持ちデータに変更する
  dat2 <- dat_body %>% 
    setNames(colname) %>% 
    fill(pivot_longer_targets[1:2]) %>% 
    mutate(across(everything(), ~{if_else(.=="-",NA_character_, .)})) %>% 
    pivot_longer(cols = !c(all_of(pivot_longer_targets)),
                 names_to=c("sex","age"),
                 names_sep="_",
                 values_to = "value") %>% 
    mutate(val2 = as.numeric(value))
  
  check_any_new_na_in_val2 <- dat2 %>% 
    filter(is.na(val2) & !is.na(value)) %>% 
    nrow()
  
  message_string <- str_c("NA generated in val2 is ",check_any_new_na_in_val2)
  if(check_any_new_na_in_val2 > 0){
    warning(str_c(message_string," dat is saved in obj"))
    obj <<- target_data
    
  }else{
    message(message_string)
  }
  
  dat3 <- dat2 %>% 
    mutate(value = val2) %>% 
    select(-val2)
  
  
  #加算の場合のフラグをここで立てる。
  is_kasan <- any(str_detect(colnames(dat3),"加算"))
  dat3 <- dat3 %>% mutate(kasan_flag = as.integer(is_kasan))
  
  colnames(dat3)[colnames(dat3)=="分類コード"]     <- "bunrui_code"
  colnames(dat3)[colnames(dat3)=="加算"]           <- "kasan"
  colnames(dat3)[colnames(dat3)=="款"]             <- "kan"
  colnames(dat3)[colnames(dat3)=="分類名称"]       <- "bunrui_name"
  colnames(dat3)[colnames(dat3)=="診療行為コード"] <- "sinryou_code"
  colnames(dat3)[colnames(dat3)=="診療行為"]       <- "sinryou"
  colnames(dat3)[colnames(dat3)=="点数"]           <- "tensu"
  colnames(dat3)[colnames(dat3)=="%(加減算)"]      <- "perc_change"
  colnames(dat3)[colnames(dat3)=="総計"]           <- "overall_total"
  
  
  return(dat3)  
}

cross_files <- files %>% filter(dir5 == "Cross")
age_files   <- files %>% filter(dir5 != "Cross" & type1 == "性年齢別")
pref_files  <- files %>% filter(dir5 != "Cross" & type1 == "都道府県別")

i <- 1
age_files2 <- age_files %>%
  select(full_path, sheet_names) %>% 
  unnest(c(sheet_names)) %>% 
  slice(45) %>% 
  mutate(data = map2(full_path, sheet_names, ~{
    print(i); i <<- i + 1
    print( str_c(.x,"___",.y))
    temp_data <- read_excel(path = .x, sheet = .y)
    wrangle_sinryou_koui_agekubun(temp_data)
  }))

temp <- age_files2 %>% 
  pull(data) %>% 
  reduce(bind_rows)

View(temp)
