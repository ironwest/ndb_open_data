library(tidyverse)
library(readxl)
source("functions/excel_reshaper.R", encoding = "UTF-8")
source("functions/utility.R", encoding = "UTF-8")


files <- util_list_all_files_in_data() %>% 
  filter(str_detect(dir4,"^特定健診$")) %>% 
  mutate(kensa_koumoku = str_extract(filename,".+(?=都道府県別)")) %>% 
  mutate(kensa_koumoku = str_replace_all(kensa_koumoku," |　",""))

#kensa_koumokuで、HbA1cとHbA1Cと表記の揺れがあるので、小文字に変換
files <- files %>% 
  mutate(kensa_koumoku = if_else(kensa_koumoku == "HbA1C", "HbA1c", kensa_koumoku))

#標準形をTidyにする関数
wrangle_tokutei_kensin_data <- function(target_data){
  
  #ぜんぶNAの列は削除する--------------------
  target_data <- target_data %>% 
    rowwise() %>% 
    filter(!all(is.na(c_across()))) %>% 
    ungroup()
  
  #列名を作成する
  #表が開始する行を取得
  row_hyou_start <- which(str_detect(target_data[[1]],"都道府県"))
  row_data_start <- which(str_detect(target_data[[1]],"北海道"))
  dat <- target_data %>% slice(row_hyou_start:nrow(.))
  colname <- column_concatenator(dat,row_data_start - 1)
  
  #NDB2のみ「都道府県名」ほかの年度は「都道府県」のため、都道府県に統一
  colname[colname=="都道府県名"] <- "都道府県"
  
  #検査値階層に単位がついているため、のちにデータをグラフかするときに
  #面倒となるので、ここで修正
  unit_for_kensa <- str_extract(colname[2],"(?<=検査値階層).+")
  colname[2] <- "検査値階層"
  
  #データのみにする
  dat_body <- dat %>% slice(row_data_start:nrow(.))
  
  pivot_longer_target1 <- colname[1]
  pivot_longer_target2 <- colname[2]
  
  #列名とデータをくっつけて縦持ちデータに変更する
  
  dat2 <- dat_body %>% 
    setNames(colname) %>% 
    fill("都道府県") %>% 
    select(!matches("中計")) %>% 
    pivot_longer(cols = !c(matches(pivot_longer_target1), 
                           matches(pivot_longer_target2)),
                 names_to=c("type","sex","age","unit"),
                 names_pattern="(.+?_|)(.+)_(.+)_(.+)",
                 values_to = "value")
  
  #値を文字列型から数値型に
  dat2 <- dat2 %>% 
    mutate(val2 = as.numeric(value))
  
  check_any_na_in_val2 <- dat2 %>% 
    filter(is.na(val2)) %>% 
    nrow()
  
  message_string <- str_c("NA generated in val2 is ",check_any_na_in_val2)
  if(check_any_na_in_val2 > 0){
    warning(str_c(message_string," dat is saved in obj"))
    obj <<- target_data
    
  }else{
    message(message_string)
  }
  
  dat3 <- dat2 %>% 
    mutate(value = val2) %>% 
    select(-val2) %>% 
    mutate(kensa_unit = unit_for_kensa) %>% 
    rename(pref = `都道府県`, kaisou = `検査値階層`)
  
  return(dat3)  
}

#ヘモグロビンのデータが、形違うので、読み込みつつ処理を変更する関数
read_file_from_path <- function(filepath){
  print(filepath)
  
  dat <- read_excel(filepath)
  
  #ヘモグロビンのデータかどうかで返すデータを変更する。
  is_haemoglobin_file <- str_detect(filepath,"ヘモグロビン")
  
  #データの加工
  if(is_haemoglobin_file){
    
    dat_male   <- dat %>% select(1:10)  %>% wrangle_tokutei_kensin_data()
    dat_female <- dat %>% select(11:20) %>% wrangle_tokutei_kensin_data()
    
    result <- bind_rows(dat_male, dat_female)
  }else{
    
    result <- dat %>% wrangle_tokutei_kensin_data() 
    
  }
  
  
  return(result)
}


#データを処理する----------------
#
#尚、ヘモグロビンデータの形が他と違うので別に処理する
haemoglobin_files <- files %>% 
  filter( str_detect(kensa_koumoku,"ヘモグロビン"))

teikei_files <- files %>% 
  filter( !str_detect(kensa_koumoku,"ヘモグロビン"))

#ヘモグロビンデータ---------------------
load_data_haem <- haemoglobin_files %>% 
  mutate(long_data = map(full_path, read_file_from_path))

#ヘモグロビン以外のデータ------------------------
load_data_else <- teikei_files %>% 
  mutate(long_data = map(full_path, read_file_from_path))

load_data <- bind_rows(load_data_haem, load_data_else)

#データを保存する-----------------------
dir.create(
  file.path("apps","data","processed","tokutei_kensin"), 
  recursive=TRUE, 
  showWarnings = FALSE
)

write_rds(
  x = load_data,
  file = file.path("apps","data","processed","tokutei_kensin","data.rds"),
  compress = "gz"
)
