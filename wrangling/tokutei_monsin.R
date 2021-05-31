library(tidyverse)
library(readxl)
source("functions/excel_reshaper.R", encoding = "UTF-8")
source("functions/utility.R", encoding = "UTF-8")

files <- util_list_all_files_in_data() %>% 
  filter(str_detect(dir4,"質問票")) %>% 
  mutate(qnum = str_extract(filename, str_c(reg_zenkaku(),"+"))) %>% 
  mutate(qnum2 = util_replace_zenkakunum(qnum))

wrangle_tokutei_monsin_file <- function(filepath){
  print(filepath)
  
  #データを読み込む---------------
  rawdat <- read_excel(path=filepath,col_names = FALSE)
  
  #ぜんぶNAの列は削除する--------------------
  rawdat <- rawdat %>% 
    rowwise() %>% 
    filter(!all(is.na(c_across()))) %>% 
    ungroup()
  
  #列名を作成する
  dat <- rawdat %>% slice(2:nrow(.))
  colname <- column_concatenator(dat,4)
  
  #NDB2のみ「都道府県名」ほかの年度は「都道府県」のため、都道府県に統一
  colname[colname=="都道府県名"] <- "都道府県"
  
  #データのみにする
  dat_body <- dat %>% slice(5:nrow(.))

  #列名とデータをくっつけて縦持ちデータに変更する
  dat2 <- dat_body %>% 
    setNames(colname) %>% 
    fill("都道府県") %>% 
    select(!matches("中計")) %>% 
    pivot_longer(cols = !c(`都道府県`,`回答`),
                 names_to=c("type","sex","age","unit"),
                 names_sep="_",
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
    obj <<- rawdat
    
  }else{
    message(message_string)
  }
  
  dat3 <- dat2 %>% 
    mutate(value = val2) %>% 
    select(-val2)
  
  dat4 <- dat3 %>% 
    rename(pref = `都道府県`, answer = `回答`)

  return(dat4)  
}

#ファイルから問診内容を抜き出す関数
extract_monsin_content <- function(filepath){
  #データを読み込む---------------
  dat <- read_excel(path=filepath,col_names = FALSE)
  
  #問診内容を抜き出す---------------
  raw <- as.character(dat[1,1])
  
  qnum <- str_extract(raw,"(?<=質問項目).+?(?=）)")
  qtext <- str_extract(raw,"(?<=）).+?(：)")
  
  return(tibble(qnum=qnum, qtext=qtext))
}


#データを処理する----------------
load_data <- files %>% 
  mutate(question_text = map(full_path, extract_monsin_content)) %>% 
  mutate(long_data = map(full_path, wrangle_tokutei_monsin_file)) %>% 
  select(ndb = dir3, file_qnum = qnum2, question_text, long_data)

#読み込んだデータの件数を列に出す
load_data <- load_data %>% 
  mutate(tibble_rows = map_int(long_data, nrow))

#load_data %>% select(ndb, file_qnum, tibble_rows) %>% View()

#保存用にload_dataを分割する----------------
tokutei_monsin_question <- load_data %>% 
  select(ndb, file_qnum, question_text) %>% 
  unnest(c(question_text)) %>% 
  arrange(qnum)

tokutei_monsin_data <- load_data %>% 
  select(ndb, file_qnum, long_data) %>% 
  unnest(c(long_data))

#データを保存する-----------------------
dir.create(
  file.path("apps","data","processed","tokutei_monsin"), 
  recursive=TRUE, 
  showWarnings = FALSE
)

write_rds(
  x = tokutei_monsin_question,
  file = file.path("apps","data","processed","tokutei_monsin","question.rds"), 
  compress="gz"
)

write_rds(
  x = tokutei_monsin_data,
  file = file.path("apps","data","processed","tokutei_monsin","data.rds"),
  compress = "gz"
)
