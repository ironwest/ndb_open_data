

library(tidyverse)
library(readxl)
source("functions/excel_reshaper.R", encoding = "UTF-8")
source("functions/utility.R", encoding = "UTF-8")


#ひとまず、ファイルの種類毎に後の処理が分けられるように、ファイル名から
#フラグ（type）を作成しておく
files <- util_list_all_files_in_data() %>% 
  filter(str_detect(dir4,"処方薬")) %>% 
  filter(dir5 %in% c("内服","外用","注射"))
# %>% 
#   mutate(inout    = str_extract(filename, "入院|外来")) %>% 
#   mutate(out_ishp = str_extract(filename, "院内|院外")) %>% 
#   mutate(age_pref = str_extract(filename, "都道府県別|性年齢別"))

#files %>% count(dir5, inout, out_ishp, age_pref) #これで4つずつ(2-5回分)そろった。

#一つのエクセルに複数シートがある場合があるので、シート名を取得する
files <- files %>% 
  mutate(sheet_names = map(full_path, excel_sheets)) %>% 
  unnest(c(sheet_names))

files <- files %>% 
  mutate(inout = str_extract(sheet_names, "入院|外来")) %>% 
  mutate(out_ishp = str_extract(sheet_names, "院内|院外")) %>% 
  mutate(age_pref = str_extract(filename, "都道府県別|性年齢別"))

files %>% 
  select(dir5, inout, out_ishp, age_pref, sheet_names) %>% 
  unnest(sheet_names) %>% 
  count(dir5, inout, out_ishp, age_pref, sheet_names) #シート名でも全部4つずつあること確認。

#年齢別の形をTidyにする関数
wrangle_medicine_agekubun <- function(target_data, dev=FALSE){
  if(dev) browser()
  original_data <- target_data
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
  
  row_hyou_start <- which(str_detect(first_column,"^薬効分類"))
  
  #医薬品コードを利用(3列目)
  row_data_start <- which(!is.na(target_data[3]))[2]
  
  dat <- target_data %>% slice(row_hyou_start:nrow(.))
  colname <- column_concatenator(dat,row_data_start - 1)
  
  #debug用
  if(any(colname %in% c("sdfsdfsdfsdf"))){
    browser()
  }
  
  #データのみにする
  dat_body <- dat %>% slice(row_data_start:nrow(.))
  
  #表記ゆれ男性、男あり。
  pivot_longer_targets <- colname[1: (min(which(str_detect(colname,"(男性|男)_")==TRUE))-1)]
  
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
  
  #列名を英語にしておく（日本語だとShinyapps.ioでエラーが・・・）
  colnames(dat3)[colnames(dat3)=="薬効分類"]     <- "yakko_bunrui"
  colnames(dat3)[colnames(dat3)=="薬効分類名称"] <- "yakko_name"
  colnames(dat3)[colnames(dat3)=="医薬品コード"] <- "iyakuhin_code"
  colnames(dat3)[colnames(dat3)=="医薬品名"]     <- "iyakuhin_name"
  colnames(dat3)[colnames(dat3)=="薬価基準収載医薬品コード"] <- "yj_code"
  colnames(dat3)[colnames(dat3)=="薬価"]         <- "yakka"
  colnames(dat3)[colnames(dat3)=="後発品区分"]   <- "is_generic"
  colnames(dat3)[colnames(dat3)=="総計"]         <- "overall_total"
  colnames(dat3)[colnames(dat3)=="単位"]         <- "unit"
  
  #表記ゆれへの対応
  dat4 <- dat3 %>% 
    mutate(sex = case_when(
      sex == "男性" ~ "男",
      sex == "女性" ~ "女",
      TRUE ~ sex
    ))
  
  return(dat4)  
}

#都道府県の形をTidyにする関数
wrangle_medicine_pref <- function(target_data, dev=FALSE){
  if(dev) browser()
  original_data <- target_data
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
  
  row_hyou_start <- which(str_detect(first_column,"^薬効分類"))
  
  #最初の列を利用すると、分類コードない分類名称の診療行為がNDB5から
  #存在するため、2列目を利用
  row_data_start <- which(!is.na(target_data[2]))[2]
  
  dat <- target_data %>% slice(row_hyou_start:nrow(.))
  colname <- column_concatenator(dat,row_data_start - 1)
  
  #debug用
  if(any(colname %in% c("分類名称__オンライン医学管理料",
                        "診療行為コード__113023890",
                        "診療行為__オンライン医学管理料",
                        "点数__100",
                        "総計__480"))){
    browser()
  }
  
  #データのみにする
  dat_body <- dat %>% slice(row_data_start:nrow(.))
  
  pivot_longer_targets <- colname[1: (min(which(str_detect(colname,"01_")==TRUE))-1)]
  
  #列名とデータをくっつけて縦持ちデータに変更する
  dat2 <- dat_body %>% 
    setNames(colname) %>% 
    fill(pivot_longer_targets[1:2]) %>% 
    mutate(across(everything(), ~{if_else(.=="-",NA_character_, .)})) %>% 
    pivot_longer(cols = !c(all_of(pivot_longer_targets)),
                 names_to=c("prefid","prefname"),
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

  #列名を英語にしておく（日本語だとShinyapps.ioでエラーが・・・）
  colnames(dat3)[colnames(dat3)=="薬効分類"]     <- "yakko_bunrui"
  colnames(dat3)[colnames(dat3)=="薬効分類名称"] <- "yakko_name"
  colnames(dat3)[colnames(dat3)=="医薬品コード"] <- "iyakuhin_code"
  colnames(dat3)[colnames(dat3)=="医薬品名"]     <- "iyakuhin_name"
  colnames(dat3)[colnames(dat3)=="薬価基準収載医薬品コード"] <- "yj_code"
  colnames(dat3)[colnames(dat3)=="薬価"]         <- "yakka"
  colnames(dat3)[colnames(dat3)=="後発品区分"]   <- "is_generic"
  colnames(dat3)[colnames(dat3)=="総計"]         <- "overall_total"
  colnames(dat3)[colnames(dat3)=="単位"]         <- "unit"
  
  return(dat3)
}


#薬剤のWrangle---------------------------
age_files   <- files %>% 
  filter(age_pref == "性年齢別")

pref_files  <- files %>% 
  filter(age_pref == "都道府県別")

#年齢データの加工----------------------------
i <- 1
age_files2 <- age_files %>%
  unnest(c(sheet_names)) %>%
  #slice(1:nrow(.)) %>% 
  mutate(data = map2(full_path, sheet_names, ~{
    print(i); i <<- i + 1
    print( str_c(.x,"___",.y))
    temp_data <- read_excel(path = .x, sheet = .y)
    wrangle_medicine_agekubun(temp_data,FALSE)
  }))

temp <- age_files2 %>% 
  unnest(data)

res <- temp %>% 
  select(
    ndb = dir3,
    med_type = dir5,
    inout,
    out_ishp,
    age_pref,
    yakko_bunrui,
    yakko_name,
    iyakuhin_code,
    iyakuhin_name,
    unit,
    yj_code,
    yakka,
    is_generic,
    overall_total,
    sex, age,
    value
  )



dir.create(file.path("apps","data","processed","medicine"))
write_rds(res,file.path("apps","data","processed","medicine","age_data.rds"), compress="gz")

# 都道府県データのWrangle---------------------------
i <- 1
pref_files2 <- pref_files %>%
  unnest(c(sheet_names)) %>%
  mutate(data = map2(full_path, sheet_names, ~{
    print(i); i <<- i + 1
    print( str_c(.x,"___",.y))
    temp_data <- read_excel(path = .x, sheet = .y)
    wrangle_medicine_pref(temp_data,FALSE)
  }))

temp <- pref_files2 %>% 
  unnest(data)

res <- temp %>% 
  select(
    ndb = dir3,
    med_type = dir5,
    inout,
    out_ishp,
    age_pref,
    yakko_bunrui,
    yakko_name,
    iyakuhin_code,
    iyakuhin_name,
    unit,
    yj_code,
    yakka,
    is_generic,
    overall_total,
    prefid,
    prefname,
    value
  )

dir.create(file.path("apps","data","processed","medicine"))
write_rds(res,file.path("apps","data","processed","medicine","pref_data.rds"), compress="gz")


# クロス表-------------------------------------
i <- 1
cross_files2 <- cross_files %>%
  unnest(c(sheet_names)) %>%
  mutate(data = map2(full_path, sheet_names, ~{
    print(i); i <<- i + 1
    print( str_c(.x,"___",.y))
    temp_data <- read_excel(path = .x, sheet = .y)
    wrangle_sinryou_koui_cross(temp_data,FALSE)
  }))

temp <- cross_files2 %>% 
  unnest(data)

res <- temp %>% 
  select(
    ndb = dir3, bunrui_dai = dir5, cross_code, cross_name, in_out = sheet_names,
    overall_total, prefid, prefname, sex, age, value
  )

dir.create(file.path("apps","data","processed","ika_sinryou"))
write_rds(res,file.path("apps","data","processed","ika_sinryou","cross_data.rds"), compress="gz")
