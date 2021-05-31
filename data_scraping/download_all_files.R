library(tidyverse)
library(rvest)

dat <- readxl::read_excel("data/links_excel_modified.xlsx")

#h4項目の表記ゆれや空白を削除。
#h4項目がない場合はそのままh3項目を踏襲
dat <- dat %>% 
  mutate(h4 = str_trim(h4)) %>% 
  mutate(h4 = str_replace_all(h4,"　","")) %>% 
  mutate(h4 = case_when(
    is.na(h4) ~ h3,
    h4 == "" ~ h3,
    TRUE ~ h4
  )) %>% 
  mutate(txt = str_replace_all(txt,"【|】",""))

#フォルダを作成する

temp <- dat %>%  
  group_by(kai,h3) %>% 
  nest()


#tempのデータを利用してフォルダにファイルをダウンロードする関数
download_files <- function(dir1,dir2,data){
  
  #dir1とdir2を作る
  new_dir <- file.path("data","ndb",dir1,dir2)
  
  if(dir.exists(new_dir)){
    #do nothing
  }else{
    dir.create(new_dir,recursive = TRUE)
  }
  
  dir_list <- data$h4 %>% unique()
  
  walk(file.path(new_dir,dir_list), ~{
    if(dir.exists(.)){
      #do nothing
    }else{
      dir.create(.)  
    }
  })
  
  #作成したところにファイルをダウンロードする
  data <- data %>% 
    mutate(dl_to = file.path(new_dir,h4,str_c(txt,".xlsx"))) %>% 
    mutate(dl_to = str_replace(dl_to,"\u00A0",""))
  
  walk2(data$url, data$dl_to, ~{
    print(str_c(.y))
    Sys.sleep(1) #サーバーに負担をかけないように、1秒おきにダウンロード
    download.file(url = .x, destfile = .y, quiet = TRUE, mode = "wb")
  })
}

#ダウンロードする
pmap(list(temp$kai,temp$h3,temp$data), ~{
  download_files(..1,..2,..3)
})

