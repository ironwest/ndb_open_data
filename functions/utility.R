library(tidyverse)

#dataフォルダにある全ファイルをtibble形式で取得
util_list_all_files_in_data <- function(){
  
  lf <- list.files("data/ndb", recursive = TRUE, pattern = "xlsx$")
  
  res <- tibble(
    dir1 = "data",
    dir2 = "ndb",
    lf = lf
  ) %>% 
    separate(col="lf", into=c("dir3","dir4","dir5","filename"), sep="/") %>% 
    mutate(full_path = file.path(dir1,dir2,dir3,dir4,dir5,filename))
  
  return(res)
}


#全角数字を含む文字列を半角数字に置き換える
util_replace_zenkakunum <- function(zenkaku){
  
  replacer <- c(
    "０" = "0",
    "１" = "1",
    "２" = "2",
    "３" = "3",
    "４" = "4",
    "５" = "5",
    "６" = "6",
    "７" = "7",
    "８" = "8",
    "９" = "9"
  )
  
  return(map_chr(zenkaku, ~{str_replace_all(.,replacer)}))

}

#全角数字をひっかける正規表現を作成
reg_zenkaku <- function(){
  return("[１２３４５６７８９０]")
}
