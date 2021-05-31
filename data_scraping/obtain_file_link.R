library(tidyverse)
library(rvest)

mhlw <- "https://www.mhlw.go.jp"
rooturl <- "https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000177182.html"
rootdat <- read_html(rooturl)

links <- tibble(
  text = html_text(html_nodes(rootdat,"a")),
  url  = html_attr(html_nodes(rootdat,"a"),"href")
) %>% 
  filter( str_detect(text,"回NDB")) %>% 
  mutate(url = if_else(str_detect(url, "^https"), url, str_c(mhlw,url)))

make_xls_url <- function(kai,aurl){
  
  rootpage <- read_html(aurl)
  
  target_content <- html_nodes(rootpage,xpath = ".//h3|.//h4|.//a")
  
  urls <- tibble(
    tag = html_name(target_content),
    txt = html_text(target_content),
    url = html_attr(target_content,name="href")
  ) %>% 
    mutate(h3 = if_else(tag == "h3", txt, NA_character_)) %>% 
    mutate(h4 = case_when(
      tag == "h4" ~ txt,
      tag == "h3" ~ "h3",
      TRUE ~ NA_character_
    )) %>%
    fill(h3, h4) %>% 
    filter(!is.na(url)) %>%
    mutate(kai = kai) %>% 
    select(kai, h3,h4,txt,url) %>% 
    mutate(url = if_else(str_detect(url,"^https"), url, str_c(mhlw,url))) %>% 
    filter(str_detect(url,"xls(x|)$"))  
  
  return(urls)
}

links2 <- links %>%
  mutate(kai = 1:5) %>% 
  mutate(link_tibble = map2(kai, url, make_xls_url)) %>% 
  select(link_tibble) %>% 
  unnest(link_tibble) %>% 
  mutate(h4 = str_replace(h4,"h3",""))

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb,"links")
openxlsx::writeData(wb,"links",links2)
openxlsx::saveWorkbook(wb,"data/links_excel.xlsx",overwrite = TRUE)
