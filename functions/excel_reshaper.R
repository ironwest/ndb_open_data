library(tidyverse)

column_concatenator <- function(.data, initial_rows){
  ddd  <- .data %>% 
    slice(1:initial_rows)
  
  ddd <- ddd %>% 
    t() %>% 
    as_tibble(.name_repair = "unique") %>% 
    fill(everything()) %>% 
    mutate(across(everything(), ~{replace_na(.,"")})) %>% 
    rowwise()
  
  unite_these <- colnames(ddd)
  
  ddd <- ddd %>% 
    unite(unite_these)
  
  concatenated_column_name <- ddd$unite_these %>% 
    str_replace_all(.,"_+$","") %>% 
    str_replace_all(.,"\r\n","")
  
  return(concatenated_column_name)
    
}
