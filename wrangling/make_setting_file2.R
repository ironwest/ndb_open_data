#make_setting_file-医科診療行為の設定ファイルを作成する。
#医科診療行為のデータをもとに、shinyアプリで利用する設定ファイルを作成する。
#保存先はapps/setting/
setwd(here::here())
library(tidyverse)

age_data <- read_rds("apps/data/processed/ika_sinryou/age_data.rds")
pref_data <- read_rds("apps/data/processed/ika_sinryou/pref_data.rds")

#ndbの回数と年を合わせる(NDB5回はレセプト平成30年度、特定健診平成29年度と年度がずれているのに
#注意
ndb <- tibble(
  ndb = as.character(c(1:5)),
  seireki = c(2014:2018)
)
write_rds(ndb,"apps/setting/ndb_nendo_nontokuho.rds",compress="gz")

# 医科診療報酬点数表項目―----------------------------------

bunrui_set <- age_data %>% 
  group_by(sinryou_code) %>% 
  summarise(sinryou = last(sinryou)) %>% ungroup()

write_rds(
  bunrui_set,
  file.path("apps","setting","ika_bunrui.rds"), compress="gz"
)

# 医科診療報酬点数表項目―シート名
write_rds(
  age_data$in_out %>% unique(),
  file.path("apps","setting","ika_bunrui_in_out.rds"), compress="gz"
)

# 医科診療報酬点数表項目-分類コード、診療コードマスター------------------------
# age_master <- age_data %>% 
#   select(sinryou_code, sinryou) %>% 
#   distinct()
# 
# age_master_dup <- age_master %>% count(sinryou_code) %>% filter(n > 1)
# 
# ##二回、診療コードが出現していることをチェック
# dup_check <- age_data %>% 
#   semi_join(age_master_dup, by="sinryou_code") %>% 
#   select(ndb, bunrui_dai, type2, in_out, sinryou_code, sinryou) %>% 
#   distinct()
# 
# dup_check %>% arrange(sinryou_code)
# 
# sinryou_master_age <- age_data %>% 
#   select(sinryou_code, sinryou) %>% 
#   distinct() %>% 
#   arrange(sinryou_code) %>% 
#   group_by(sinryou_code) %>% 
#   summarise(sinryou = last(sinryou))
# ## 診療行為名がNDB5回から大幅に変更（同じコードでも）
# ## マスタが変更になると、診療行為名が変更になる?とりあえず、重複していても、
# ## 診療行為コードがユニークであればよいようにアプリの処理は作成する必要あり。
# ## とりあえず5回の名前で検索できるようにマスターファイルを作成する
# ## 都道府県の診療行為についても同様に、処理
# 
# sinryou_master_pref <- pref_data %>% 
#   select(sinryou_code, sinryou) %>% 
#   distinct() %>% 
#   arrange(sinryou_code) %>% 
#   group_by(sinryou_code) %>% 
#   summarise(sinryou=last(sinryou))
# 
# #年齢と都道府県で同じ件数。
# sinryou_master_age %>% anti_join(sinryou_master_pref, by="sinryou_code")
# #否重複もなし
# 
# write_rds(
#   sinryou_master_age,
#   file.path("apps","setting","ika_sinryou_code.rds"), compress="gz"
# )

#年齢区分------------------------


age_data %>% 
  select(age) %>% 
  distinct() %>% 
  pull(age)
#表記ゆれがはなはだしい・・・
#またアルファベット順にggplotで描画されたら
#順番が狂うので、ここで因子型に変換しておく。

age_levels <- age_data %>% 
  select(age) %>% 
  distinct() %>% 
  pull(age)

clipr::write_clip(age_levels)

age_levels <- c("0～4歳",
"5～9歳",
"10～14歳",
"15～19歳",
"20～24歳",
"25～29歳",
"30～34歳",
"35～39歳",
"40～44歳",
"45～49歳",
"50～54歳",
"55～59歳",
"60～64歳",
"65～69歳",
"70～74歳",
"75～79歳",
"75-79歳",
"80～84歳",
"80-84歳",
"85～89歳",
"85-89歳",
"90歳以上")

age_labels <- 
  c("0-4",
    "5-9",
    "10-14",
    "15-19",
    "20-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50-54",
    "55-59",
    "60-64",
    "65-69",
    "70-74",
    "75-79",
    "75-79",
    "80-84",
    "80-84",
    "85-89",
    "85-89",
    ">=90")


age_data2 <- age_data %>% 
  mutate(age = factor(x = age, levels = age_levels, labels = age_labels))

age_data2 %>% count(age)

write_rds(
  age_labels,
  file.path("apps","setting","ika_age.rds"), compress="gz"
)

#性別----------------
age_data %>% 
  count(sex)

#女　と　男

#都道府県-----------------------
pref_setting <- pref_data %>% 
  select(prefid, prefname) %>% 
  distinct()

write_rds(
  pref_setting,
  file.path("apps","setting","ika_pref.rds"), compress="gz"
)

#点数表--------------------------
tensu <- age_data %>% 
  select(ndb, in_out, sinryou_code, tensu) %>% 
  distinct()

write_rds(
  tensu,
  file.path("apps","setting","ika_tensu.rds"), compress="gz"
)


#shiny アプリ用に年齢と都道府県データを軽量化する。
age_data_light <- age_data2 %>% 
  mutate(sex = if_else(sex == "男", "male", "female")) %>% 
  select(ndb, in_out, sinryou_code,  sex, age, value) 

pref_data_light <- pref_data %>% 
  select(ndb, in_out, sinryou_code, prefid, value)

write_rds(
  age_data_light, 
  file.path("apps","data","processed","ika_sinryou","age_data_light.rds"), compress="gz"
)

write_rds(
  pref_data_light,
  file.path("apps","data","processed","ika_sinryou","pref_data_light.rds"), compress="gz"
)
