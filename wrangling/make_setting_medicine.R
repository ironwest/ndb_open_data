#make_setting_medicine-医薬品データの設定ファイルの作成と、
#アプリにロードするデータの軽量化
#医科診療行為のデータをもとに、shinyアプリで利用する設定ファイルを作成する。
#保存先はapps/setting/
setwd(here::here())
library(tidyverse)

age_data <- read_rds("apps/data/processed/medicine/age_data.rds")
pref_data <- read_rds("apps/data/processed/medicine/pref_data.rds")


#薬効分類---------------------

yakko_age <- age_data %>% 
  select(yakko_bunrui, yakko_name)

yakko_age <- yakko_age %>% distinct()

yakko_pref <- pref_data %>% 
  select(yakko_bunrui, yakko_name) %>% 
  distinct()

yakko <- bind_rows(yakko_age, yakko_pref) %>% distinct()

write_rds(
  yakko,
  file.path("apps","setting","medicine_yakko.rds"),compress="gz"
)

age_data <- age_data %>% select(!yakko_name)
pref_data <- pref_data %>% select(!yakko_name)

#医薬品コード-------------------------
iyakuhin_tensu <- age_data %>% 
  select(ndb, med_type,yakko_bunrui, iyakuhin_code, 
         iyakuhin_name, unit, yj_code, yakka, is_generic, yakka) %>% 
  distinct()

iyakuhin_tensu %>% slice(1:100) %>%  pull(1) %>% guess_encoding()
iyakuhin_tensu %>% slice(1:100) %>%  pull(2) %>% guess_encoding()
iyakuhin_tensu %>% slice(1:100) %>%  pull(3) %>% guess_encoding()
iyakuhin_tensu %>% slice(1:100) %>%  pull(4) %>% guess_encoding()
iyakuhin_tensu %>% slice(1:100) %>%  pull(5) %>% guess_encoding()
iyakuhin_tensu %>% slice(1:100) %>%  pull(6) %>% guess_encoding()
iyakuhin_tensu %>% slice(1:100) %>%  pull(7) %>% guess_encoding()
iyakuhin_tensu %>% slice(1:100) %>%  pull(8) %>% guess_encoding()
iyakuhin_tensu %>% slice(1:100) %>%  pull(9) %>% guess_encoding()

iyakuhin_tensu <- iyakuhin_tensu %>% 
  mutate(med_type = utf8::as_utf8(med_type))

#医薬品コード（レセプト電算処理システム用コード）は製品毎
#Ｙｊコードは区分含めて絞り込むのに使える。

age_data2 <- age_data %>% 
  select(ndb, med_type, inout, out_ishp, iyakuhin_code, yj_code, sex, age, value)
pref_data2 <- pref_data %>% 
  select(ndb, med_type, inout, out_ishp, iyakuhin_code, yj_code, prefid , prefname,  value)

age_data2 %>% 
  left_join(iyakuhin_tensu, by = c("ndb","med_type","iyakuhin_code","yj_code"))

write_rds(
  iyakuhin_tensu,
  file.path("apps","setting","iyakuhin_tensu_master.rds"),compress="gz"
)

iyakuhin_master <- iyakuhin_tensu %>% 
  select(iyakuhin_code, yj_code, iyakuhin_name, is_generic) %>% 
  distinct()

write_rds(
  iyakuhin_master,
  file.path("apps","setting","iyakuhin_master.rds"), compress="gz"
)

#一応、ここで作成した医薬品のマスタと、年齢区分、都道府県データをJoin
#して件数が変化しないかを確認する。

#年齢区分------
vec_age <- age_data2$age %>% unique()

age_level <- c(
  "0～4歳"  ,"5～9歳"  ,"10～14歳","15～19歳","20～24歳",
  "25～29歳","30～34歳","35～39歳","40～44歳","45～49歳",
  "50～54歳","55～59歳","60～64歳","65～69歳","70～74歳",
  "75～79歳","80～84歳","85～89歳","90～94歳","95～99歳",
  "90歳以上","100歳以上"
)

age_data2 <- age_data2 %>% 
  mutate(age = factor(age, levels = vec_age))

##########
age_data2$med_type <- stringi::stri_enc_toutf8(age_data2$med_type)

write_rds(
  age_data2,
  file.path("apps","data","processed","medicine","age_data_light.rds"),compress="gz"
)

write_rds(
  age_level,
  file.path("apps","setting","med_age.rds"),compress="gz"
)


pref_data2 <- pref_data2 %>% 
  mutate(prefid = factor(prefid), 
         prefname = factor(prefname))

pref_data3 <- pref_data2 %>% 
  mutate(med_type = utf8::as_utf8(med_type))

pref_data3 %>% slice(1:100) %>% pull(1) %>% guess_encoding()
pref_data3 %>% slice(1:100) %>% pull(2) %>% guess_encoding()
pref_data3 %>% slice(1:100) %>% pull(3) %>% guess_encoding()
pref_data3 %>% slice(1:100) %>% pull(4) %>% guess_encoding()
pref_data3 %>% slice(1:100) %>% pull(5) %>% guess_encoding()
pref_data3 %>% slice(1:100) %>% pull(6) %>% guess_encoding()
pref_data3 %>% slice(1:100) %>% pull(7) %>% guess_encoding()
pref_data3 %>% slice(1:100) %>% pull(8) %>% guess_encoding()
a <- pref_data3 %>% slice(1:1) %>% pull(9)



pref_data3$med_type[1:100] %>% guess_encoding()

write_rds(
  pref_data3,
  file.path("apps","data","processed","medicine","pref_data_light.rds"), compress="gz"
)

#総使用量を作成する
dat_overall_age <- age_data %>% 
  select(ndb, med_type, inout, out_ishp, iyakuhin_code, yj_code, overall_total) %>% 
  distinct()

dat_overall_pref <- pref_data %>% 
  select(ndb, med_type, inout, out_ishp, yakko_bunrui,
         iyakuhin_code,  yj_code, iyakuhin_name, unit,overall_total) %>% 
  distinct()

#総使用量、年齢別と都道府県別のデータで微妙に集計後の数値が違う。
#そのため、都道府県の数値を利用してoverallデータを作成する。
dat_overall <- dat_overall_age %>% mutate(type = "age") %>% 
  bind_rows(dat_overall_pref %>% mutate(type = "pref"))

# temp <- dat_overall %>% head(100)
# temp$med_type <- stringi::stri_enc_toutf8(temp$med_type)
# 
# map(temp,guess_encoding)

dat_overall_pref$med_type <- stringi::stri_enc_toutf8(dat_overall_pref$med_type)

write_rds(
  dat_overall_pref,
  file.path("apps","data","processed","medicine","overall.rds"), compress="gz"
)


#年度をくっつけるtibbleを作成
nendo <- tibble(
  ndb = as.character(c(2:5)),
  nendo = c(2015:2018)
)

write_rds(
  nendo,
  file.path("apps","setting","meidicine_nendo.rds"), compress="gz"
)








