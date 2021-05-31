library(tidyverse)

ken <- read_rds("apps/data/processed/tokutei_kensin/data.rds")


#arguments
koumoku <- "収縮期血圧"

kensa_data <- ken %>% 
  filter(kensa_koumoku == koumoku) %>% 
  select(ndb = dir3, kensa_koumoku, long_data) %>% 
  unnest(c(long_data))

#全国平均と都道府県のグラフを作成するためのデータ
zenkoku <- kensa_data 


#切り取る区分としては都道府県、検査値階層、性別、年齢の4つ。
#今回は事業場向けなので、
#日本全国と都道府県を絞り込んだ2パターンを表示。
#（都道府県の比較は行わない
#
#日本全国:
tot_all <- ken %>% 
  group_by(ndb) %>% 
  summarise(total = sum(value,na.rm=TRUE))



ques <- read_rds("apps/data/processed/tokutei_monsin/question.rds")
dat <- read_rds("apps/data/processed/tokutei_monsin/data.rds")

ques

ddd <- dat %>% 
  filter(file_qnum == 1)

#全体の人数を集計する
total_ndb <- ddd %>% 
  group_by(ndb) %>% 
  summarise(total_ndb = sum(value,na.rm=TRUE))

#各都道府県の全体人数を集計する
total_ndb_prefecture <- ddd %>% 
  group_by(ndb, `都道府県`) %>% 
  summarise(total_pref = sum(value,na.rm=TRUE))

#都道府県、男女別の全体人数を集計する
total_ndb_prefecture_sex <- ddd %>% 
  group_by(ndb, `都道府県`,sex) %>% 
  summarise(tot_pref = sum(value,na.rm=TRUE))

#日本全国の回答の割合を集計する
res_ndb <- ddd %>% 
  group_by(ndb,`回答`) %>% 
  summarise(value = sum(value,na.rm=TRUE)) %>% 
  left_join(total_ndb, by="ndb") %>% 
  mutate(perc = value/total_ndb)

#都道府県の回答の割合を集計する
res_ndb_prefecture <- ddd %>% 
  group_by(ndb,`都道府県`,`回答`) %>% 
  summarise(value = sum(value,na.rm=TRUE)) %>% 
  left_join(total_ndb_prefecture, by = c("ndb","都道府県")) %>% 
  mutate(perc = value/total_pref)

res_ndb_prefecture %>% 
  filter(`都道府県` == "愛知県") %>% 
  filter(`回答` == "はい") %>% 
  
  ggplot() + 
  geom_col(aes(x = ndb, y = perc)) +
  scale_y_continuous(labels = scales::percent) +
  ylim(0,1)
