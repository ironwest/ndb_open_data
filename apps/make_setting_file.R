#make_setting_file
#特定健康診断のデータをもとに、shinyアプリで利用する設定ファイルを作成する。
#保存先はapps/setting/
setwd(here::here())
library(tidyverse)

kensin_data <- read_rds("apps/data/processed/tokutei_kensin/data.rds")

#ndbの回数と年を合わせる(NDB5回はレセプト平成30年度、特定健診平成29年度と年度がずれているのに
#注意
ndb <- tibble(
  ndb = as.character(c(1:5)),
  seireki = c(2013:2017)
)
write_rds(ndb,"apps/setting/ndb_nendo.rds",compress="gz")


#検査項目-------------------------------
kensa_koumoku <- kensin_data %>% 
  select(kensa_koumoku) %>% 
  distinct()

#アプリで利用する検査項目を絞り込む
#（BMI、AST、ALT、HbA1c、HDL、LDL、GTP、拡張期血圧、収縮期血圧
#　拡張期血圧、中性脂肪、腹囲のみとする）
target_kensa_koumoku <- c("BMI","AST","ALT","HbA1c","HDL","LDL","GTP",
                          "拡張期血圧","収縮期血圧","中性脂肪","腹囲")

kensa_koumoku <- kensa_koumoku %>% 
  filter(str_detect(kensa_koumoku, str_c(target_kensa_koumoku,collapse="|")))

write_rds(kensa_koumoku, "apps/setting/ndb_kensa_koumoku.rds",compress="gz")

#都道府県-----------------------
pref_data <- kensin_data %>% 
  select(long_data) %>% 
  unnest(c(long_data)) %>% 
  select(`都道府県`) %>% 
  distinct() %>% 
  filter(`都道府県` != "全国") %>% 
  rename(pref = `都道府県`)

write_rds(pref_data, "apps/setting/ndb_prefecture.rds",compress="gz")

#検査項目毎の検査区分------------------
kaisou <- kensin_data %>% 
  semi_join(kensa_koumoku, by = "kensa_koumoku") %>% 
  select(kensa_koumoku ,long_data) %>% 
  unnest(c(long_data))

kaisou <- kaisou %>% 
  select(kensa_koumoku, kaisou = `検査値階層`) %>% 
  distinct()

pull_vec <- function(num){
  kaisou %>% filter(str_detect(kensa_koumoku,target_kensa_koumoku[num])) %>% pull(kaisou)
}

bmi <- pull_vec(1)
ast <- pull_vec(2)
alt <- pull_vec(3)
a1c <- pull_vec(4)
hdl <- pull_vec(5)
ldl <- pull_vec(6)
gtp <- pull_vec(7)
dbp <- pull_vec(8)
sbp <- pull_vec(9)
tg  <- pull_vec(10)
huk <- pull_vec(11)

#levels
bmi_lev <- bmi[c(5,4,3,2,1)] 
ast_lev <- ast[c(1,2,4,3)]  
alt_lev <- alt[c(1,2,4,3)]
a1c_lev <- a1c[c(6,5,4,3,2,1)]
hdl_lev <- hdl[c(1,2,3)]
ldl_lev <- ldl[c(6,5,4,3,2,1)]
gtp_lev <- gtp[c(1,2,4,3)]
sbp_lev <- sbp[c(6,5,4,3,2,1)]
dbp_lev <- dbp[c(6,5,4,3,2,1)]
tg_lev  <- tg [c(3,2,1)]
huk_lev <- huk[c(3,2,1)]

#labels
bmi_lab <- bmi_lev
ast_lab <- ast_lev[c(1,3,3,4)]
alt_lab <- alt_lev[c(1,3,3,4)]
a1c_lab <- a1c_lev
hdl_lab <- hdl_lev
ldl_lab <- ldl_lev
gtp_lab <- gtp_lev[c(1,3,3,4)]
sbp_lab <- sbp_lev
dbp_lab <- dbp_lev
tg_lab  <- tg_lev
huk_lab <- huk_lev

kubun <- list()
kubun$lev <- list(
  bmi = bmi_lev,
  ast = ast_lev,
  alt = alt_lev,
  a1c = a1c_lev,
  hdl = hdl_lev,
  ldl = ldl_lev,
  gtp = gtp_lev,
  sbp = sbp_lev,
  dbp = dbp_lev,
  tg = tg_lev,
  huk = huk_lev
)

kubun$lab <- list(
  bmi = bmi_lab,
  ast = ast_lab,
  alt = alt_lab,
  a1c = a1c_lab,
  hdl = hdl_lab,
  ldl = ldl_lab,
  gtp = gtp_lab,
  sbp = sbp_lab,
  dbp = dbp_lab,
  tg = tg_lab,
  huk = huk_lab
)

write_rds(kubun, "apps/setting/ndb_kensa_kubun.rds",compress="gz")


#年齢の区分-------------------------
age_kubun <- kensin_data %>% 
  select(long_data) %>% 
  unnest(c(long_data)) %>% 
  select(age) %>% 
  distinct()

write_rds(age_kubun,"apps/setting/ndb_age_kubun.rds",compress="gz")


#問診項目-------------------
monsin <- read_rds("apps/data/processed/tokutei_monsin/question.rds")

qs <- monsin %>% 
  select(file_qnum, qtext) %>% 
  distinct() %>% 
  mutate(qtext = str_replace_all(qtext, "\\s|　|：","")) %>% 
  mutate(qnum = as.numeric(file_qnum)) %>% 
  select(qnum, qtext) %>% 
  arrange(qnum)

qshort <- tribble(
  ~qnum, ~short,
  1    , "01.内服(血圧)",
  2    , "02.内服(血糖)/インスリン",
  3    , "03.内服(脂質)",
  4    , "04.既往歴(脳卒中)",
  5    , "05.既往歴(心臓病)",
  6    , "06.既往歴(CKD/透析)",
  7    , "07.既往歴(貧血)",
  8    , "08.喫煙習慣",
  9    , "09.20歳から10kg増加",
  10   , "10.汗をかく運動",
  11   , "11.身体活動",
  12   , "12.歩く速度",
  13   , "13.体重増減3kg/年",
  14   , "14.たべる速度",
  15   , "15.就寝前の食事",
  16   , "16.夕食後の間食",
  17   , "17.朝食欠食",
  18   , "18.飲酒頻度",
  19   , "19.1日あたり飲酒量",
  20   , "20.睡眠での休養",
  21   , "21.生活習慣の改善意思",
  22   , "22.保健指導の利用"
)

qs <- left_join(qs,qshort,by="qnum")

write_rds(qs, "apps/setting/ndb_monsin.rds", compress="gz")
