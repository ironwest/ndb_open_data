library(tidyverse)

#ここでは、ダウンロードしたエクセルファイルすべてを
#tidyな形にして、Shinyアプリケーションで
#楽に利用できる形にしてみましょう。

#まずは、ひとつだけファイルを読み込んでtidyなデータにしてみましょう


dat <- readxl::read_excel("data/ndb2/BMI　都道府県別性年齢階級別分布.xlsx")


#まずは列名にするデータをつくります。
#
colnamedat <- tibble(
  gender = dat %>% slice(2) %>% unlist(),
  age    = dat %>% slice(3) %>% unlist()
) %>% 
  fill(gender, .direction = "down")

colnamedat[1,1] <- "prefname"
colnamedat[2,1] <- "kubun"

coln <- colnamedat %>% 
  unite(col = "coln", c("gender", "age"), na.rm = TRUE) %>% 
  pull(coln)


valuedat <- dat %>% 
  slice(5:n()) %>% 
  setNames(.,coln)

valuedat <- valuedat %>% 
  fill(prefname, .direction = "down")

res <- valuedat %>% 
  pivot_longer(cols = c(-prefname, -kubun),
               names_to = c("gender","age"), 
               names_sep = "_") %>% 
  filter(age != "中計")

basic_data <- colnames(dat)[1] %>% str_split(pattern = "：")

res <- res %>% 
  mutate(type = basic_data[[1]][1],
         nendo = basic_data[[1]][2])

#できあがり。
#さて、問題はこの変換が他のデータで問題なく動くかですが、

#関数化して、適応するのをやってみましょう！
#次の動画で。

#lec--------------------------


#では、関数化に取り組んでみましょう。
#とは言っても、ひとつ前の動画で作成したスクリプトを
#コピペして、ファイルパスをアーギュメントにするだけです

process_excel <- function(filepath){
  dat <- readxl::read_excel(filepath)

  colnamedat <- tibble(
    gender = dat %>% slice(2) %>% unlist(),
    age    = dat %>% slice(3) %>% unlist()
  ) %>% 
    fill(gender, .direction = "down")
  
  colnamedat[1,1] <- "prefname"
  colnamedat[2,1] <- "kubun"
  
  coln <- colnamedat %>% 
    unite(col = "coln", c("gender", "age"), na.rm = TRUE) %>% 
    pull(coln)
  
  
  valuedat <- dat %>% 
    slice(5:n()) %>% 
    setNames(.,coln)
  
  valuedat <- valuedat %>% 
    fill(prefname, .direction = "down")
  
  res <- valuedat %>% 
    pivot_longer(cols = c(-prefname, -kubun),
                 names_to = c("gender","age"), 
                 names_sep = "_") %>% 
    filter(age != "中計")
  
  basic_data <- colnames(dat)[1] %>% str_split(pattern = "：")
  
  res <- res %>% 
    mutate(type = basic_data[[1]][1],
           nendo = basic_data[[1]][2])
  
  return(res)
}

temp1 <- process_excel("data/ndb1/BMI　都道府県別性年齢階級別分布.xlsx")
temp2 <- process_excel("data/ndb2/GOT（AST）　都道府県別性年齢階級別分布.xlsx")
temp3 <- process_excel("data/ndb3/LDLコレステロール 都道府県別性年齢階級別分布.xlsx")
temp4 <- process_excel("data/ndb4/中性脂肪 都道府県別性年齢階級別分布.xlsx")

temp1
temp2
temp3
temp4

#予想以上にうまく動いています。
#ただ、ndb3から、

temp3$nendo[1]

#年度の表記のところで、1回目、2回目にはなかった、
#※集計結果が10未満の場合は「‐」で表示（10未満の箇所が1箇所の場合は総計以外全て「‐」で表示）
#という文言が入っているのでこれは後で対応しましょう。
#(ついでにいうと、文字列で、-という表記が数字の中に紛れている
#可能性もあることは留意しつつ、文字列を数字に変換するときには
#注意が必要となりそうです。)

#それでは、次の動画では上記のprocess_excel関数を利用して、ダウンロードされたすべての
#エクセルファイルを一つのtibbleにまとめていきます。

#Lec-すべてのダウンロードデータの読み込み-----------

#それでは、ダウンロードされたすべてのエクセルファイルを取り込みましょう。
#map関数を利用して行っていくのですが、練習してみたい人は動画を止めて
#やってみてください。


#やってみましたか？

#それでは始めます。

#まず、読み込み対象となるファイルを列名として持つtibbleを作成します。

dat <- tibble(filepath = list.files("data", 
                                    pattern = "xlsx$", 
                                    full.names = TRUE,
                                    recursive = TRUE))


#次に、map関数を利用して、データを読み込みましょう
dat <- dat %>% 
  mutate(data = map(filepath, process_excel))

dat2 <- dat %>% 
  select(data) %>% 
  unnest(data)

View(dat2)

#どうでしょうか？13万行のデータとなっていますね？
#次の動画では、読み込んだデータの型変換等を実施していきます。

#Lec-読み込んだデータのチェックと型変換-------------------

#まずは、取り込んだデータのチェックをしましょう。

dat2$type %>% unique()

#はい、HbA1cが二つあるのですが、これは、エクセルファイル名に大文字のCと
#小文字のcで表記にゆれがあったからでわかっていたことです。
#とりあえず、特定健診（）の（も半角全角が入り混じっているっぽいので
#きれいにしましょう。

dat2 <- dat2 %>% 
  mutate(type2 = str_replace_all(type,"特定健診|\\(|\\)|（|）",""))

dat2$type2 %>% unique()

#あとは、GOT、GPTも消しておきましょう

dat2 <- dat2 %>% 
  mutate(type2 = str_replace_all(type,"特定健診|\\(|\\)|（|）|GOT|GPT",""))

dat2$type2 %>% unique()

#そして、HbA1Cとγ-GTPをキレイにすると・・・
dat2 <- dat2 %>% 
  mutate(type2 = str_replace(type2,"γ-GTγ-GTP","γ-GTP")) %>% 
  mutate(type2 = str_replace(type2,"HbA1C","HbA1c"))

dat2$type2 %>% unique()

#できました！11個のラベルになりましたね。typeを処理する
#すくりぷとはこれでOKです。
#次は年度に行きましょう

dat2$nendo %>% unique()

#※集計結果が10未満の場合は「‐」で表示
#（10未満の箇所が1箇所の場合は総計以外全て「‐」で表示）

#というメッセージが年度のコラムに書いてあるので、これも何とかしましょう
#（このメッセージの内容はあとで値を処理するときにどうするか考えましょう

#まずは年度です。25から28までの数字をとりだしておきます。

dat3 <- dat2 %>% 
  mutate(nendo2 = str_extract(nendo,"(?<=H)\\d+(?=年度)"))

dat3 %>% select(nendo2, nendo) %>% distinct()

#うまくいきましたね。ついでに西暦に直しておきましょう。

dat3 <- dat3 %>% 
  mutate(seireki = as.numeric(nendo2)-25+2013)

dat3 %>% select(seireki,nendo2) %>% distinct()

#完成です！
#では、次に年齢です。

dat3$age %>% unique()

#これは問題なさそうですね。

#次に値
dat3$value %>% as.numeric() %>% summary()

#とりあえず、警告では、NAがIntroducedと書いてありますが、
#多分、-が入っているところであろうと想像しつつ、
#全部で588個のNAが本当に―の値となっているかどうかを確認しましょう。

checkna <- dat3 %>% 
  select(value) %>% 
  mutate(val2 = as.numeric(value)) %>% 
  filter(is.na(val2)) %>% 
  count(value)

checkna

#はい。どうでしょうか？欠損したデータはすべて"ー"のデータが入っている
#ということが確認できました。
#欠損値になるので、NAと置き換えるか、0と置き換えるか、等
#色々と他のValueを数値に置き換えるために必要な処理がありますが、
#ここでは、NAとしておきましょう(何かの数字に置き換えてしまうと、
#欠損であることがわからなくなります）

dat4 <- dat3 %>% 
  mutate(value2 = as.numeric(value))

#Okです。都道府県名も入力ミスや表記が一致しているか
#確認しておきましょう。
dat4$prefname %>% unique()

#47都道府県、そろってそうですね。

#kubunについては、

dat4$kubun %>% unique()

#だと、検査毎に区分がばらばらなため本当に検査毎に一致しているかわかりません
#こういう時はCount関数等を使いましょう

dat4 %>% count(type2, kubun) %>% View()



#眺めた感じだと、問題なのは、数字と記号が全角、半角が混在しているという
#ところにあります。全角数字と半角数字、あとはいくつかの記号
#を置き換えるようにすればよいので、

dat5 <- dat4 %>% 
  mutate(
    kubun2 = str_replace_all(kubun,"０","0"),
    kubun2 = str_replace_all(kubun2,"１","1"),
    kubun2 = str_replace_all(kubun2,"２","2"),
    kubun2 = str_replace_all(kubun2,"３","3"),
    kubun2 = str_replace_all(kubun2,"４","4"),
    kubun2 = str_replace_all(kubun2,"５","5"),
    kubun2 = str_replace_all(kubun2,"６","6"),
    kubun2 = str_replace_all(kubun2,"７","7"),
    kubun2 = str_replace_all(kubun2,"８","8"),
    kubun2 = str_replace_all(kubun2,"９","9"),
    kubun2 = str_replace_all(kubun2,"．","."),
    kubun2 = str_replace_all(kubun2,"～",""),
  )

#という風に基本コースでは書いていたと思うのですが、

#コース作成後に、named vectorを与えてあげることで、
#逐次、replaceすることができるという記載を発見したので、
#上のstr_replace_allを12回使って書いている処理と、次
#の処理は全く同じ処理になります。

replacer <- c("０" = "0", "１" = "1", "２" = "2",
              "３" = "3", "４" = "4", "５" = "5",
              "６" = "6", "７" = "7", "８" = "8",
              "９" = "9", "．" = ".", "～" = "" )

dat5 <- dat4 %>% 
  mutate(kubun2 = str_replace_all(kubun,replacer))

dat5 %>% count(type2, kubun2) %>% View()

#再掲とkubunに書いてあるデータ以外は問題なさそう(件数が一致している）
#ので、再掲と書いてある行にどんなデータがあるかを眺めましょう。

dat5 %>% filter(str_detect(kubun2,"再掲")) %>% View()

#？？？大本のエクセルファイルを見た方がはやそうです。
#H25のBMIをみてみると・・・
#
#なるほど。区分として集計したデータみたいです。
#これはいらないので、

dat6 <- dat5 %>% 
  filter(!str_detect(kubun2,"再掲"))

dat6 %>% count(kubun2, type2) %>% View()

#すべての区分が同じ数ずつあること、確認できました！

#ここまでやってきたものをまとめてみると
#次のようなスクリプトになります。
#

dat <- tibble(filepath = list.files("data", 
                                    pattern = "xlsx$", 
                                    full.names = TRUE,
                                    recursive = TRUE))

dat <- dat %>% 
  mutate(data = map(filepath, process_excel)) %>% 
  select(data) %>% 
  unnest(data)

dat <- dat %>% 
  mutate(type = str_replace_all(type,"特定健診|\\(|\\)|（|）","")) %>% 
  mutate(type = str_replace_all(type,"特定健診|\\(|\\)|（|）|GOT|GPT",""))

dat <- dat %>% 
  mutate(type = str_replace(type,"γ-GTγ-GTP","γ-GTP")) %>% 
  mutate(type = str_replace(type,"HbA1C","HbA1c"))

dat <- dat %>% 
  mutate(nendo = str_extract(nendo,"(?<=H)\\d+(?=年度)")) %>% 
  mutate(nendo = as.numeric(nendo)-25+2013) %>% 
  mutate(value = as.numeric(value))


replacer <- c("０" = "0", "１" = "1", "２" = "2",
              "３" = "3", "４" = "4", "５" = "5",
              "６" = "6", "７" = "7", "８" = "8",
              "９" = "9", "．" = ".", "～" = "" )

dat <- dat %>% 
  mutate(kubun = str_replace_all(kubun,replacer))

dat <- dat %>% 
  filter(!str_detect(kubun,"再掲"))

View(dat)

#どうでしょうか？
#出来上がりました！！これを、
dir.create("appdata")
write_rds(dat, "appdata/data.rds", compress="gz")

#として保存することでShinyアプリ用のデータのできあがりです！

#簡単に書いてあるように見えるかもしれませんが、
#上のような確認とデータ変換で問題がないことを
#確かめつつ変換の処理を書いていくと、
#ミスや意図しない動作等が少ないスクリプトをかけると
#思います。

#お疲れさまでした。
#これで本セクションは終了です
#
#次のセクションで、今回取得したデータを利用してアプリケーションを
#作っていきます。

