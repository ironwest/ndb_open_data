# Readme

## はじめに

このレポジトリは厚生労働省のNDBオープンデータを可視化するアプリケーションの作成過程のスクリプトを公開する目的のものです。

このレポジトリで作成したアプリは、Shinyapps.io上の、[このリンク先](https://norimitsu-nisida.shinyapps.io/NDBapp/)で動かしています。

このレポジトリにあるスクリプトは、

* 厚生労働省のHPからデータをスクレイピングするスクリプト
* スクレイピングしたファイルをTidyな形に加工するスクリプト
* 加工されたデータをもとにアプリケーションの設定ファイルを作成するスクリプト
* 取得したデータを利用してShinyapps.ioで動かしているアプリ

を含めておいてあります。

## ファイルの簡単な説明

* `data_scraping/obtain_file_link.R` : NDBのホームページにあるエクセルファイルへのリンクを取得するためのファイルです。実行した場合、`data/links_excel.xlsx`ファイルにスクレイプした結果が保存されます。

* `data_scrping/download_all_files.R` : `data/links_excel_modifies.xlsx`ファイルを入力として、そのtibbleの構造をもとにdataフォルダ下にファイルをすべてダウンロードします。入力ファイルは、`data/links_excel.xlsx`ファイルを手で修正したものです。ヘモグロビン、眼底検査については、2回目以降、2個ファイルがありますが、2個目の詳細情報レコードのデータを除外しています。また、1回目のデータと2回目以降のデータで結構ファイル数にも差があるので、2回目以降のデータをダウンロードするようにしています。また、なぜか（単独に違う年度のデータリンクがあったり、隠しリンクで違う年度のエクセルファイルがダウンロードできてしまうという宝探し的要素がありましたが、そこらへんも手作業でキレイにしました。

* `wrangling/tokutei_kensin.R`: 特定健診の検査に関わるエクセルファイルをTidyデータに加工して、`apps/data/processed/tokutei_kensin`に保存します。

* `wrangling/tokutei_monsin.R`:
特定健診の問診に関わるエクセルファイルをTidyデータに加工して、`apps/data/processed/tokutei_monsin`に保存します。

以下、作成中

## お願い

　スクリプトの内容については、一人で作成しているため、なにか誤っている部分があるかもしれません。もし、見つけていただいた場合、お教えいただければ、修正いたします。(Gitの使い方を理解されている方であれば、リクエストいただけたら嬉しいです。)

