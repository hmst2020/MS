library(tmcn)
library(data.table)
library(hablar)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
#########  情感辭典 #########
data(NTUSD)   # 使用套件內含中文情感辭典
print(names(NTUSD)) # 列印list名稱
pos_lexicon<-tibble(   # 取出正面詞語
  word=NTUSD$positive_cht,  # 繁體中文
  sentiment=rep('positive',length(NTUSD$positive_cht)))
neg_lexicon<-tibble(   # 取出負面詞語
  word=NTUSD$negative_cht,  # 繁體中文
  sentiment=rep('negative',length(NTUSD$negative_cht)))
bing_lexicon<-rbind(pos_lexicon,neg_lexicon)  #合併正、負面詞語
print(setorder(   # 找出辭典重複資料
  find_duplicates(bing_lexicon,word,sentiment),word,sentiment))
print(find_duplicates(bing_lexicon,word) %>%  # 中性詞語
  group_by(word,sentiment) %>%
  dplyr::summarize() %>% 
  find_duplicates(word))
bing_lexicon<-unique(    # 淨化二元辭典
  bing_lexicon,by=c('word','sentiment'))  
saveRDS(bing_lexicon,    # 物件存檔備用
        file='mldata/comm_lexicon.rds')
######### 社會新聞  ############
library(httr)
library(rvest)
news_path<-'https://www.chinatimes.com/society/?chdtv' # 社會新聞
html.obj<-GET(news_path,timeout(20)) %>%  # 取回網頁原始碼
  read_html(news_path)    # 萃取html文件
news.title<- html.obj %>%  # 萃取其熱門的新聞標題
  html_nodes("section[class='hot-news'] [class='title']") %>%
  html_text()
news.link<- html.obj %>%  # 萃取其熱門新聞的超連結
  html_nodes("section[class='hot-news'] [class='title'] a") %>%
  html_attr("href")
latest_news.title<-html.obj %>%  # 萃取其最新的新聞標題
  html_nodes("div[class='articlebox-compact'] [class='title']") %>%
  html_text()
latest_news.link<-html.obj %>%  # 萃取其最新新聞的超連結
  html_nodes("div[class='articlebox-compact'] [class='title'] a") %>%
  html_attr("href")
news.title<-c(news.title,latest_news.title) # 合併上述新聞標題
news.link<-c(news.link,latest_news.link)    # 合併上述新聞超連結
news_all<-tibble(title=news.title,link=news.link) # 建立新聞物件
news_society<- data.frame()    # 起始物件變數
for (i in 1:nrow(news_all)){   # 依超連結一一取回新聞網頁內容
  news_path<-news_all[i,]$link  # 超連結
  html.obj<-GET(news_path,timeout(20)) %>% # 取回新聞網頁原始碼
    read_html(news_path)
  news.body<- html.obj %>%  # 自新聞本體萃取其各段落文字
    html_nodes("div[class='article-body'] p") %>%
    html_text()
  text<- paste(news.body, collapse = '')  # 串起各段落文字 
  news_society<-rbind(        # 列加一筆新聞(含標題及新聞文本)
    news_society,
    data.frame(
      title=news_all[i,]$title,
      body=text))
}
glimpse(news_society)  # 一瞥新聞物件欄位部分內容
# saveRDS(news_society,file='mldata/news_society.rds')

############################
library(jiebaR)
library(tm)
news_society<-readRDS(file='mldata/news_society.rds')  # 讀入新聞
cutter <- worker(                       # 斷詞器
  user='mldata/user.dict_trad.utf8',    # 詞料白名單
  stop_word='mldata/stop_words_trad.utf8'  # 詞料黑名單
)
tokenize_my_token<-function(x){  # 自訂詞料庫分詞函式(傳回list物件)
  res<-lapply(x, function(y) {        # x每筆(row)使用斷詞器處理
    cutter.text<- cutter[y]   # 使用斷詞器自然語言處理(含黑白名單)
    cutter.text<- removeNumbers(cutter.text)  # 移除數字
    cutter.text<-cutter.text[
      !cutter.text %in% c('','.')] # 去除遺留的無意義詞
    return(cutter.text)     # 傳回詞料庫分詞後之vector
  })
  return(res)   # 傳回list物件
}
rm(news_words_1)   # 第一筆新聞分解詞語
eval_sentimemt<- function(title,text_body,lexicon){
  news_words<-unnest_tokens(    # 以行為詞料庫將其各詞料為一列分列
    tbl=data.frame(body=text_body), # 詞料庫資料所在data frame
    output=word,        # 輸出各詞料的行名稱
    input=body,         # 輸入詞料庫的行名稱
    token='my_token')    # 自訂詞料庫分詞函式(自動加上tokenize_)
  if(!exists('news_words_1')){news_words_1<<-news_words;
    print(news_words,row.names=TRUE,max=10)}
  inner_join(         # 只要情感辭典語詞交集部分
    news_words,       # 新聞詞語
    lexicon,     # 二元情感辭典
    by=c('word'='word')) %>%   # 交集使用之欄位
    dplyr::count(sentiment) %>%  # 依sentiment欄位分別合計次數
    spread(sentiment,n,fill=0) %>% # 將長格式轉成寬格式，NA補0
    mutate(  # 增加sentiment及title欄位
      sentiment=
        ifelse(
          exists('positive',mode='numeric'),
          positive,0)-  # positive 欄位不存在則補0
        ifelse(
          exists('negative',mode='numeric'),
          negative,0),  # negative 欄位不存在則補0
      title=title)
}
library(data.table)
library(plyr)
sentiments <- data.frame()  # 起始物件變數
for (i in 1:nrow(news_society)){  # 每一新聞計算其情感值
  news.body<- news_society[i,]$body  # 新聞文本
  sentiments<- rbind.fill(  # 增列計算後新聞情感值 
    sentiments,
    eval_sentimemt(news_society[i,]$title,news.body,bing_lexicon))
}
print(as.data.table(sentiments),5)   # 列印前後5筆
qplot(sentiments$sentiment,    # 情感評分分布
      geom="histogram",
      bins=5,
      main='新聞情感評分分布',
      xlab='情感評分',
      ylab='新聞數')
###### 文字雲  #######
library(wordcloud2)
news_society[1,]$title  # 第一則新聞標題
words<-cutter[news_society[1,]$body] %>%  # 將文本斷詞
  freq() %>%                            # 計算每詞語在文本中頻次
  dplyr::rename(word=char) %>%      # 欄位名稱char改為word
  inner_join(         # 只要情感辭典語詞交集部分
    bing_lexicon,     # 二元情感辭典
    by=c('word'='word')) %>%  # join 的關聯欄位
  mutate(color=ifelse(  # 增一欄位color正、負面詞語分別給予藍、紅色
    sentiment=='positive','blue','red'))
print(as.data.table(words),5)   # 列印該則新聞文本詞語
wordcloud2(  # 繪製文字雲
  words,     # 資料依據(data frame物件)
  color=words$color,  # 指定顏色欄
  shap='triangle',    # 雲形狀
  size = 0.3)         # 文字大小縮放比
################ end of 6_2.R ###########