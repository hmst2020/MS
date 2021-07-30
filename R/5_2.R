Sys.getlocale()   # 目前的語言環境
Sys.setlocale("LC_CTYPE", "Chinese")  # 變更語言環境
tnews<-readRDS(file = "mldata/tmdf.rds")  # 讀取R資料庫
library(dplyr)
glimpse(tnews)      # 一瞥tnews
print(as_tibble(tnews))   # 以tibble物件格式一瞥tnews

library(jiebaR)
cutter <- worker(                  # 斷詞器
  user='mldata/user.dict.utf8',    # 詞料白名單
  stop_word='mldata/stop_words.utf8')  # 詞料黑名單
library(tm)
tokenize_my_token<-function(x){  # 自訂詞料庫分詞函式(傳回list物件)
  res<-lapply(x, function(y) {        # x每筆(row)使用斷詞器處理
    cutter.text<- cutter[y]   # 使用斷詞器自然語言處理(含黑白名單)
    cutter.text<-removeNumbers(cutter.text)  # 移除數字
    cutter.text[!grepl(          # 只保留非標點符號、圖、數字
      "[:punct:]|[:graph:]|[:xdigit:]", 
      cutter.text)] 
    return(cutter.text)     # 傳回詞料庫分詞後之vector
  })
  return(res)   # 傳回list物件
}
library(tidytext)
news_words<-unnest_tokens(    # 以行為詞料庫將其各詞料為一列分列
  tbl=tnews,          # 詞料庫資料所在data frame
  output=word,        # 輸出各詞料的行名稱
  input=body,         # 輸入詞料庫的行名稱
  token='my_token')    # 自訂詞料庫分詞函式(自動加上tokenize_)
head(news_words)   # 最前6筆
tail(news_words)   # 最後6筆

news_words<-news_words[-which(news_words$word==''),]  # 視需要
news_words<-news_words[-which(news_words$word=='-'),] # 視需要
news_words<-news_words[-which(news_words$word=='.'),] # 視需要
news_words<-count( # 依group 欄位加總次數，並增一次數欄位n
  x=news_words,    # data frame 對象
  title,           # group by 欄位1
  word,            # group by 欄位2
  sort = TRUE)     # 是否排序(降冪)
grouped_news_words<-dplyr::group_by(    # 轉成grouped_df物件
  .data=news_words,    # data frame 物件
  title)               # group 的欄位依據
total_words<-dplyr::summarize(   # 依指定的group加總
  .data=grouped_news_words,  # grouped_df物件
  total = sum(n))  #增加一加總欄位total
news_words <- left_join(  # 以left join 將兩個data frame合併
  news_words,    # 左邊data frame
  total_words,   # 右邊data frame
  by='title')    # 兩個data frame據以連結的共同欄位
news_tf_idf <- bind_tf_idf(   # 產生計算tf-idf結果集(data frame)
    tbl=news_words,   # 資料依據
    term=word,        # term 的欄位
    document=title,   # document的欄位
    n=n)                # 次數欄位
print(data.table::as.data.table(news_tf_idf))   # 列印前後5筆
### 印證TF-IDF 權值計算####
tf<-news_words$n/news_words$total
idf<-log(length(unique(news_words$title))/  
           table(news_words$word))
idf<-idf[news_words$word]
tf_idf<-tf*idf
print(tf_idf[1:10])
#######################
news_wide<- reshape2::dcast(    # 依formula關係建立矩陣
  data=news_tf_idf ,     # data frame資料源
  formula=title ~ word,  # 矩陣列與行對應資料源之欄位
  value.var = 'tf_idf', #  矩陣對應的數字欄位
  na.rm=TRUE         # 先去除value.var指定的欄位值為NA的紀錄再處理
)
print(news_wide[1:5,1:5]) # 列印前5筆5行

news_wide[is.na(news_wide)]<-0  # 將NA以0取代
rownames(news_wide)<-news_wide[,1]  # 以第1行為列名
news_wide<- news_wide[,-1]  # 去除第1行
news_dist<- proxy::dist(   # 產生距離矩陣
  news_wide, 
  method = "euclidean")
print(as.matrix(news_dist))  # 列印距離矩陣

library(factoextra)
optimal.clust<-fviz_nbclust(   # 決定最佳群組數
  x=news_wide,
  method='silhouette',
  FUNcluster=kmeans,
  k.max=nrow(news_wide)-1
)
print(optimal.clust)  # 列印群組數曲線

news_hcls<-hclust(  # 使用階層集群法
  news_dist,method = "ward.D2")  
n_clus<-cutree(news_hcls,k=3)  # 列印階層集群法分類結果
print(n_clus)  # 列印階層集群法分類結果
######## end of 5_2.R #############