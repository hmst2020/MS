item_master<-readRDS(file='mldata/item_master.rds')   # 讀取商品主檔
library(dplyr)
glimpse(item_master)   # 一瞥商品主檔
print(as_tibble(item_master)) # 商品主檔前10筆
library(jiebaR)
cutter <- worker(                  # 建立斷詞器
  user='mldata/user.dict.utf8',    # 詞料白名單
  stop_word='mldata/stop_words.utf8'  # 詞料黑名單
)
library(tm)
tokenize_my_token<-function(x){ # 自訂詞料庫分詞函式(傳回list物件)
  res<-lapply(x, function(y) {  # x每筆(row)使用斷詞器處理
    cutter.text<- cutter[y]     # 使用斷詞器自然語言處理(含黑白名單)
    return(cutter.text)         # 傳回詞料庫分詞後之vector
  })
  return(res)   # 傳回list物件
}
library(tidytext)
item_words<-unnest_tokens(  # 行為詞料庫切分各詞為一列
  tbl=item_master[          # 詞料庫資料所在data frame
    c('StockCode','Description')],
  output=word,        # 輸出各詞料的行名稱
  input=Description,  # 輸入詞料庫的行名稱
  token='my_token'    # 自訂詞料庫分詞函式(自動加上tokenize_)
)
item_words<-count( # 依group 欄位加總次數，並增次數欄位n
  x=item_words,    # data frame 對象
  StockCode,       # group by 欄位1
  word,            # group by 欄位2
  sort = TRUE)     # 是否排序(降冪)
print(item_words)   # 列印斷詞結果

item_tf_idf <- bind_tf_idf(   # 產生計算tf-idf結果集(data frame)
  tbl=item_words,   # 資料依據
  term=word,        # term 的欄位
  document=StockCode,   # document的欄位
  n=n)
glimpse(item_tf_idf)  # 一瞥商品各詞語的TF-IDF權值
##########recommender##########
######載入交易明細資料####
xls_data<-readRDS(file='mldata/Online_Retail.rds')
tbl_data<-table(   # 將客戶的商品交易次數對應
  xls_data[,c('Customer.ID','StockCode')])
tbl_data[tbl_data==0]<-NA
library(recommenderlab)
user_freq<-as(   # 客戶的商品交易次數轉成實際評比物件
  as(tbl_data,'matrix'),
  'realRatingMatrix')
user_freq<-user_freq[  # 過濾消費過低的客戶與冷門商品
  rowCounts(user_freq)>=30,
  colCounts(user_freq)>=30
]
print(getList(user_freq)[1:2]) #列印前2客戶購買紀錄
########## recommender by user profile ############
n_recommended<-5    # 推薦商品筆數
items_to_keep<-10   # 假設已知購買
user_profiles<-lapply(   # 建立客戶購買紀錄(以詞語加權值)
  getList(user_freq),    # user_freq 的清單逐筆處理
  FUN=function(user_rating){
    user_rating<-user_rating[  # 最多items_to_keep筆做為已知
      1:min(items_to_keep,length(user_rating))]
    df<-data.frame(      # 建立該客戶的已知商品及評比
      item=names(user_rating),freq=user_rating)
    result<-item_tf_idf[ # 用已知商品相關的詞語與評比left join
      which(item_tf_idf$StockCode %in% names(user_rating)),
      ] %>%
      left_join(df,by=c('StockCode'='item'))
    result<-cbind(   # 計算商品各詞語tf-idf*客戶評比為權重值
      result[,c('StockCode','word')], 
          data.frame(weighted=result$tf_idf*result$freq)
      ) %>%
      group_by(word) %>%    # 依詞語加總
      summarise(weighted=sum(weighted),n=n(),.groups='drop')
    return (result[       # 回傳依權值降冪的詞語紀錄
      order(result$weighted,
            decreasing = TRUE),])
  })
print(user_profiles[1:1]) #列印第1筆客戶詞語紀錄
eval_prediction<-lapply(   # 以客戶購買紀錄推估客戶未知的購買
  seq_along(user_profiles),  # 已知的客戶購買詞語紀錄
  FUN=function(x,y,z,i){# x~z傳入之參數值，i為seq_along函式產生之順序碼
    user_id<-y[i]     # 依順序碼取出客戶代號
    user_pro<-x[[i]]  # 依順序碼取出客戶購買詞語紀錄
    user_predicted<-inner_join( # 以word欄位join兩個data frame  
      user_pro,   # 已排序的客戶購買詞語紀錄
      item_tf_idf,  # 各商品的詞語 TF-IDF權值
      by=c('word'='word'))[,c('word','weighted','StockCode','idf')]
    user_recom<-user_predicted %>%   # 將同商品的TF-IDF權重加總
      group_by(StockCode) %>%
      summarise(predicted_weighted=sum(weighted),.groups='drop')
    user_bought<-names(  # 已知購買的商品代號
      z[[user_id]][1:min(items_to_keep,length(z[[user_id]]))])
    user_recom<-user_recom[  # 排除已購買之商品
      !user_recom$StockCode %in% 
        user_bought,]
    recom_items<-user_recom[order( # 依權值排序推薦前n_recommended項
      user_recom$predicted_weighted,
      decreasing = TRUE),][1:n_recommended,]
    return (recom_items)  # 傳回每客戶之推薦商品清單
  },
  x=user_profiles,   # 所有客戶已知的購買詞語紀錄
  y=names(user_profiles), # 所有客戶代號
  z=getList(user_freq)    # 所有客戶的購買紀錄
)
names(eval_prediction)<-names(user_profiles) # 賦予list客戶名稱
print(eval_prediction[1:1]) #列印第1筆客戶推薦清單

N<-ncol(user_freq)-n_recommended  # 評估商品總數
eval_accuracy<-lapply(   # 估計對客戶推薦的準確性
  seq_along(eval_prediction),  # 依每推薦每客戶的商品清單處理
  FUN=function(x,y,z,i){   # x,y,z 參考函式傳入之參數值，i 為順序碼
    recom_items<-x[[i]]  # 客戶之推薦商品清單
    TP<-nrow(recom_items[  # 計算TP
      recom_items$StockCode %in% 
        names(z[[y[i]]]),])
    FP<-n_recommended-TP   # 計算FP
    FN<-length(z[[y[i]]])-TP   # 計算FN
    TN<-N-(TP+FP+FN)           # 計算TN 
    precision<-TP/n_recommended  # 計算精確比例
    recall<-TP/(TP+FN)  # 計算召回比例
    TPR<-recall         # 計算真陽率
    FPR<-FP/(FP+TN)     # 計算偽陽率
    return (c(TP=TP,FP=FP,FN=FN,TN=TN,
              N=N,precision=precision,recall=recall,
              TPR=TPR,FPR=FPR))
  },
  x=eval_prediction,
  y=names(eval_prediction),
  z=getList(user_freq)
)
names(eval_accuracy)<-names(eval_prediction)  # 賦予list客戶名稱
acc_sum<-t(as.data.frame(eval_accuracy)) # 轉換以客戶為列之評估資料
rownames(acc_sum)<-substring(rownames(acc_sum),2) # 賦予客戶名稱
print(round(colMeans(acc_sum),6))   # 列印整體之confusion matrix

############# end of 5_6.R############
