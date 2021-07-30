library(SentimentAnalysis)
library(jiebaR)
library(tm)
library(tmcn)
library(tidytext)
library(tidyverse)
library(data.table)
cutter <- worker(                  # 斷詞器
  user='mldata/user.dict_trad.utf8',    # 詞料白名單
  stop_word='mldata/stop_words_trad.utf8'  # 詞料黑名單
)
tokenize_my_token<-function(x){  # 自訂詞料庫分詞函式(傳回list物件)
  res<-lapply(x, function(y) {   # x每筆(row)使用斷詞器處理
    cutter.text<-removeNumbers(y) # 移除數字
    cutter.text<-removePunctuation(cutter.text)
    cutter.text<-cutter[cutter.text]   # 使用斷詞器自然語言處理(含黑白名單)
    return(cutter.text)     # 傳回詞料庫分詞後之vector
  })
  return(res)   # 傳回list物件
}
news_finance<-readRDS(   # 讀入專題新聞/專題
  file='mldata/news_topics.rds')
finance_dict<-readRDS(   # 讀入財務專用辭典(Loughran & McDonald)
  file='mldata/finance_dict.rds')
finance_lexicon<-rbind(data.frame(  # 將正、負面詞語合併於一
  word=finance_dict$positive,
  sentiment='positive'),
  data.frame(
    word=finance_dict$negative,
    sentiment='negative')
)
finance_lexicon<-unique(    # 淨化二元辭典(去重複)  
  finance_lexicon,by=c('word','sentiment'))
###########情感分析 方法一###########################
eval_sentimemt<- function(title,text_body,lexicon){
  news_words<-unnest_tokens(    # 以行為詞料庫將其各詞料為一列分列
    tbl=data.frame(body=text_body), # 詞料庫資料所在data frame
    output=word,        # 輸出各詞料的行名稱
    input=body,         # 輸入詞料庫的行名稱
    token='my_token')    # 自訂詞料庫分詞函式(自動加上tokenize_)
  if(!exists('news_words_1')){news_words_1<<-news_words;
    print(news_words,row.names=TRUE,max=10)}
  jtemp<-inner_join(         # 只要情感辭典語詞交集部分
    news_words,       # 新聞詞語
    lexicon,          # 二元情感辭典
    by=c('word'='word')) %>%   # 交集使用之欄位
    dplyr::count(sentiment) %>%  # 依sentiment欄位分別合計次數
    spread(sentiment,n,fill=0) # 將長格式轉成寬格式，NA補0
  total_words<-nrow(news_words) # 計算詞語總數
  if(nrow(jtemp)==0){    # 處理無詞語交集之文件視為中性
    return (data.frame(
      positive=0,
      negative=0,
      sentiment=0,
      total_words=total_words,
      title=title
    ))
  }
  mutate(  # 增加sentiment及title欄位
    .data=jtemp,
    sentiment=
      ifelse(
        exists('positive',mode='numeric'),
        positive,0)-  # positive 欄位不存在則補0
      ifelse(
        exists('negative',mode='numeric'),
        negative,0),  # negative 欄位不存在則補0
    total_words=total_words,
    title=title)
}
library(data.table)
library(plyr)
sentiments <- data.frame()  # 彙整各文件情感值
for (i in 1:nrow(news_finance)){ # 處理各文件情感值
  news.body<- news_finance[i,]$body
  sentiments<- rbind.fill(  # 遇傳回值缺行時自動補上NA
    sentiments,
    eval_sentimemt(   #eval_sentimemt函式傳回值
      title=news_finance[i,]$title,
      text_body=news.body,
      lexicon=finance_lexicon))
}
print(data.table(sentiments),5)  # 列出前後5筆
sentiments<-sentiments %>%  # 計算情感指數
  mutate(MySentiment=sentiment/total_words) %>%
  select(c('title','MySentiment'))
print(data.table(sentiments),5)  # 列印前後5筆
saveRDS(sentiments,file='mldata/finance_sentiments.rds')
###########情感分析 方法二###########################
stw<-readLines('mldata/stop_words.utf8')  # 黑名單
dict_bin<-SentimentDictionaryBinary( # 外部財務專用辭典
  positiveWords=finance_lexicon[  # 正面詞語
    finance_lexicon$sentiment=='positive',]$word,
  negativeWords=finance_lexicon[  # 負面詞語
    finance_lexicon$sentiment=='negative',]$word
)
news_corpus <- Corpus(    # 詞料庫建立
  VectorSource(news_finance$body)) %>%  
  tm_map(tokenize_my_token)    # 詞語切割(依據函式)
news_dtm<-tmcn::createDTM(   # 產生DocumentTermMatrix物件
  news_corpus,
  language='zh')
doc_senti<-analyzeSentiment(  # 進行分析
  x=news_dtm,    # 給予的DocumentTermMatrix物件
  language = "zh-TW",  # 指定語言
  rules=list("MySentiment"=      # 計算規則
               list(ruleSentiment,   # 以ruleSentiment函式計算
                    dict_bin)),    # 指定使用的辭典
  stopwords=stw,    # 詞語黑名單
  removeStopwords=TRUE, # 黑名單之詞語不列入分析
  stemming=TRUE)        # 使用詞幹
print(data.table(doc_senti),5)  # 列印前後5筆
pnresp<-convertToBinaryResponse( # 將方法二情感評分轉成兩極分類
  data.frame(
    title=news_finance$title,
    Sentiment=doc_senti$MySentiment))
print(data.table(pnresp),5)    # 列印前後5筆
finance_sentiments<-readRDS(file='mldata/finance_sentiments.rds')
pnresp2<-convertToBinaryResponse(  # 將方法一情感評分轉成兩極分類 
  data.frame(
    title=finance_sentiments$title,
    Sentiment=finance_sentiments$MySentiment))
print(data.table(pnresp2),5)   # 列印前後5筆
compareToResponse(   # 比較兩方法之評分
  doc_senti,
  finance_sentiments$MySentiment)
########### generate lexicon ##############
library(mlapi)
library(text2vec)
library(scales)
news_finance<-readRDS(   # 讀入專題新聞/專題
  file='mldata/news_topics.rds')
finance_sentiments<-readRDS(   # 讀入先前依財務專用辭典的分析結果
  file='mldata/finance_sentiments.rds')
response<-finance_sentiments$MySentiment
#response<-rescale(  # 重新調整尺度範圍
#  finance_sentiments$sentiment,    # 權充專家的評分
#  to=c(-5, 5))   # 尺度縮小於-5 ~ +5 之間
control_parm=list(  # 控制參數
  alpha=0,
  family="gaussian", 
  grouped=FALSE)

########## 方法一 ###########
library(glmnet)
library(scales)
set.seed(1)  # 消除隨機結果使一致
response<-rescale(  # 重新調整尺度範圍
  finance_sentiments$MySentiment,
  to=c(-5, 5))   # 尺度縮小於-5 ~ +5 之間
cv.enet <- glmnet::cv.glmnet(  #  k-fold 交叉驗證傳回lambda值
  x=as.matrix(news_dtm),       #  文件對詞語之頻次矩陣
  y=response,      #  專家的文件評比
  alpha=control_parm$alpha,    # 同1=lasso , 0=ridge  0.5=elastic net
  family=control_parm$family,  
  grouped=control_parm$grouped)
plot(cv.enet)   # 繪出k-fold 交叉驗證結果
print(cv.enet)  # 列印k-fold 交叉驗證結果
coefs <- coef(  # 依據指定lambda值萃取模型係數
  object=cv.enet,  # 適配模型物件
  s=cv.enet$lambda.min)  # lambda值
scoreNames <- coefs@Dimnames[[1]][  # 取出選出的詞語(變數項)
  setdiff(coefs@i+1, 1)]
scores <- coefs@x         # 取出所有係數(含截距值)
if (length(coefs@i) > 0 && coefs@i[1]==0) {  # 判斷是否含截距值
  scores <- scores[-1]   # 去除截距值，留下與變數項等長之係數
}
intercept <- ifelse(  # 取截距值
  length(coefs@i) > 0 && coefs@i[1]==0,
  coefs@x[1],
  0)
wordFrequency<-colSums(  # 計算各詞語(變數項)在各文件的頻次
  as.matrix(news_dtm[,scoreNames]) != 0)
idf <- log(nrow(news_dtm)/wordFrequency)  # 計算idf值
df_lexicon<- data.frame(    # 以係數為各詞語之權重
  word=scoreNames,
  weight=scores
)
top_n(x=df_lexicon,n=20,wt=weight) %>%    # 萃取前10項權重最高之詞語
  ggplot(aes(x=weight,y=reorder(word,weight)))+  # 繪出條狀圖
  geom_bar(  
    width =0.5,  # 條狀寬度
    stat = "identity",  # 條狀高度依y(weight)值
    colour = "black"
  )+
  xlab('權重')+
  ylab('詞語')
##### 比較自建財經專用辭典與Loughran & McDonald財務專用辭典######
dict_bin<-SentimentDictionaryBinary( # 外部財務專用辭典
  positiveWords=finance_lexicon[  # 正面詞語
    finance_lexicon$sentiment=='positive',]$word,
  negativeWords=finance_lexicon[  # 負面詞語
    finance_lexicon$sentiment=='negative',]$word
)
news_lexicon_wt<-SentimentDictionaryWeighted(#自建財務專用辭典權重
  words=scoreNames,
  scores=scores,
  idf=idf,
  intercept=intercept
)
plot(news_lexicon_wt) # 繪出自建財務專用辭典權重分布
news_lexicon<-SentimentDictionaryBinary(  # 自建財務專用辭典
  positiveWords=df_lexicon[  # 正面詞語
    df_lexicon$weight>=0,]$word,
  negativeWords=df_lexicon[  # 負面詞語
    df_lexicon$weight<0,]$word
)
compareDictionaries(  # 比較自建與外部辭典
  news_lexicon,
  dict_bin)
summary(news_lexicon)   # 列印辭典權重彙總
########## 方法二 ###########
library(SentimentAnalysis)
#tm::inspect(news_dtm[1:20,500:510])
set.seed(2)
response<-rescale(  # 重新調整尺度範圍
  finance_sentiments$MySentiment,
  to=c(-5, 5))
control_parm$family='gaussian'
news_lexicon_2_wt<-generateDictionary(
  x=news_dtm,
  response=response,
  control=control_parm,
  weighting = tm::weightTfIdf,
  modelType='ridge'
)
plot(news_lexicon_2_wt)  # 繪出自建財務專用辭典權重分布
summary(news_lexicon_2_wt)  # 列印辭典權重彙總
news_lexicon_2<-SentimentDictionaryBinary(  # 自建財務專用辭典
  positiveWords=news_lexicon_2_wt$words[  # 正面詞語
    news_lexicon_2_wt$scores>=0],
  negativeWords=news_lexicon_2_wt$words[  # 負面詞語
    news_lexicon_2_wt$scores<0]
)
compareDictionaries(  # 比較自建與外部辭典
  news_lexicon_2,
  dict_bin)
doc_senti_2<-analyzeSentiment(  # 進行分析
  x=news_dtm,    # 給予的DocumentTermMatrix物件
  language = "zh-TW",  # 指定語言
  rules=list("MySentiment"=      # 計算規則
               list(ruleSentiment,   # 以ruleSentiment函式計算
                    news_lexicon_2)),    # 指定使用的辭典
  stopwords=stw,    # 詞語黑名單
  removeStopwords=TRUE, # 黑名單之詞語不列入分析
  stemming=TRUE)        # 使用詞幹
compareToResponse(   # 比較兩方法之評分
  doc_senti_2,
  finance_sentiments$MySentiment)
plotSentimentResponse(   # 繪出比較圖
  doc_senti_2$MySentiment,
  response)
######### end of 6_3.R ##########
