path<- 'mldata/Online_Retail.xlsx' # 下載檔案置於工作目錄下mldata目錄
library("readxl")           # 載入套件
xls_data <- read_excel(     # 讀取excel 檔案之活頁簿
  path = path,
  sheet='Online Retail')
print(xls_data) # 列印此tibble 前10筆記錄
########### 資料淨化 ##########
library(dplyr)            # 載入套件
print(colnames(xls_data))  # 列印欄位名稱
colnames(xls_data)<-make.names(   # 欄位名稱空白改以點替代
  names(xls_data),unique = TRUE)
print(colnames(xls_data))  # 列印欄位名稱

xls_data<-xls_data[      # 剔除退貨之資料
  xls_data$Quantity>0,]
xls_data<-xls_data[      #剔除退貨及未知客戶之資料
  !is.na(xls_data$Customer.ID),]
xls_data<-xls_data[      #剔除購買郵遞、運費之交易資料
  !xls_data$StockCode %in% c('POST','DOT'),]
############建構item master#######
xls_data_sorted<-xls_data[  # 將交易明細資料依發票日排序
  order(xls_data$InvoiceDate,decreasing = FALSE),]
item_master<- distinct(      # 建立商品主檔
  .data=xls_data_sorted,     # 依發票日排序的資料
  x=StockCode,               # distinct 的鍵值依據欄位
  .keep_all = TRUE)          # 保留鍵值及其他欄位
item_master<-item_master[    # 商品主檔只取需要之欄位
  c('StockCode','Description')]
print(item_master)  # 列印tibble物件前10筆
###########
xls_data<-aggregate( # 使用聚集函式
  formula=Quantity~ Customer.ID + StockCode , # 依客戶與商品group
  data=xls_data, # 明細資料來源
  FUN=sum)       # 依group條件加總
xls_data['value']<-1    # 增加一欄位名稱value且其值為1
print(head(xls_data))   # 列印data.frame前6筆

data_wide<- reshape(    # 長格式轉換成寬格式data.frame 4372 * 3684
  data=xls_data[,-c(3)],  # 長格式資料物件
  direction='wide',       # 指定轉換成寬格式
  idvar='Customer.ID',    # 每列依據欄位
  timevar='StockCode',    # 寬格式每欄依據
  v.names='value')        # 數值欄位名稱
print(data_wide[1:10,1:5])  # 列印前10筆6欄位
rownames(data_wide)<-data_wide[,1] # 第一行為列名
data_wide<-data_wide[,-1]  # 去除第一行
colnames(data_wide)<-substring(  # 矩陣行名稱去除前置7位元
  colnames(data_wide),7)
print(data_wide[1:10,1:5])   # 列印前10筆客戶及前5 欄商品
data_wide<-as.matrix(data_wide)   # 將data.frame物件轉成matrix物件
data_wide[is.na(data_wide)]<-0   # NA值改以二進位值0
print(data_wide[1:10,1:5])  # 列印前10筆6欄位

library(recommenderlab)    # 載入套件
bin_matrix<-as(            # 轉換成binaryRatingMatrix
  data_wide,
  'binaryRatingMatrix')   
print(getList(             # 列印customerID=17850的購買商品
  bin_matrix['17850',]))   
arules::image(            # 繪製購買與否(0或1)
  bin_matrix[1:50,1:50],  # 前面50個CustomerID在前面50個商品
  main='二進制評比矩陣分布'
)

n_users<- colCounts(bin_matrix)  # 各商品之客戶數
library(ggplot2)
qplot(                      # 繪出商品之客戶數分布
  n_users,
  geom="histogram",
  bins=50,
  main='所有商品的客戶數分布',
  xlab = '客戶數',
  ylab = '商品數')
qplot(                    # 繪客戶數100以下商品分布
  n_users[n_users<100],   # 過濾客戶數100以下
  geom="histogram",
  bins=50,
  xlab = '客戶數',
  ylab = '商品數',
  main='商品的客戶數100以下之分布')
bin_matrix<-bin_matrix[      # 過濾去除客戶數10以下之商品
  ,colCounts(bin_matrix)>=10
]
print(sum(rowCounts(bin_matrix)==0)) # 列印無商品購買之客戶
bin_matrix<-bin_matrix[      # 去除只買過10項商品以下之客戶
  rowCounts(bin_matrix)>=10,
]
### 建構商品推薦模型####
which_train<- sample(     # 將樣本分割訓練組/測試組
  x=c(TRUE,FALSE),  # 母體
  size=nrow(bin_matrix),   # 樣本數
  replace=TRUE,     # 取後放回
  prob=c(0.8,0.2)   # 樣本分配佔比
)
recc_data_train<-bin_matrix[which_train,]   # 訓練組二進制評比矩陣
recc_data_test<-bin_matrix[!which_train,]   # 測試組二進制評比矩陣
recom<-recommenderRegistry$get_entry(
  method='IBCF',
  dataType=class(recc_data_train))
print(recom)      # 列印推薦模型物件內容
print(recom$fun)  # 列印推薦模型物件原始碼

recc_model<- Recommender(   # 建立商品推薦模型物件
  data=recc_data_train,     # 訓練組
  method='IBCF',         # 基於商品(item)的協同過濾
  parameter=list(        # 給予參數
    method='Jaccard',    # 使用Jaccard相似性
    k=30                 # 使用KNN個數，預設值為30
  )
)
print(recc_model)  # 列印建立的模型物件
model_detail<-getModel(recc_model)   # 模型明細
class(model_detail$sim)      # 物件類別
print(as(     # 列印模型數筆商品相似性明細
  model_detail$sim[1:10,1:10],
  'matrix'))
dim(model_detail$sim)        # 商品相似性矩陣
all(            # 確認相似性矩陣每列KNN個數均為30
  rowSums(model_detail$sim != 0)==30)   

#####印證jaccard 運算法#####
jaccard<- function(x, y){   # 自訂jaccard similarity 函式
  if(length(x) == length(y)){
    intersection=sum(x*y)
    union = sum(x+y)-
      length(which((x+y)==2))
    return(intersection/union)
  } else{
    stop('向量長度需一致')
  }
}
cid<-which(model_detail$sim[1,]>0)[1] # 自商品相似矩陣找出>0來印證
model_detail$sim[     # 顯示商品相似性矩陣(dgCMatrix)內容
  1,cid,drop=FALSE]   # 第1列，cid行內容
dimn<-dimnames(             # 讀取相似矩陣之商品代號
  model_detail$sim[1,
  cid,drop=FALSE])  
recc_model@model$sim[  # 找出兩個商品之相似值
  dimn[[1]],dimn[[2]], 
  drop=FALSE] 
train.data<-as(    # itemMatrix 轉成matrix
  recc_data_train@data,
  'matrix') 
print(train.data[1:6,1:8])  # 列印訓練組部分矩陣資料
jaccard(                    #印證jaccard
  train.data[,dimn[[1]]],
  train.data[,dimn[[2]]]) 
##### 為測試組使用者推薦商品######
n_recommended<-5    #  推薦商品個數
recc_predicted<-predict(    # 使用前述推薦模型建立推薦商品
  object=recc_model,        # 推薦模型
  newdata=recc_data_test,   # 測試組資料
  n=n_recommended,          # 推薦商品個數
  type='topNList')           # 推薦類別(結果)
recc_matrix<-sapply(      # 測試組的使用者的topNList
  X=recc_predicted@items, # 測試組每客戶預測結果(list物件)
  FUN=function(x){        # x 為商品index
    return (recc_predicted@itemLabels[x])#傳回對應之推薦商品名稱
  })
print(recc_matrix[,1:10]) # 列印前10筆客戶推薦商品(n_recommended=5)
recc_matrix_desc<-apply(   # 將前10筆客戶的推薦商品轉以商品名稱
  X=recc_matrix[,1:10], 
  MARGIN=2,           # 以行向量傳入FUN指定之函式處理後傳回
  FUN=function(x){
    return (dplyr::filter(    # 傳回從商品主檔過濾的名稱
      item_master,
      StockCode %in% x))
  }
)
print(recc_matrix_desc[1:2])   #  列印前2客戶之推薦商品
Top_5_List<-as(recc_predicted, "list")  # 將topNList轉成list物件
print(Top_5_List[1:2])      # 列印前2客戶推薦商品
getRatings(recc_predicted)[1:2] # 同上列印前2客戶推薦商品的評比
###############印證推薦模型#################
sim <- model_detail$sim  # 商品相似性矩陣
u <- as(                # 將測試組資料物件轉dgCMatrix類別
  recc_data_test,'dgCMatrix') 
rtgs<- t(               # 依使用者已購商品的相似值做為評比值
  as(tcrossprod(sim,u)/
       tcrossprod(sim!=0, u !=0),
     "matrix"))
rtgs_r<-removeKnownRatings(       # 將測試組已購商品去除
  as(rtgs,'realRatingMatrix'),
  recc_data_test)
sort(                           # 降冪排序列出前5項(推薦)商品
  rtgs_r@data['12673',],decreasing=TRUE)[1:5]
##### 自訂函式以評估模型(K-摺疊交叉驗證)   ########
evaluateModel<-function(  # 函式宣告
  bin_matrix,             # 二元矩陣(binaryRatingMatrix)
  n_fold=10,               # k-摺疊 數
  items_to_keep=4,        # 假設known item 數
  number_neighbors=30,    # KNN數
  weight_description=0.2, 
  items_to_recommend=5,   # 對推薦商品數
  byUser=TRUE             # 依客戶(使用者)評估
){
  set.seed(1)   # 確保k-fold 的資料組於每次評估都相同內容
  eval_sets<- evaluationScheme(    # 建立評估方案
    data=bin_matrix,               # 傳入參數值(二元評比矩陣)
    method="cross-validation",     # 分成k-fold 交叉驗證
    k=n_fold,    # 傳入參數值(k-摺疊 數)
    given=min(   # 每客戶保存商品數不得大於min(rowCounts(bin_matrix))
      items_to_keep,
      min(rowCounts(bin_matrix)))   
  )
  print(getData(eval_sets,'train',run=1))  # 訓練集總數
  print(getData(eval_sets,'know',run=1))   # 測試集
  print(getData(eval_sets,'unknow',run=1)) # 驗證集
  print(getList(getData(eval_sets,'know'))[1:3]) # 測試集部分客戶
  recc_model<- Recommender(
    data=getData(eval_sets,'train',run=1),
    method='IBCF',   # 基於物品(item)的協同過濾
    parameter=list(
      method='Jaccard',
      k=number_neighbors   # 只紀錄最相近似的個數
    )
  )
  eval_prediction<- predict(        #  對測試組資料產生預測
    object=recc_model,
    newdata=getData(eval_sets,'know'),
    n=items_to_recommend,    # type=ratings時此引數會自動忽略
    type='topNList')          # top N 清單
  print(getList(        # 列印每客戶推薦商品(items_to_recommend=5)
    eval_prediction)[1:3]) 
  print(getList(        # 列印已知客戶購買
    getData(eval_sets,'unknow')[1:3,])) 
  eval_accuracy <- calcPredictionAccuracy(   # 衡量預測之準確度
    x=eval_prediction,    # 測試組的預測資料
    data=getData(eval_sets,'unknow'),   # 測試組的實際資料
    given=items_to_recommend,    # 推薦商品數
    byUser = byUser   # 是否依每userId
  )
  return (eval_accuracy)
}
model_evaluation<-evaluateModel(
  bin_matrix=bin_matrix,
  items_to_keep=10,
  byUser=FALSE)
if (is.vector(model_evaluation)){  # byUser參數值決定傳回物件類別
  print(round(model_evaluation,6))
}else{
  print(model_evaluation[1:3,])
}
############ end of 5_4.R#######################