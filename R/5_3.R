###########讀取raw data ######################
ratings=read.csv(           # 讀入使用者評比資料建構data.frame物件
  "mldata/ratings.csv")
movies=read.csv(            # 讀入影片主檔資料建構data.frame物件
  "mldata/movies.csv")
library(data.table)
print(as.data.table(ratings)) #前、後5筆userId對於movieId的評比紀錄
print(as.data.table(movies))  #前、後5筆movies主檔資料
############將raw data 加工縮小物件大小#######################
library(stringi)
library(reshape2)
dt<-merge(   # 以movieId為key以inner join(預設)合併
  ratings,
  movies,
  by="movieId")
print(as.data.table(dt))  #前、後5筆長格式資料
rtmatrix = reshape2::dcast(    # 依formula關係建立矩陣
  data=dt,      # data frame資料源
  formula=userId ~ movieId+title,  # 矩陣列與行對應資料源之欄位
  value.var = 'rating', #  矩陣對應的數字欄位
  na.rm=TRUE,         # 先去除value.var指定的欄位值為NA的紀錄再處理
  drop=TRUE)           # 是否丟棄完全無評比資料之使用者與影片
rownames(rtmatrix)<-rtmatrix[,1]  # 列依userId命名
rtmatrix<-rtmatrix[,-1]    # dcast結果第一個欄位為多餘，將之去除
print(rtmatrix[      # 列印稀疏評比資料
  1:10,   # 前10筆userId
  1:4])   # 前4個movieId
library(recommenderlab)     # 載入recommenderlab套件
MovieLense = as(          # 將矩陣轉成realRatingMatrix物件
  as.matrix(rtmatrix), 
  "realRatingMatrix")
print(object.size(  # 稀疏矩陣記憶體占用
  as.matrix(rtmatrix)),
  units='auto')   
print(object.size(  # 緊緻矩陣realRatingMatrix記憶體占用
  MovieLense),
  units='auto') 
##########再過濾出熱門影片###################################
ratings_movies<-MovieLense[ # 篩選符合條件的資料
  rowCounts(MovieLense)>50, # 擁有超過50個影片評分的使用者
  colCounts(MovieLense)>100 # 擁有超過100位使用者評分的影片
]
print(ratings_movies)       # 列印篩選後realRatingMatrix筆數

avg_ratings_per_user<-      # 每一使用者之平均評比
  rowMeans(ratings_movies) 
library(ggplot2)
qplot(avg_ratings_per_user,    # 繪出使用者平均評分分布
      geom="histogram",
      bins=50,
      main='使用者平均評分分布')

ratings_movies_norm<-          
  normalize(ratings_movies)     # 將每user對影片的評比標準化
ratings_movies_norm[            # 標準化之後均值近似於0
  rowMeans(ratings_movies_norm)>0.000001,]
min_movies<-quantile(       # 最後5% 使用者的的至少評比影片數
  rowCounts(ratings_movies),
  0.95)   
min_users<-quantile(        # 最後5% 影片的的至少給予評比的使用者數
  colCounts(ratings_movies),
  0.95)
image(ratings_movies[      # 繪出標準化前這5%使用者與5%影片評比熱圖
  rowCounts(ratings_movies)>min_movies,
  colCounts(ratings_movies)>min_users],
  main='頂部5%評比分布熱圖')
image(ratings_movies_norm[ # 繪出標準化後這5%使用者與5%影片評比熱圖
  rowCounts(ratings_movies_norm)>min_movies,
  colCounts(ratings_movies_norm)>min_users],
  main='頂部5%標準化評比分布熱圖')
############分割訓練組與測試組############################
which_train<- sample(# 以ratings_movies總數依80/20比例區分TRUE/FALSE
  x=c(TRUE,FALSE),  # 母體
  size=nrow(ratings_movies),   # 樣本數
  replace=TRUE,     # 取後放回
  prob=c(0.8,0.2))   # 樣本分配佔比
recc_data_train<-ratings_movies[which_train,] # 訓練組評比資料
recc_data_test<-ratings_movies[!which_train,] # 測試組評比資料
######## 建立IBCF模型 #######################
recommenderRegistry$get_entry_names() # 列出所有可用的模型類別名稱
recomm_models<-       # 取出IBCF_realRatingMatrix模型物件
  recommenderRegistry$get_entry(
    'IBCF',
    dataType='realRatingMatrix')
print(recomm_models)    # 列印推薦模型內容

recc_model<- Recommender(   # 建立商品推薦模型物件
  data=recc_data_train,     # 訓練組
  method='IBCF',         # 基於商品(item)的協同過濾
  parameter=list(        # 給予參數
    method='Cosine',     # 使用Cosine 計算相似性
    normalize='Z-score', # 使用Z-score將資料標準化
    k=25))               # 使用KNN個數，預設值為30
model_detail<-getModel(recc_model)   # 模型明細
print(model_detail$sim[1:5,1:5])       # 列印模型數筆商品相似性明細
class(model_detail$sim)      # 物件類別
image(                       # 繪製熱圖
  model_detail$sim[1:10,1:10],
  main='前數筆商品相似性熱圖')
print(rowCounts(      # 列印每商品相似商品數(同k=25)
  as(model_detail$sim,'realRatingMatrix')))
## 印證Cosine 運算法 #####
cid<-which(model_detail$sim>0)[1]    # cosine值第一個出現大於0的位置
model_detail$sim[     # 顯示商品相似性矩陣(dgCMatrix)內容
  1,cid,drop=FALSE]   # 第1列，cid行內容
recc_data_train_norm<-          # 訓練組資料使用Z-score將資料標準化
  normalize(
    recc_data_train,
    method='Z-score')
dimn<-dimnames(                 # 讀取行、列名即影片名
  model_detail$sim[1,
  cid,drop=FALSE])   
cosmatrix<-as(                  # 矩陣化
  recc_data_train_norm[,c(dimn[[1]],dimn[[2]])],
  'matrix')
cosv<-cosmatrix[    # 取兩影片同時存在使用者評比的資料
  which(!is.na(cosmatrix[,1]) & !is.na(cosmatrix[,2])),]
library(lsa)
lsa::cosine(cosv[,1],cosv[,2])  # 計算兩影片cosine similiarity
recommenderlab::similarity(     # 使用similarity計算similiarity
  as(cosv,'realRatingMatrix'),  # 物件類別轉換為realRatingMatrix
  method='cosine',              # 計算cosine similiarity
  which='items')
#########################################
##### 為測試組使用者推薦商品######
n_recommended<- 6     # 推薦最多6個影片給每一使用者
recc_predicted<-predict(   # 使用學習組資料的推薦模式對測試組資料組做預測
  recc_model,              # 推薦模式
  newdata=recc_data_test,  # 測試組資料
  n=n_recommended,         # 推薦數(最多)
  type='topNList')         # 預測結果類別
recc_matrix<-sapply(      # 測試組的使用者的topNList
  recc_predicted@items,   # 測試組預測結果
  function(x){            # 對應每使用者的推薦影片名稱，x 為index
    return (colnames(ratings_movies)[x])
  })
number_of_items<-                # 測試組的影片推薦分布
  factor(table(unlist(recc_matrix)))  
qplot(number_of_items)+ 
  ggtitle('IBCF測試組各影片推薦數分布')
number_of_items_sorted<-sort(    #  依降冪排序推薦分布
  number_of_items,decreasing=TRUE)
data.frame(           # 列出推薦次數前6名
  影片名=names(head(number_of_items_sorted)),# 最受推薦次數影片名稱
  推薦次數=head(number_of_items_sorted),     # 推薦次數
  row.names = NULL)
Top_6_List<-as(recc_predicted, "list")   # 將topNList轉成list物件
print(Top_6_List[1:2])   # 列印前2筆推薦名單
print(slotNames(recc_predicted))  # 列印物件slot name
recc_predicted@ratings[1:2]  # 列印前2筆推薦名單的加權評比
getRatings(recc_predicted)[1:2]  # 同上列印前2筆推薦名單的加權評比
######驗證使用者topNList推薦商品####
sim<-model_detail$sim      # 影片之相似矩陣
newdata <- normalize(      # 測試組比照推薦模型予以標準化
  recc_data_test, 
  method = recc_model@model$normalize)
u<-as(newdata, 'dgCMatrix')  # 將標準化後之物件轉dgCMatrix類別
rtgs<-t(         # 依公式計算使用者權重法推論之標準化評比
  as(tcrossprod(sim, u)/
       tcrossprod(sim, u != 0), 
    'matrix'))
new_rtgs<-new(# 將權重法標準化評比與標準化評比建構realRatingMatrix
  'realRatingMatrix',
  data = dropNA(rtgs), 
  normalize = getNormalize(newdata))
n_rtgs<-denormalize(new_rtgs)  # 推論realRatingMatrix之實際評比
uid_1<-names(Top_6_List[1])  # 第一個使用者userId
top_1_1<-Top_6_List[1][[1]][[1]] # 第一個使用者的第一個推薦影片
uu<-getRatingMatrix(n_rtgs)[   # 從n_rtgs取得帶權重推論之實際評比
  uid_1,
  top_1_1]
print(uu)  # 列印權重法推論該使用者實際給予之評比分數
##### 自訂函式以評估模型(K-摺疊交叉驗證)   ########
items_to_keep<-4       # 假設每客戶已知評比電影數
n_fold<-5        # K-fold 數
eval_sets<- evaluationScheme(    # 建立評估方案
  data=ratings_movies,           # 傳入參數值(實評比評比矩陣)
  method="cross-validation",     # 分成k-fold 交叉驗證
  k=n_fold,    # 傳入參數值(k-摺疊 數)
  given=min(   # 每客戶保存商品數不得大於min(rowCounts(ratings_movies))
    items_to_keep,
    min(rowCounts(ratings_movies))), 
  goodRating=3)     # 評比>=3才納入評估模型的電影
print(getData(eval_sets,'train',run=1))  # 訓練組第一組
print(getData(eval_sets,'train',run=2))  # 訓練組第二組
print(getData(eval_sets,'know',run=1))   # 測試集第一組
print(getData(eval_sets,'unknow',run=1)) # 驗證集第一組
evaluateModel<-function(  # 評估函式宣告
  eval_sets=eval_sets,    # 評估方案
  run=1,   # 第幾摺疊組
  number_neighbors=30,    # KNN數
  items_to_recommend=5,   # 對推薦商品數
  byUser=TRUE            # 依客戶(使用者)評估
){
  set.seed(1)   # 確保k-fold 的資料組於每次評估都相同內容
  recc_model<- Recommender(    # 建立商品推薦模型物件
    data=getData(eval_sets,'train',run=1),
    method='IBCF',   # 基於物品(item)的協同過濾
    parameter=list(
      method='Cosine',     # 計算cosine 相似性
      k=number_neighbors))   # 只紀錄最相近似的個數
  if(!exists('recc_model_1')){   # 環境變數若不存在
    recc_model_1<<-recc_model    # 將區域變數指定予環境變數
  }
  eval_prediction<- predict(        #  對測試組資料產生預測
    object=recc_model,              # 推薦模型物件
    newdata=getData(eval_sets,'know',run=run),   # 模擬推薦新對象
    n=items_to_recommend,    # type=ratings時此引數會自動忽略
    type='ratings')          # 推估評比
  if(!exists('eval_prediction_1')){   # 環境變數若不存在
    eval_prediction_1<<-eval_prediction # 將區域變數指定予環境變數
  }
  eval_accuracy <- calcPredictionAccuracy(   # 衡量預測之準確度
    x=eval_prediction,    # 測試組的預測資料
    data=getData(eval_sets,'unknow',run=run),   # 測試組的實際資料
    byUser = byUser)   # 是否依每userId
  return (eval_accuracy)
}
model_evaluation<-evaluateModel(   # 執行模型評估並回傳結果
  eval_sets=eval_sets,    # 評估方案
  run=1,   # 評估第幾組訓練組
  number_neighbors=30,    # KNN數
  items_to_recommend=5,   # 對推薦商品數
  byUser=TRUE)            # 依客戶(使用者)評估
if (is.vector(model_evaluation)){  # byUser參數值決定傳回物件類別
  print(round(model_evaluation,6))
}else{
  print(model_evaluation[1:3,])
}

print(getList(        # 列印已知使用者的評比
  getData(eval_sets,'know')[1:1,]))
print(getList(        # 列印已知使用者的評比
  getData(eval_sets,'unknow')[1:1,]))
print(getModel(recc_model_1)$sim[1:3,1:3]) # 列印 cosine 相似性矩陣
rm(recc_model_1)   # 移除環境變數recc_model_1
print(getList(        # 列印測試組第一個使用者的推估評比
  eval_prediction_1)[1:1]) 
rm(eval_prediction_1)   # 移除環境變數recc_model_1
######### end of 5_3.R###############
