data(iris)   # 建立變數名稱
##### 方法 1 步驟說明 #########
library(data.table) # 應用data.table套件
print(as.data.table(iris))  # 列印前、後各5筆
summary(iris)  #  顯示綜合統計資料
norm <-function(v) {     # 定義正規化函式
  (v -min(v))/(max(v)-min(v))   
}
normd_iris <- as.data.frame(  # 將iris資料正規化後以data.frame傳回
  lapply(                   # 各欄位向量依FUN指定函式處理後傳回list物件
    X=iris[,c(1,2,3,4)],    # 將4個欄位分別帶入正規化函式
    FUN=norm                # 指定處理函式
    )
)
summary(normd_iris)  #  顯示正規化後綜合統計資料
euclidean<- function(v1,v2){    # 定義歐氏距離計算函式
  if(length(v1) == length(v2)){
    sqrt(sum((v1-v2)^2))     # 兩點之歐氏距離
  } else{
    stop('向量長度需要一致')
  }
}
cosine2<-function(v1,v2){       # 定義cosine距離計算函式
  if(length(v1) == length(v2)){
    numer<- sum(v1*v2, na.rm = T)   # 向量內積
    denom<-sqrt(sum(v1^2, na.rm = T))*    # 向量長相乘
      sqrt(sum(v2^2, na.rm = T))   
    return (1-numer/denom)
  } else{
    stop('向量長度需要一致')
  }
}
KNN<- function(train,test, k, FUN){    # 定義K近鄰清單函式
  if(ncol(train) != ncol(test)){
    stop('訓練組與測試組變數行數需要一致')
  }
  dist<-apply(      # 對test這一點計算與訓練組 各點之距離
    train,
    1, 
    FUN,
    test)    
  dist.sorted<-sort(   # 對距離排序後取前k列(k個點) 
    dist,
    decreasing=FALSE)[1:k]   
  neighbors<-as.numeric(names(dist.sorted))   # 將列名轉成純數   
  return(list(neighbors, dist.sorted))        # 傳回k個點指標及其距離
}
samidx<- 1:(nrow(iris)-1)           # 訓練組的index
iris_train<- iris[samidx,]          # 取出訓練組原始資料
iris_test<- iris[-samidx,]          # 取出測試組(1筆)原始資料
train.data <- normd_iris[samidx,]   # 取出訓練組正規化資料
test.data <- normd_iris[-samidx,]   # 取出測試組(1筆)正規化資料
index<- KNN(         # 呼叫KNN函式，傳回最近鄰之前k筆指標
  train=train.data, 
  test=test.data,
  k=4, 
  FUN=euclidean
  )[[1]]
print(iris_train[index,])      # 列印K-近鄰原始資料
KNN_pred<- function(knn,cls){  # 定義分類推論函式
  tbl<-table(knn[,cls])        # 將k個近鄰點依類別個數列表
  return (sort(tbl,decreasing=TRUE)[1])  # 將類別個數最多者傳回類別名稱
}
KNN_pred(                      # 呼叫分類推論函式傳回推論類別名稱
  iris_train[index,],
  'Species')   
print(iris[-samidx,'Species'])    #  列出測試組(1筆)原始資料與之比較
##### 方法 2 套件class內建函式knn #########
samidx <- sample(           # 隨機取樣比例90%為訓練組資料
  1:nrow(iris), 0.9 * nrow(iris))
iris_train<- iris[samidx,]          # 取出訓練組原始資料
iris_test<- iris[-samidx,]          # 取出測試組原始資料
train.data <- normd_iris[samidx,]   # 取出訓練組正規化資料
test.data <- normd_iris[-samidx,]   # 取出測試組正規化資料
iris_target_category<- iris[samidx,'Species']   # 訓練組的類別向量
iris_test_category <- iris[-samidx,'Species']   # 測試組的類別向量
library(class)
pred <- knn(          # 套件class內建函式knn
  train=train.data,   # 訓練組資料集
  test=test.data,     # 測試組資料集
  cl=iris_target_category,  # 目標(真實)類別
  k=4)
print(pred)  # 列印推論分類結果
tab <- table(   # 將推論分類結果與測試組類別產生個數交叉對照矩陣
  pred,
  iris_test_category)
print(tab)      # 列印交叉對照矩陣
accuracy <- function(x){     # 定義精度計算函式
  sum(diag(x)/(sum(rowSums(x)))) * 100     # 對角值部分的佔比
}
print(accuracy(tab))   # 列印精確度

library(ggplot2)
acu_func<- function(k){  # 定義精確度函式
  pred <- knn(           # 套件class內建函式knn
    train=train.data,
    test=test.data,
    cl=iris_target_category,
    k=k)
  tab <- table(   # 將推論分類結果與測試組類別產生個數交叉對照矩陣
    pred,
    iris_test_category)
  result<-sum(diag(tab)/(sum(rowSums(tab))))*100  # 對角值部分的佔比
  return(result)     
}
xydata <- data.frame(   # 建構繪圖資料物件
  x =1:30,
  y=unlist(lapply(      # 對k=1~30分別計算精確度後回傳向量物件
    1:30,
    acu_func))) 
ggplot(     #  依繪圖資料物件繪製點狀分布圖
  data=xydata,    
  mapping=aes(x=x,y=y))+
  geom_point()+
  ggtitle('精確度分布圖')+ # 圖標題
  xlab('k 值')+ylab('精確度 %')  # 給予xy軸標籤
####### end of 5_1.R###############