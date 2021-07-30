path<- 'data/preferences_32.csv'  # 資料檔案指定於工作目錄之相對路徑
q<-read.csv(path)  # 讀取問卷調查資料
q<-data.frame(
  q[,2:length(q)],  # 資料第二欄起為酒類喜好值
  row.names=q[,1])   # 擷取資料第一欄為rownames
q<-as.matrix(q)   # 轉成矩陣物件
tq<-t(q)          # 轉置成以酒類為個體，受測者為變數的矩陣
print(tq)         # 列印矩陣(個體、變數)資料
##### 步驟一:變數(行)標準化 ##########
std<-scale(tq)/    # 資料標準化(視原始資料為母體)
  sqrt((nrow(tq)-1)/nrow(tq))  
summary(std)  # 標準化資料摘要
##### 步驟二:列、行權重計算 ##########
rw<- rep(1/nrow(std),nrow(std))  # 列權重
cw<- rep(1,ncol(std))            # 行權重
X<- t(t(std*sqrt(rw))*sqrt(cw))  # 投影前慣量(含正負)
round(cbind(   # X2分布與合計
  rbind(X^2,'Total'=colSums(X^2)), # 投影前行慣量合計
  'Total'=rowSums(rbind(X^2,'Total'=colSums(X^2))),
  '%'=c(t(rowSums(X^2))/sum(X^2),1)*100),
  6)
all(round(cov2cor(cov(scale(tq))),8)==
      round(cor(tq),8))  # 驗證共變異數矩陣與相關性矩陣的關係
all(round(t(X)%*%X,7)==round(cor(tq),7))  # 驗證(4.3.3)
##############萃取主成分##########
##############使用FactoMineR套件進行PCA#########
library(FactoMineR)
res.pca<-PCA(  # 產生PCA 物件
  X=tq,         # matrix物件(列為所有品牌，行為所有變數)
  ncp = minx,   # 保留維度同Rank值
  graph = FALSE      # 是否繪圖
)
#######主成分佔比################
lambda<-res.pca$eig[,1]  # 特徵值
df<-data.frame(    # 分解後各成分比率
  特徵值=lambda,
  特徵值佔比=round(lambda/sum(lambda)*100,2),
  特徵值佔比累積=round(cumsum(lambda/sum(lambda)*100),2))
rownames(df)<-paste0('comp ',1:minx)        # 主成分順序
rbind(df,        # 慣量(特徵值)比
      'Total'=c(sum(df[,1]),sum(df[,2]),NA))
############ 慣量驗證 #################
round(cbind(   # 投影後變數慣量合計
  rbind(res.pca$var$coord^2,           # 投影後變數慣量合計
        'Total'=colSums(res.pca$var$coord^2)), 
  'Total'=rowSums(rbind(res.pca$var$coord^2,
                        'Total'=colSums(res.pca$var$coord^2)))),6)
round(cbind(   # 投影後個體慣量合計
  rbind(res.pca$ind$coord^2,           # 投影後個體慣量合計
        'Total'=colSums(res.pca$ind$coord^2)), 
  'Total'=rowSums(rbind(res.pca$ind$coord^2,
                        'Total'=colSums(res.pca$ind$coord^2)))),6)
############個體分群組########
km3<-kmeans(  #  觀察值分群函式
  x=tq,         #  分群之資料來源
  centers=3,    #  指定分群數
  iter.max = 10,  # 最多迭代計算次數
  nstart = 25   # 初始隨機分組之取組數(決定盡快達成穩定分組)
)
########factoextra 繪製雙標圖 ###########
library(factoextra)
library(tidyr)
g<-fviz_pca_biplot(
  res.pca,         # PCA 物件
  repel=TRUE,      # 重疊文字是否錯開並加上引線 
  title = 'Preference Analysis(PCA - Biplot)',  # 標題
  ggtheme = theme_minimal(),  # 繪圖主題
  labelsize=5,     #  文字大小
  pointshape=16,   # 標示點之形狀(請參閱show_point_shapes函式)
  col.ind=km3$cluster%>%factor,  # object 個體群組之顏色依據
  mean.point=FALSE,  # 不列印群組中心點
  alpha.ind=1,       # 個體顏色透明度
  palette = c("blue", "red", "black"))  # 個體群組顏色色盤
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/mdpref_biplot.svg",  #存檔目錄及檔名
  g,           # ggplot 繪圖物件
  scale = 1.5)  # 繪圖板尺規範圍擴增倍數
#################慣量與品質#############
round(t(t(res.pca$ind$coord^2)/colSums(res.pca$ind$coord^2))*100,2)==
  round(res.pca$ind$contrib,2)
#################end of 4_4###############################