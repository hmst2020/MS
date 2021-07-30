#############################################
# 學生資料與分布圖                          #
#############################################
# R環境選項變數設定
options(width=180) # 將console output 寬度調整容納長資料寬度
# 學生成績資料
library(data.table)
enrol<- readRDS(file = "data/enrol.rds")   # 載入本例資料
print(as.data.table(enrol),5)  # 列印資料內容前後各5筆
# 繪出入學申請成績(TOEFL、GMAT)分佈圖
plot(   # 產生繪圖物件
  x=enrol$TOEFL,  # x 軸資料欄
  y=enrol$GMAT,   # y 軸資料欄
  type='n',  # 資料不繪出
  xlab='TOEFL',    # x 軸標籤文字
  ylab='GMAT',     # y 軸標籤文字
  main="Student's TOEFL & GMAT")    # 標題文字
text( # 於繪圖物件標示學生代號
  x=enrol$TOEFL,   # 同上
  y=enrol$GMAT,    # 同上
  labels=rownames(enrol))   # 學生代號
############ggplot 繪製分布圖##############
library(ggplot2) # 載入繪圖套件
library(ggrepel)
ggplot(
  data=enrol,
  mapping=aes(
    x=TOEFL,
    y=GMAT
    )
  )+
  labs(title="Student's TOEFL & GMAT",
       x ='TOEFL',
       y ='GMAT'
  )+
  geom_point()+
  geom_label_repel( # 疊加文字於圖
  data=enrol, # 文字資料來源
  mapping=aes(
    x=TOEFL,
    y=GMAT,
    label=row.names(enrol)
  ),
  show.legend =FALSE,
  hjust=-0.1, # 文字位置水平向右幾個字寬
  vjust=-0.1  # 文字位置垂直向上調整幾個字高
)
###########################################
# 將不同尺規度量的資料標準化(standarlize) #
###########################################
summary(enrol)  # 標準化之前分布概要
enrol.std<- scale(  # 標準化觀察值
  x=enrol,
  center=TRUE,  # 一致位移以0為中心
  scale=TRUE    # 以標準差為一致之單位
)
print(enrol.std) # 列印標準化之結果
print(summary(enrol.std)) # 列印彙總之結果
############比較資料標準化前後歐氏距離之影響##############
dist.e.n <- dist( # 計算交互之歐氏距離
  x=enrol,         # 未標準化資料
  method = "euclidean", # 計算歐氏距離
  diag = TRUE,          # 含對角線(對應自身距離)之數字
  upper = TRUE          # 為閱讀方便上半部數字亦印出
)
cluster.e.n <- hclust( # 以全階方式進行群組分析
  dist.e.n,
  method = "complete"
)
cutree(cluster.e.n,k=3)  # 未標準化資料的分群結果

dist.e.s <- dist( # 計算交互之歐氏距離
  x=enrol.std,         # 標準化資料
  method = "euclidean", # 計算歐氏距離
  diag = TRUE,          # 含對角線(對應自身距離)之數字
  upper = TRUE          # 為閱讀方便上半部數字亦印出
)
cluster.e.s <- hclust( # 以全階方式進行群組分析
  dist.e.s,
  method = "complete"
)
cutree(cluster.e.s,k=3)  # 標準化資料的分群結果
########比較資料標準化前後馬氏距離之影響############
dist.e.m <- dist( # 計算交互之馬氏距離
  x=enrol,         # 標準化資料
  method = "manhattan", # 計算馬氏距離
  diag = TRUE,          # 含對角線(對應自身距離)之數字
  upper = TRUE          # 為閱讀方便上半部數字亦印出
)
cluster.e.m <- hclust( # 以全階方式進行群組分析
  dist.e.m,
  method = "complete"
)
cutree(cluster.e.m,k=3)  # 標準化資料馬氏距離的分群結果
###########肘部法折線圖建議分群數################
library(factoextra)
optimal.clust<-fviz_nbclust(
  x=enrol,
  method='silhouette',
  FUNcluster=hcut,
  k.max=14,  # 受測者數
  nstart = 5
)
print(optimal.clust)  # 列印肘部法折線圖
#############################################
# 繪出歐氏多階群組分析圖                      #
#############################################
plot(  # 繪出以歐氏距離分析的多階群組圖
  x=cluster.e.s,
  xlab='Cluster',
  ylab='Euclidean distance',
  main='Hierarchical cluster Analysis',
)
abline(h=4,col='red',lty=2)
cbind(   # 標準化資料歐氏距離的分2群結果
  enrol,
  cluster=cutree(cluster.e.s,k=2)
  ) 
################本例結束#################################

#################################################
# 以馬氏距離(Mohanlanobis distance)分析(未分群) #
#################################################
dist.manhattan <- dist( # 計算交互之馬氏距離
  x=enrol.std,
  method = "manhattan",
  diag = TRUE,
  upper = TRUE
)
options(width=180) # 同上
sink( # 同上
  file="E:/temp/dist_manhattan.txt",
  type="output")
print(dist.manhattan) # 將馬氏距離output
sink() # 同上
#############################################
# 繪出馬氏多階群組分析()                      #
#############################################
cluster.m <- hclust(  # 以全階方式進行群組分析
  dist.manhattan,
  method = "complete"
)
plot(  # 繪出以馬氏距離分析的多階群組圖
  x=cluster.m,
  xlab='Cluster',
  ylab='Mohanlanobis distance',
  main='Hierarchical cluster Analysis',
)
#############################################
# 繪出馬氏多階群組分析(資料未標準化)                      #
#############################################
dist <- dist( # 計算交互之馬氏距離
  x=enrol,
  method = "manhattan"
)
cluster <- hclust(  # 以全階方式進行群組分析
  dist,
  method = "complete"
)
plot(  # 繪出以馬氏距離分析的多階群組圖
  x=cluster,
  xlab='Cluster',
  ylab='Mohanlanobis distance',
  main='Hierarchical cluster Analysis',
)
############## end of 3_2#################