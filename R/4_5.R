df<- readRDS(file = "data/coffeeBrand.rds")   # 本例資料載入
print(df)    # 列印資料表內容
ka<- 1:8     # 活躍變數
ks<- 9:9     # 補充量化變數
si<- 5       # 補充個體
##############不區分補充個體 ###############
pca<-FactoMineR::PCA(     # 主成分分析函式
  #scale.unit = TRUE,
  X=df[,ka],              # 分析資料
  ncp=ncol(df),           # 保留分析維度資料
  graph = FALSE)            # 是否繪圖
km3gp<-kmeans(    #  觀察值分群函式
  x=df[,ka],      #  分群之資料來源
  centers=3,      #  指定分群數
  iter.max = 10,  # 最多迭代計算次數
  nstart = 25)     # 初始隨機分組之取組數(決定盡快達成穩定分組)
pca$eig   # 列出特徵值(慣量)及其排序
iner_dist_ind<-t(t(pca$ind$coord^2))/     # 個體慣量分布(列投影品質)
  rowSums(pca$ind$coord^2)
print(iner_dist_ind) # 列印個體慣量分布
sum(iner_dist_ind[-nrow(iner_dist_ind),   # 犧牲活耀個體慣量合計占比
              3:ncol(iner_dist_ind)])/
  sum(iner_dist_ind)
iner_dist_var<-t(t(pca$var$coord^2))/     # 變數慣量分布(行投影品質)
  rowSums(pca$var$coord^2)
print(iner_dist_var) # 列印變數慣量分布
sum(iner_dist_var[,3:ncol(iner_dist_var)])/  # 犧牲變數慣量合計占比
  sum(iner_dist_var)
####################### 區分補充個體、補充變數 #######
pca<-FactoMineR::PCA(     # 主成分分析函式
  X=df,                   # 分析資料
  quanti.sup=ks,          # 補充量化變數(行)
  ind.sup=si,             # 補充個體(列)
  ncp=ncol(df[,ka]),      # 保留分析維度資料
  graph = FALSE)          # 是否繪圖
km3gp<-kmeans(    #  觀察值分群函式
  x=df[-si,ka],      #  分群之資料來源
  centers=3,      #  指定分群數
  iter.max = 10,  # 最多迭代計算次數
  nstart = 25)     # 初始隨機分組之取組數(決定盡快達成穩定分組)
pca$eig   # 列出特徵值(慣量)及其排序
iner_dist_ind<-t(t(pca$ind$coord^2))/     # 個體慣量分布(列投影品質)
  rowSums(pca$ind$coord^2)
print(iner_dist_ind) # 列印個體慣量分布
sum(iner_dist_ind[,3:ncol(iner_dist_ind)])/  # 犧牲個體慣量合計占比
  sum(iner_dist_ind)
iner_dist_var<-t(t(pca$var$coord^2))/     # 變數慣量分布(行投影品質)
  rowSums(pca$var$coord^2)
print(iner_dist_var) # 列印變數慣量分布
sum(iner_dist_var[,3:ncol(iner_dist_var)])/   # 犧牲變數慣量合計占比
  sum(iner_dist_var)
############################################
library(factoextra)
library(dplyr)
library(tidyr)
g<-fviz_pca_biplot(           #  產生個體及其變數(屬性)雙標繪圖物件
  X=pca,                      #  依據物件(pca或prcomp物件皆可)
  geom = c("point","text"),   # 點及文字標示object位置
  ggtheme = theme_minimal(),  # 使用之布景主題
  title = "咖啡品牌知覺定位(PCA - Biplot)", # 圖標題
  col.ind=km3gp$cluster%>%factor,  # 個體群組之顏色依據
  col.ind.sup = "red",
  repel=TRUE,                      # 重疊文字是否錯開並加上引線
  mean.point=FALSE,                # 是否顯示各組之中心點
  labelsize=3,                     # 文字大小
  alpha.ind=1,                     # 顏色之透明度
  pointshape=16)+      # 標示點之形狀(請參閱show_point_shapes函式)
  scale_color_manual(     # 圖例說明
    name = "群組", 
    labels = c("GP1", "GP2", "GP3"),
    values=  c("forestgreen","blue","#A52A2A"))
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/pca_biplot.svg",  #存檔目錄及檔名
  g,           # ggplot 繪圖物件
  scale = 1.0)  # 繪圖板尺規範圍擴增倍數
####################步驟解析################################
##### 步驟一:變數(行)標準化 ##########
qn<- as.matrix(df[-si,c(ka,ks)])  # 擷取量化變數與活躍個體部分
center <- colMeans(qn, na.rm=TRUE)    #  平均值
sd<-sqrt(colSums((t(t(qn)-center)^2)/nrow(qn)))   # 母體標準差
std.qn<-t((t(qn)-center)/sd)  #  標準化資料
round(cbind(   # 變數標準化列表
  rbind(std.qn,'Total'=colSums(std.qn)), # 行合計
  'Total'=rowSums(rbind(std.qn,'Total'=colSums(std.qn)))), # 列合計
8)
##### 步驟二:列、行權重計算 ##########
print(t(std.qn[,ka])%*%std.qn[,ka])    # 列權重調整前
rw<- rep(1/nrow(std.qn),nrow(std.qn))  # 列權重
cw<- rep(1,ncol(std.qn[,ka]))          # 行權重
X<- std.qn[,ka]*sqrt(rw)               # 慣量
print(t(X)%*%X)             # 用來特徵分解  
print(cor(std.qn[,ka]))     # 活躍變數的相關矩陣(correlation matrix)
##### 步驟三:奇異值分解 ##########
library(pracma)
minx<-Rank(X)   # 有效維度
PCS<- c('主成分1','主成分2','主成分3','主成分4')[1:minx]
#minx<-min(nrow(X),ncol(X))    # SVD 的行列
SVD<- svd(X,nv=minx,nu=minx)  # 奇異值分解
mult<-as.vector(sign(t(cw)%*%SVD$v)) #  使變號為正
U<- SVD$u #%*%diag(c(mult))      # 左奇異矩陣
rownames(U)<-rownames(X)
colnames(U)<-PCS
V<- SVD$v #%*%diag(c(mult))      # 右左奇異矩陣
rownames(V)<-colnames(X)
colnames(V)<-PCS
S<- replace(SVD$d,SVD$d<0,0)[1:minx]  # 奇異值小於0視為0
lambda<- S^2               # 特徵值
P<- t(t(U)*S*mult)/sqrt(rw)    # 個體座標
Q<- t(t(V)*S*mult)/sqrt(cw)    # 變數座標
inertia<-data.frame(    # 分解後各成分比率
  特徵值=lambda,
  特徵值佔比=round(lambda/sum(lambda)*100,2),
  特徵值佔比累積=round(cumsum(lambda/sum(lambda)*100),2))
rownames(          # 主成分順序
  inertia
)<- PCS
rbind(inertia,        # 慣量(特徵值)比
      'Total'=c(sum(inertia[,1]),sum(inertia[,2]),NA))
print(inertia)  # 列印主成分順序
round(cbind(   # 投影前列慣量合計
  rbind(X^2,'Total'=colSums(X^2)), # 投影前行慣量合計
  'Total'=rowSums(rbind(X^2,'Total'=colSums(X^2))),
  '%'=c(t(rowSums(X^2))/sum(X^2),1)*100),
  6)
round(cbind(   # 投影後個體慣量合計
  rbind(P^2,'Total'=colSums(P^2)), # 投影後個體慣量合計
  'Total'=rowSums(rbind(P^2,'Total'=colSums(P^2)))),6)
round(cbind(   # 投影後變數慣量合計
  rbind(Q^2,'Total'=colSums(Q^2)), # 投影後變數慣量合計
  'Total'=rowSums(rbind(Q^2,'Total'=colSums(Q^2)))),6)
sum(X^2)                   # 投影前總慣量
sum(Q^2)                   # 投影後總慣量
print(cor(X,P))            # 列印X與個體的主成分相關係數
round(cor(X,P),7)==round(Q,7)
round(cor(X,P)^2,8)==round(pca$var$cor^2,8)
round(cor(X,P)^2,8)==round(pca$var$cos2,8)  # cosθ=r(k,v)
t(t(X%*%Q)*(1/sqrt(lambda)))/sqrt(rw)          # 個體座標
t(t(t(X)%*%(P*sqrt(rw)))*(1/sqrt(lambda)))     # 變數座標
t(t(X%*%pca$var$coord)*(1/sqrt(lambda)))/sqrt(rw)          # 個體座標
t(t(t(X)%*%(pca$ind$coord*sqrt(rw)))*(1/sqrt(lambda)))
#####補充個體定位#####
sup.ind.std<- (df[si,ka]-center[ka])/sd[ka]   # 補充個體變數標準化
sup.ind.coord<-t(t((as.matrix(sup.ind.std)%*%Q))/S)   # 補充個體座標公式
print(sup.ind.coord)   # 列印補充個體座標
#####補充變數向量#####
sup.quan.std<-std.qn[,ks,drop=FALSE]
sup.quan.coord<-t(t(t(sup.quan.std)%*%P)/S/nrow(P))
print(sup.quan.coord)      # 列印補充變數座標
print(cor(std.qn[,ks],P))  # 列印補充變數與活躍個體相關係數
all(round(cor(std.qn[,ks],P),7)==            #印證公式
      round(pca$quanti.sup$coord,7)) 
all(round(pca$quanti.sup$coord,7)==     #印證公式
      round(sup.quan.coord,7))
####################### 方法二 ######################
PRCOMP<-prcomp(    #  主成分分析函式
  as.matrix(df),   #  分析資料
  center = TRUE,   #  是否計算行平均值
  scale. = TRUE    #  是否依據行標準差進行分析
)
g<-fviz_pca_biplot(    # 同上說明
  X=PRCOMP,
  geom = c("point","text"),
  ggtheme = theme_gray(),
  title = "咖啡品牌知覺定位(PCA - Biplot)",
  col.ind=km2gp$cluster%>%factor,
  repel=TRUE,
  mean.point=FALSE,
  labelsize=3,
  alpha.ind=1,
  pointshape=16,  # 標示點之形狀(請參閱show_point_shapes函式)
  addEllipses=TRUE,
  ellipse.type = "convex"
)
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/prcomp_biplot_1.svg",  #存檔目錄及檔名
  g,           # ggplot 繪圖物件
  scale = 1.5  # 繪圖板尺規範圍擴增倍數
) 
#################end of 4_5 ##################################