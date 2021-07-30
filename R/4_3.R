path<- 'data/orange_c.csv'  # 資料檔案指定於工作目錄之相對路徑
q<-read.csv(path,sep=';')   # 依分隔欄位符，讀取資料
rownames(q)=q[,1]           # 以資料第一行柳丁汁品牌名稱，做為資料列名
q<- q[,-1]                  # 已有資料列名，去除第一行
ka<- 1:7     # 活躍變數(active variable)
ks<- 8:14    # 補充量化變數(supplementary quantitative variable)
kc<- 15:16   # 補充類別變數(supplementary categorical variable)
kt<- c(ka,ks,kc)  # 所有變數
is<- 7       # 補充個體(supplementary individual)
q      # 列印整理後資料內容

corq<- round(   # 相關矩陣
  cor(q[,ka]),6)
covq<-round(   # 共變異數矩陣
  cov(scale(q[,ka])),6)  # 將行向量標準化
all(eigen(corq)$values==eigen(covq)$values)  # 驗證特徵值相同
all(eigen(corq)$vectors==eigen(covq)$vectors)  # 驗證特徵向量相同
##############方法一 使用FactoMineR套件進行PCA，#####
##############再以factoextra 繪圖              ######
library(FactoMineR)
library(factoextra)
res.pca<-PCA(  # 產生PCA 物件
  X=q[,kt],         # matrix物件(列為所有品牌，行為所有變數)
  quanti.sup=ks,    # 補充量化變數(行)
  quali.sup=kc,     # 補充類別變數(行)
  ind.sup=is,       # 補充個體(列)
  ncp=ncol(q[,ka]), # 維度數
  graph = TRUE      # 是否繪圖
)

print(res.pca) # 列出PCA 此list物件的內容
print(res.pca$eig) #  列出特徵值及其排序

plot(  # 列印特徵值折線圖
  x=res.pca$eig[,3],
  main='特徵值折線圖',
  type='b',   # 標示點與連線
  pch=16,      # 標示點之形狀(請參閱show_point_shapes函式)
  xlab='特徵值維度',
  ylab='累計佔比'
)

library(tidyr)
g<-fviz_pca_biplot(
  res.pca,         # PCA 物件
  repel=TRUE,      # 重疊文字是否錯開並加上引線 
  title = 'Orange Guice Data Analysis(PCA - Biplot)',  # 標題
  ggtheme = theme_minimal(),  # 繪圖主題
  labelsize=5,    #  文字大小
  pointshape=16   # 標示點之形狀(請參閱show_point_shapes函式)
)
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/orangejuice_biplot.svg",  #存檔目錄及檔名
  g,           # ggplot 繪圖物件
  scale = 1.5  # 繪圖板尺規範圍擴增倍數
)
##############方法二(細部解析) #############################
##### 變數與個體直接關聯圖 ##########
qn<- as.matrix(q[,c(ka,ks)][-is,])  # 擷取量化變數與活躍個體部分
library(ggplot2)
library(cowplot)
library(ggrepel)
p1 <- ggplot(as.data.frame(qn),
             aes(x=qn[,5],y=qn[,1])) + 
  labs(
    title='A',
    x =colnames(qn)[5],
    y =colnames(qn)[1]
  )+
  geom_point()+
  geom_label_repel(label=row(qn)[,1])
p2 <- ggplot(as.data.frame(qn), 
             aes(x=qn[,5],y=qn[,6])) + 
  labs(
    title='B',
    x =colnames(qn)[5],
    y =colnames(qn)[6]
  )+
  geom_point()+
  geom_label_repel(label=row(qn)[,1])
p3 <- ggplot(as.data.frame(qn),
             aes(x=qn[,3],y=qn[,1])) + 
  labs(
    title='C',
    x =colnames(qn)[3],
    y =colnames(qn)[1]
  )+
  geom_point()+
  geom_label_repel(label=row(qn)[,1])
p4 <- ggplot(as.data.frame(qn),
             aes(x=qn[,3],y=qn[,1])) + 
  labs(
    title='D',
    x =colnames(qn)[3],
    y =colnames(qn)[1]
  )+
  geom_point()+
  geom_label_repel(label=row(qn)[,1])
plot_grid(p1,p2,p3,p4)
#######################################
##### 步驟一:變數(行)標準化 ##########
center <- colMeans(qn, na.rm=TRUE)  #  平均值
sd<-sqrt(colSums((t(t(qn)-center)^2)/nrow(qn)))   # 母體標準差
std.qn<-t((t(qn)-center)/sd)  #  標準化資料(公式(4.3.1)
round(cbind(   # 變數標準化列表
  rbind(std.qn,'Total'=colSums(std.qn)), # 行合計
  'Total'=rowSums(rbind(std.qn,'Total'=colSums(std.qn)))), # 列合計
  8)
all(round(std.qn,8)==round(          #  驗證與scale函式差異
  scale(qn)/sqrt((nrow(qn)-1)/nrow(qn)),8)
)
##### 步驟二:列、行權重計算 ##########
print(t(std.qn[,ka])%*%std.qn[,ka])    # 列權重調整前
rw<- rep(1/nrow(std.qn),nrow(std.qn))  # 列權重
cw<- rep(1,ncol(std.qn[,ka]))          # 行權重
X<- t(t(std.qn[,ka]*sqrt(rw))*sqrt(cw)) # 公式(4.3.2)
print(X)    # 列印帶正負號的慣量(X^2)的平方跟
round(cbind(   # 投影前列慣量合計
  rbind(X^2,'Total'=colSums(X^2)), # 投影前行慣量合計
  'Total'=rowSums(rbind(X^2,'Total'=colSums(X^2))),
  '%'=c(t(rowSums(X^2))/sum(X^2),1)*100),
  6)
all(round(t(X)%*%X,7)==round(cor(std.qn[,ka]),7))  # 驗證(4.3.3)
##### 步驟三:奇異值分解 ##########
library(pracma)
minx<-Rank(X)   # 有效維度(秩)
PCS<- c('主成分1','主成分2','主成分3','主成分4','主成分5','主成分6')[1:minx]
eigx<-eigen(cov(X))      # 共變異數特徵分解
PC<-X%*%eigx$vectors         # 主成分
round(eigx$values,8)     # 分解之特徵值
diag(round(cov(PC),8))   # 主成分共變異數
######SVD 分解方法一######
if (nrow(X)<=ncol(X)){     # 是否為胖矩陣
  SVD<- svd(t(X),nv=minx,nu=minx)  # 奇異值分解
}else{        # 是否為瘦矩陣
  SVD<- svd(X,nv=minx,nu=minx)  # 奇異值分解
}
U<- ifelse(     # 左奇異矩陣
  nrow(X)<=ncol(X),list(SVD$v),list(SVD$u))[[1]] 
rownames(U)<-rownames(X)    # 指定列名
colnames(U)<-PCS            # 指定行名
V<- ifelse(     # 右左奇異矩陣
  nrow(X)<=ncol(X),list(SVD$u),list(SVD$v))[[1]]
rownames(V)<-colnames(X)    # 指定列名
colnames(V)<-PCS            # 指定行名
S<- replace(SVD$d,SVD$d<0,0)[1:minx]  # 奇異值小於0視為0為無效奇異值
lambda<- S^2     # 特徵值
round(X,9)==round(U%*%diag(S)%*%t(V),9)  # 驗證分解結果(4.3.4)
print(lambda)    # 列印特徵值
round(eigx$values[1:minx]*Rank(X),8)==round(lambda,8) 
######SVD 分解方法二######
eigu<- eigen(X%*%t(X))  
lambda<- replace(eigu$values,eigu$values<0,0) 
U<-eigu$vectors
V<- t(t(t(X)%*%U%*%diag(sqrt(lambda)))/eigu$values)
S<-sqrt(lambda)[1:minx]
lambda<-S^2
U<-U[,1:minx]
rownames(U)<-rownames(X)
colnames(U)<-PCS
V<-V[,1:minx]
rownames(V)<-colnames(X)
colnames(V)<-PCS
round(X,9)==round(U%*%diag(S)%*%t(V),9)  # 驗證分解結果(4.3.4)
#######主成分佔比################
df<-data.frame(    # 分解後各成分比率
  特徵值=lambda,
  特徵值佔比=round(lambda/sum(lambda)*100,2),
  特徵值佔比累積=round(cumsum(lambda/sum(lambda)*100),2))
rownames(df)<-  # 主成分順序
  c('Comp1','Comp2','Comp3','Comp4','Comp5')
print(df)  # 列印主成分順序
############計算座標###########
mult<-as.vector(sign(t(cw)%*%V)) #  變號因子
# mult<-rep(1,Rank(X))
P<- t(t(U)*S*mult)/sqrt(rw)    # 個體座標 (4.3.10)
Q<- t(t(V)*S*mult)/sqrt(cw)    # 變數座標 (4.3.11)
print(P)    # 列印活躍個體座標
print(Q)    # 列印活躍變數座標
all(round(P,6)==round(res.pca$ind$coord,6))  # 驗證變號後與方法一一致
all(round(Q,6)==round(res.pca$var$coord,6))  # 驗證變號後與方法一一致
round(diag(cov(X%*%t(t(V)*mult)))*Rank(X),8)==  # 變號後主成分共變異數
  round(lambda,8)   
#########投影前慣量################
sum(X^2)                # 投影前總慣量
#########投影後慣量################
inertia<-data.frame(    # 分解後各成分比率
  特徵值=lambda,
  特徵值佔比=round(lambda/sum(lambda)*100,2),
  特徵值佔比累積=round(cumsum(lambda/sum(lambda)*100),2))
rownames(inertia)<- PCS    # 主成分順序
rbind(inertia,        # 慣量(特徵值)比
      'Total'=c(sum(inertia[,1]),sum(inertia[,2]),NA))
round(cbind(   # 投影後個體慣量合計  (4.3.12)
  rbind(P^2,'Total'=colSums(P^2)), # 投影後個體慣量合計
  'Total'=rowSums(rbind(P^2,'Total'=colSums(P^2)))),6)
round(cbind(   # 投影後變數慣量合計
  rbind(Q^2,'Total'=colSums(Q^2)), # 投影後變數慣量合計
  'Total'=rowSums(rbind(Q^2,'Total'=colSums(Q^2)))),6)
sum(Q^2)                   # 投影後總慣量
###############################
print(cor(X,P))            # 列印X活躍變數與個體的主成分相關係數
all(round(cor(X,P),7)==round(Q,7))  # 驗證(4.3.14)
all(round(cor(X,P)^2,8)==round(res.pca$var$cos2,8))  # 驗證(4.4.4.6)

t(1/sqrt(lambda)*     # 驗證個體座標(4.3.15)
  t(std.qn[,ka]%*%Q))     
t(1/sqrt(lambda)*      # 驗證個體座標(4.3.16)
    t(t(std.qn[,ka])%*%P)/nrow(std.qn[,ka]))
t(1/sqrt(lambda)*
    t(std.qn[,ka]%*%res.pca$var$coord))  # 驗證個體座標(4.3.15)
t(1/sqrt(lambda)*      # 驗證個體座標(4.3.16)
    t(t(std.qn[,ka])%*%res.pca$ind$coord)/nrow(std.qn[,ka]))
#######慣量與品質###############
k<- 2   # 取二維繪圖座標
colSums(P^2)/sum(P^2)   # 個體投影慣量佔比(4.3.18)
colSums(Q^2)/sum(Q^2)   # 變數投影慣量佔比(4.3.18)
lambda/sum(lambda)      # 特徵值佔比(4.3.18)

t(t(P^2))/rowSums(P^2)  # 個體慣量分布(列投影品質)
t(t(Q^2))/rowSums(Q^2)  # 變數慣量分布(行投影品質)
t(t(P[,1:k]^2)/colSums(P[,1:k]^2))*100   # 個體慣量貢獻比(4.3.20)
t(t(Q[,1:k]^2)/colSums(Q[,1:k]^2))*100   # 變數慣量貢獻比(4.3.21)
#####補充個體主成分#####
sup.ind.std<-(q[is,ka]-center[ka])/sd[ka] # 補充個體變數標準化
sup.ind.coord<-t(      # 補充個體座標公式(4.3.22)
  t((as.matrix(sup.ind.std)%*%Q))/S)
print(sup.ind.coord)   # 列印補充個體座標
##########品牌分佈圖###################
xlabel=paste0('Dim1(',   #計算Dim1特徵值佔比
              round(S[1]^2/sum(S^2)*100,1)
              ,'%)')
ylabel=paste0('Dim2(',   #計算Dim2特徵值佔比
              round(S[2]^2/sum(S^2)*100,1)
              ,'%)')
plot(  # 列印個體分布圖
  x=P,
  main='品牌分布圖',
  type='p',   # 標示點與連線
  pch=16,      # 標示點之形狀(請參閱show_point_shapes函式)
  xlab=xlabel,
  ylab=ylabel,
  xlim=c(-3.5,4.5), ylim=c(-2,3)
)
abline(h = 0, v = 0, col = "gray60")
text(
  x=P[,1],
  y=P[,2],
  row.names(P),
  adj=c(0,-1),
  cex=.8
)
#####補充計量變數主成分#####
sup.quan.std<-std.qn[,ks,drop=FALSE]   # 取標準化補充計量變數
sup.quan.coord<-t(        # 公式(4.3.26)
  t(t(sup.quan.std)%*%P)/S/nrow(P))
print(sup.quan.coord)     # 列印補充變數座標印證(4.3.25)
print(cor(std.qn[,ks],P)) # 列印補充變數與活躍個體相關係數印證(4.3.25)
######補充類別變數主成分###############
k<- 2   # 取二維繪圖座標
sup.qual<- lapply(      # 計算各補充類別變數的活躍個體主成分均值
  q[-is,kc],
  function(x){
    aggregate(P[,1:k,drop=FALSE], by=list(category=x), FUN=mean)
  }
)
sup.qual.coord<- data.frame()
for(i in 1:length(sup.qual)){      # 計算各補充類別變數下個類別座標
  sup.qual.coord<-rbind(sup.qual.coord,sup.qual[[i]])
}
rownames(sup.qual.coord)<-sup.qual.coord[,1] # 維度命名
sup.qual.coord<-sup.qual.coord[,-1]
sup.qual.coord      #  列印補充類別座標
############整理繪圖資料#################
all(round(Q[,1:k],7)==round(res.pca$var$coord[,1:k],7))  # 驗證
all(round(P[,1:k],7)==round(res.pca$ind$coord[,1:k],7))  # 驗證
rx<-(max(P[,1])-min(P[,1]))/(max(Q[,1])-min(Q[,1]))
ry<-(max(P[,2])-min(P[,2]))/(max(Q[,2])-min(Q[,2]))
r<-min(rx,ry)  # 放大之比例
m<- 0.7 # 美觀係數
Q1<-Q*r*m   # 將活躍變數向量等比例放大
sup.quan.coord1<-sup.quan.coord*r*m  # 補充變數亦等比例放大
cod.df<-rbind(  #  整理繪圖資料
  base::transform(data.frame(P[,1:k,drop=FALSE]),rc=1),
  base::transform(data.frame(Q1[,1:k,drop=FALSE]),rc=2),
  base::transform(data.frame(sup.quan.coord1[,1:k,drop=FALSE]),rc=3),
  base::transform(data.frame(sup.ind.coord[,1:k,drop=FALSE]),rc=4),
  base::transform(sup.qual.coord,rc=5)
)
colnames(cod.df)<-c('x','y','rc')  # 繪圖資料行命名
#############雙標圖(方法二使用)########################
library(ggplot2)
library(ggrepel)
xlabel=paste0('Dim1(',   #計算Dim1特徵值佔比
              round(S[1]^2/sum(S^2)*100,1)
              ,'%)')
ylabel=paste0('Dim2(',   #計算Dim2特徵值佔比
              round(S[2]^2/sum(S^2)*100,1)
              ,'%)')
colors<- c('blue','#8B0000','#DC143C','#1200FF','black')
linetype<- c('blank','solid','dashed','blank','blank')
pointsize<- c(2,0,0,4,2)
gg<- ggplot(
  data=cod.df,   # 繪圖資料來源
  mapping=aes(x=x,y=y)  # x、y軸在引數data 的對應行
)+
  labs(
    title='PCA-Biplot',
    x =xlabel,
    y =ylabel
  )+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  geom_hline(yintercept=0,linetype="dashed", color = "#123456")+
  geom_vline(xintercept=0,linetype="dashed", color = "#123456")+
  geom_point(
    data=cod.df,
    mapping=aes(x=x,y=y),
    size = pointsize[cod.df$rc],
    color=colors[cod.df$rc]
  )+ # 畫出各點點狀圖
  scale_x_continuous(  # y軸為計量值之尺規標示
    limits=c(min(cod.df$x),max(cod.df$x)),
    breaks=seq(-3,3,by=0.5)
  )+
  scale_y_continuous(  # y軸為計量值之尺規標示
    limits=c(min(cod.df$y),max(cod.df$y)),
    breaks=seq(-3,3,by=0.5)
  )+
  geom_segment(
    data=cod.df,
    aes(x=0, xend=x, y=0, yend=y),
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    linetype=linetype[cod.df$rc],
    size = 0.5, 
    arrow = arrow(length = unit(10, "points")),
    colour=colors[cod.df$rc]
  )+
  geom_label_repel( # 疊加文字於圖
    data=cod.df, # 文字資料來源
    mapping=aes(
      x=x,y=y,
      label=row.names(cod.df)
    ), 
    colour=colors[cod.df$rc],
    show.legend =FALSE,
    hjust=-0.1, # 文字位置水平向右幾個字寬
    vjust=-0.1  # 文字位置垂直向上調整幾個字高
  )
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/gg_biplot.svg",  #存檔目錄及檔名
  gg,           # ggplot 繪圖物件
  scale = 1.5  # 繪圖板尺規範圍擴增倍數
)
############## end of 4_3 #########################