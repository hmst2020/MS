###############資料與檢驗######################
X<-as.matrix(  # 讀取本例預存之data frame次數資料，並轉換成matrix
  readRDS(file = "data/vndefect.rds")
)
cbind(   # 列合計
  rbind(X,'Total'=colSums(X)), # 行合計
  'Total'=rowSums(rbind(X,'Total'=colSums(X))))
chitbl<-chisq.test(X,correct = TRUE)  # 卡方檢驗(有陣列元素值含<5的警告)
print(chitbl)        # 列印檢測結果
qchisq(.95, df=21)   # 計算臨界值(critical value)
fisher.test(X,simulate.p.value=TRUE)  # 費雪檢驗
###########方法一(使用graphicsR套件之mosaicplot函式)###############
mosaicplot(      # 繪馬賽克圖(不良原因與供應商對應圖)
  X,
  main='',
  color=c('red','green','blue','orange'),
  type='pearson')
###########方法二(使用FactoMineR套件 之CA函式)###############
library(ca)
xca<-ca(X)   # 簡易對應分析
graphics::plot(xca)  #雙座標圖(不良原因與供應商對應圖)
print(xca)   # 列印簡易對應分析物件內容
round(sum(xca$sv^2),8)==     # 驗證特徵值和與總慣量(列慣量和)
  round(sum(xca$rowinertia),8)
round(sum(xca$sv^2),8)==     # 驗證特徵值和與總慣量(行慣量和)
  round(sum(xca$colinertia),8)
###########方法三(使用FactoMineR套件 之CA函式)###############
library(FactoMineR)
ca.r<-CA(        #  對應分析
  X=X,
  graph=FALSE)    # 只傳回分析物件，不繪圖
print(ca.r)      # 列印分析物件內容
library(factoextra)
fviz_ca_biplot(  # 繪出雙標圖
  X=ca.r,    # CA函式傳回之物件
  geom=c('arrow','text'),   # 向量箭頭與文字標示並呈
  repel=TRUE)       # 文字避開重疊
fviz_contrib(ca.r, choice = "row", axes = 1)  # 列慣量貢獻條狀圖
mean(rmass)   # 平均列慣量
###########################

##############方法四(細部解析) #############################
N<- sum(X)  # 總數(grand total)
Z<- X/N     # 將次數資料轉成比例(總次數佔比)資料，公式((4.8.1)
rmass<- rowSums(Z)   # 列群集、列質量(row masses) (4.8.2)
cmass<- colSums(Z)   # 行群集、行質量(column masses) (4.8.2)
cbind(   # 實際機率分佈
  rbind(Z,'Total'=cmass),   # 加上行合計
  'Total'=rowSums(rbind(Z,'Total'=cmass)))  # 加上列合計

E = rmass %o% cmass   # Chi-square 獨立模型期望機率，公式(4.8.3)
cbind(   # 期望機率分佈
  rbind(E,'Total'=colSums(E)), # 加上行合計
  'Total'=rowSums(rbind(E,'Total'=colSums(E))))  # 加上列合計
round(  # Chi-square 期望值(次數)(theoretical frequence)
  cbind(
    rbind(E*N,'Total'=colSums(E*N)), # 加上行合計
    'Total'=rowSums(rbind(E*N,'Total'=colSums(E*N))))) # 加上列合計
rowSums(E)==rmass    # 驗證
colSums(E)==cmass    # 驗證

X2.dist<-((X-E*N)^2)/(E*N)   # 卡方值分佈
round(     # 卡方值分佈及行、列合計
  cbind(
    rbind(X2.dist,'Total'=colSums(X2.dist)),
    'Total'=rowSums(rbind(X2.dist,'Total'=colSums(X2.dist)))),
  2)
print(sum(X2.dist))    # 列印卡方值
####距離矩陣計算(一)######
R<- (Z - E)/sqrt(E)   # 帶正負號的慣量平方跟(4.8.6)
print(R)   # 列印 R 帶正負號的慣量平方跟
all_equal(round(R^2,8),round(X2.dist/N,8))  #驗證慣量分布(4.8.7)
####距離矩陣計算(二)#####
R<-       # 印證公式(4.8.8)
solve(sqrt(diag(rmass)))%*% 
  (Z-rmass%*%t(cmass))%*%  
  solve(sqrt(diag(cmass)))
rownames(R)<-rownames(Z)
colnames(R)<-colnames(Z)
print(R)   # 列印R公式(4.8.8)
################SVD #################################
library(pracma)
minx<-Rank(R)   # 有效維度
PCS<- c('主成分1','主成分2','主成分3','主成分4')[1:minx]
if (nrow(R)<=ncol(R)){
  SVD<- svd(t(R),nv=minx,nu=minx)  # 奇異值分解
}else{
  SVD<- svd(X,nv=minx,nu=minx)  # 奇異值分解
}
U<- ifelse(     # 左奇異矩陣
  nrow(R)<=ncol(R),list(SVD$v),list(SVD$u))[[1]] 
rownames(U)<-rownames(R)
colnames(U)<-PCS
V<- ifelse(     # 右左奇異矩陣
  nrow(R)<=ncol(R),list(SVD$u),list(SVD$v))[[1]]
rownames(V)<-colnames(R)
colnames(V)<-PCS
S<- replace(SVD$d,SVD$d<0,0)[1:minx]  # 奇異值小於0視為0為無效奇異值
lambda<- S^2     # 特徵值
print(list(d=S,u=U,v=V))     # 列印奇異分解結果
round(R,10)==round(U%*%diag(S)%*%t(V),10)  # 驗證公式(4.4.8)
df<-data.frame(    # 分解後各成分比率
  特徵值=lambda,
  特徵值佔比=round(lambda/sum(lambda)*100,2),
  特徵值佔比累積=round(cumsum(lambda/sum(lambda)*100),2))
rownames(df)<-  # 主成分順序
  c('Comp1','Comp2','Comp3')
print(df)  # 列印主成分順序
############計算座標########
P<-t(t(U/sqrt(rmass))*S)    # Ni 座標，公式(4.8.11)
Q<- t(t(V/sqrt(cmass))*S)    # Nj 座標，公式(4.8.12)
round(rmass%*%(P^2),8)==round(lambda,8)   #驗證公式(4.8.13)
round(cmass%*%(Q^2),8)==round(lambda,8)   #驗證公式(4.8.14)
round(sum(lambda),8)==round(sum(R^2),8)   #驗證公式(4.8.15)
###############################
k<- 2    # 平面維度
mult<-as.vector(sign(t(rep(1,nrow(V)))%*%V)) #  變號因子
P.coord<-t(t(P)*mult)     # V1座標
Q.coord<-t(t(Q)*mult)     # V1座標
cod.df<-rbind(  #  整理繪圖資料
  transform(data.frame(  # 在data frame增加欄位rc
    P.coord[,c(1:k)],
    row.names=rownames(X)),
    rc='r'),
  transform(data.frame(   # 在data frame增加欄位rc
    Q.coord[,c(1:k)],
    row.names=colnames(X)),
    rc='c')
)
colnames(cod.df)<-c('x','y','rc')  # 繪圖資料行命名
################出圖#####################
library(ggplot2)
library(ggrepel)
xlabel=paste0('Dim1(',   #計算Dim1特徵值佔比
              round(S[1]^2/sum(S^2)*100,1)
              ,'%)')
ylabel=paste0('Dim2(',   #計算Dim2特徵值佔比
              round(S[2]^2/sum(S^2)*100,1)
              ,'%)')
ggplot(
  data=cod.df,   # 繪圖資料來源
  mapping=aes(x=x,y=y))+  # x、y軸在引數data 的對應行
  labs(
    title='CA-Biplot',
    x =xlabel,
    y =ylabel)+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  geom_hline(yintercept=0,linetype="dashed", color = "#123456")+
  geom_vline(xintercept=0,linetype="dashed", color = "#123456")+
  geom_point(      # 畫出各點點狀圖
    data=cod.df,
    mapping=aes(x=x,y=y),
    color=ifelse(cod.df$rc=='r','blue','red'))+
  scale_x_continuous(  # y軸為計量值之尺規標示
    limits=c(min(cod.df$x),max(cod.df$x)),
    breaks=seq(-1,1,by=0.05))+
  scale_y_continuous(  # y軸為計量值之尺規標示
    limits=c(min(cod.df$y),max(cod.df$y)),
    breaks=seq(-1,1,by=0.05))+
  geom_segment(
    data=cod.df,
    aes(x=0, xend=x, y=0, yend=y),
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 0.5, 
    arrow = arrow(length = unit(10, "points")),
    colour = ifelse(cod.df$rc=='r','blue','red'))+
  geom_label_repel( # 疊加文字於圖
    data=cod.df, # 文字資料來源
    mapping=aes(
      x=x,y=y,
      label=row.names(cod.df)),
    colour=ifelse(cod.df$rc=='r','blue','red'),
    show.legend =FALSE,
    hjust=-0.4, # 文字位置水平向右幾個字寬
    vjust=-0.1)    # 文字位置垂直向上調整幾個字高
#################背後數據#######################
prop.r<-prop.table(P^2, 1) # 每列在基底上的投影的平方(據以計算品質)
prop.c<-prop.table(Q^2, 1) # 每列在基底上的投影的平方(據以計算品質)
q.r<-(prop.r[,1]+prop.r[,2])/rowSums(prop.r)  # 列品質
q.c<-(prop.c[,1]+prop.c[,2])/rowSums(prop.c)  # 行品質
rintia<- rowSums(R^2)  #  列慣量(row interia)
cintia<- colSums(R^2)  #  行慣量(column interia)
r.in<-rintia/sum(rintia) # 列慣量比(ratio of row interia)
c.in<-cintia/sum(cintia) # 行慣量比(ratio of column interia)
rp<- Z/rmass # 列剖面(row profile)
cp<- t(Z)/cmass  # 行剖面(column profile)
d2i<- colSums((t(rp)-cmass)^2/cmass)    # 列距離公式(4.8.16)
cos2.r<-P^2/d2i     # 公式(4.8.18)
d2j<- colSums((t(cp)-rmass)^2/rmass)    # 行距離公式(4.8.17)
cos2.c<-Q^2/d2j      # 公式(4.8.19)
con.r <- t(t(P^2*rmass)/S^2)  # 列慣量貢獻比(4.8.20)
con.c <- t(t(Q^2*cmass)/S^2)  # 行慣量貢獻比((4.8.21))
df.sum<- round(data.frame(
  品質=c(q.r,q.c),
  質量=c(rmass,cmass),
  慣量=c(r.in,c.in),
  第一主成分座標=c(P[,1],Q[,1]),
  第一主成分相關係數=c(cos2.r[,1],cos2.c[,1]),
  第一主成分貢獻=c(con.r[,1],con.c[,1]),
  第二主成分座標=c(P[,2],Q[,2]),
  第二主成分相關係數=c(cos2.r[,2],cos2.c[,2]),
  第二主成分貢獻=c(con.r[,2],con.c[,2])
),3)
print(df.sum)  # 列印整體分析

library("corrplot")
corrplot(cor(X,P.coord))   # 瑕疵品數與不良原因相關係數熱圖
############## end of 4_8.R ##################