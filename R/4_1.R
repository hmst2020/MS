# 載入城市距離矩陣
D<-as.matrix(readRDS(file = "data/eurocities.rds"))
isSymmetric(D)  # 檢查是否為對稱矩陣
print(as.dist(D))  # 列印距離矩陣
########### 使用stats套件的cmdscale函式#################
CMDSCALE<-cmdscale(    # 計量(Metric)多元尺度分析函式
  d=as.dist(D),     # 距離物件(dist) 
  eig=TRUE          # 是否計算特徵空間資料
)
print(CMDSCALE$GOF)  # 列印適合度(Goodness-of-fit)
print(CMDSCALE$eig)  # 列印特徵值
points<-CMDSCALE$points    
print(points)        # 列印維度座標
sum(abs(CMDSCALE$eig[1:2]))/sum(abs(CMDSCALE$eig)) # GOF.1
sum(CMDSCALE$eig[1:2])/sum(pmax(CMDSCALE$eig,0)) # GOF.2
######## 依座標繪分析圖(適用於各分析方法)#########
theta<- pi/3   # 旋轉角度
rottheta<-matrix(     # 60度逆時鐘旋轉矩陣各向量
  c(cos(theta),sin(theta),-sin(theta),cos(theta)),
  nrow=2)
mds<-as.data.frame(  # 將旋轉後座標點資料轉成data frame 物件
  t(rottheta%*%t(points))
)
rownames(mds) <- rownames(D)    # 賦予列名
colnames(mds) <- c("Dim.1", "Dim.2") # 賦予欄位名稱
print(mds)  # 列印旋轉後座標
library(ggpubr)
g<-ggscatter(   # 產生散佈圖
  data=mds,  # 知覺資料  
  x="Dim.1", # x 軸對應於資料之欄位
  y="Dim.2", # x 軸對應於資料之欄位
  label=rownames(mds), # 文字標示資料依據
  shape=18, # 標示點之形狀
  size=2,   # 點的大小
  repel=TRUE # 避免臨界資料其文字標示於圖外
)+
  geom_hline(   # 於y軸0處畫一橫虛線
    yintercept=0,linetype="dashed", color = "#5634AE"
  )+
  geom_vline(   # 於x軸0處畫一直虛線
    xintercept=0,linetype="dashed", color = "#6543AF"
  )
ggsave(  # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/eurocities.svg",  #存檔目錄需存在，否則會有錯誤拋出
  g,           # ggplot 繪圖物件
  scale = 1    # 繪圖板尺規範圍擴增倍數
)
#####  Kruskal壓力(Stress)係數計算 #####
stress<- function(D,points){
  ord.f<-order(   #距離矩陣(input data) 由小而大順序位置
    as.dist(D)
  )
  ord.d<-order(   #dij 由小而大順序位置
    dist(points)
  )
  iso<-isoreg( #單調遞增迴歸函式(isotonic regression或稱monotonic regression)
    x=as.dist(D)[ord.f],  # 將input data 依大小順序排序
    y=dist(points)[ord.d]   # 將dij 依大小順序排序
  )
  d<-iso[["yf"]][order(ord.f)] #將單調遞增迴歸資料還原順序同距離矩陣(input data)順序
  ed<-dist(points)             # 將output data產生距離矩陣
  result<-sqrt(sum((d-ed)^2)/sum(d^2))  # 依Kruskal Stress-1公式
  return (result)
}
print(stress(D,points)*100)  # 列印Stress 係數
###### 計算squared correlation######
RSQ<- function(od,nd){ #od: 原距離矩陣  nd: 主座標分解後之距離矩陣
  #return (1-sum((c(od)-c(nd))^2)/sum((c(od)-mean(c(od)))^2))  # RSQ
  return (cor(c(od), c(nd))^2)    # 公式(4-4)
}
rsq<-RSQ(as.dist(D), dist(points))
print(rsq) # 列印squared correlation 係數
########### 使用ape 套件的pcoa函式#################
library(ape)
PCOA<-pcoa(    # 主座標分解(principal coordinate decomposition)函式
  D=as.dist(D),       # 距離物件(dist) 
  correction="none"   # 是否調整負特徵值
)
k<- 2   # 指定二維主座標
points <- PCOA$vectors[, seq_len(k), drop = FALSE]
sum(abs(CMDSCALE$eig[1:2]))/sum(abs(CMDSCALE$eig)) # GOF.1
sum(CMDSCALE$eig[1:2])/sum(pmax(CMDSCALE$eig,0)) # GOF.2
print(stress(D,points)*100) 
rsq<-RSQ(as.dist(D), dist(points))
print(rsq) # 列印squared correlation 係數
########### 解析 ################
# 步驟一 計算 Gower's centered similarity matrix   ########
DD<- -0.5*D^2 
# 雙中心化矩陣(double-centred matrices)
# 方法 1
n<-nrow(DD)
idp <- diag(n) - matrix(1,n,n)/n   # 從I-1/n*11T 計算冪等(idempotent)矩陣
B <- idp %*% DD %*% idp
# 方法 2
B <- scale( # 使用scale函式，center引數設為TRUE，scale 引數設為FALSE
  t(scale(t(DD),center=TRUE,scale=FALSE)),
  center=TRUE,
  scale=FALSE
)
# 方法 3
R = DD*0 + rowMeans(DD)
C = t(DD*0 + colMeans(DD))
B<-DD - R - C + mean(DD)
# 方法 4
library(MDMR)
B<-gower(d.mat=D) # 使用MDMR套件的gower函式

# 上述各方法
print(round(rowSums(B),8))  # 各列加總(檢查中心化)
print(round(colSums(B),8))  # 各行加總(檢查中心化)
#######################end of 4.1###############
# 步驟二 求算特徵值及特徵向量
### 特徵分解方法 1 ######
library(pracma)
o<-orth(B)  # 求算行(column)之標準正交基(又稱單範正交基底，orthonormal basis)
rn<-ncol(B)-qr(B)$rank  # 測試A矩陣的Rank value(列秩)，亦即存在共線向量(collinear vectors)
SQ<-1-rowSums(o^2)
for (i in 1:rn){
  f<- 1/(rn*2-1)
  o<-cbind( # 補上column使rank與行數差異，使列亦為單位典範
    o,
    sqrt(SQ*f)
    )
}
eigvalue<-diag(solve(o)%*%B%*%o)  # 取對角矩陣之對角為特徵值
eigvector<-o[,order(   # 將特徵列向量與特徵值排序(由大而小)同步
  eigvalue,
  decreasing=TRUE
)]
eigvalue<-sort(eigvalue,decreasing=TRUE) # 將特徵值排序(由大而小)
ex<- list(values=eigvalue,vectors=eigvector)

### 特徵分解方法 2 ######
ex <- eigen(B)  # 對稱矩陣特徵值分解

# 上述各方法
print(ex) # 列印分解結果
V<-ex$vectors
all(round(B%*%V,6)==
      round(V%*%diag(ex$values),6))  # 驗證方程式4.1.1
round(V%*%t(V),8)  # 是否為正交
all(round(V%*%t(V),8)==round(t(V)%*%V,8)) # 是否為正交
rowSums(V^2)  # 是否模長為 1
colSums(V^2)  # 是否模長為 1

# 步驟三 計算座標
k<-2
ev <- ex$values[seq_len(k)]  # 取特徵值前二維度
evec <- ex$vectors[, seq_len(k), drop = FALSE] # 取特徵向量前二行為座標依據
points <- t(t(evec) * sqrt(ev))  # 將座標依長度比例擴充
sum(abs(ev))/sum(abs(ex$values)) # GOF.1
sum(ev)/sum(pmax(ex$values,0)) # GOF.2
print(stress(D,points)*100)    # 壓力係數
# 步驟四 依座標繪分析圖(同上)
theta<- pi/3   # 旋轉角度
theta1<- 0     # 映射軸與水平軸之夾角
rottheta<-matrix(     #逆時鐘旋轉矩陣
  c(cos(theta),sin(theta),-sin(theta),cos(theta)),
  nrow=2) 
rottheta1<-matrix(    # 映射矩陣
  c(cos(theta1*2),sin(theta1*2),sin(theta1*2),-cos(theta1*2)),
  nrow=2)
mds<-as.data.frame(  # 將旋轉後座標點資料轉成data frame 物件
  t(rottheta%*%(rottheta1%*%t(points)))   #先映射後旋轉
)
rownames(mds) <- rownames(D)    # 賦予列名
colnames(mds) <- c("Dim.1", "Dim.2") # 賦予欄位名稱
############end of 4.1 ####################