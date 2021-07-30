DIMNAMES<-c('A','B','C','D','E','F') # 糖果棒名稱
D<-matrix(    # 糖果棒之相似資料
  c(0,2,13,4,3,8,
    2,0,12,6,5,7,
    13,12,0,9,10,11,
    4,6,9,0,1,14,
    3,5,10,1,0,15,
    8,7,11,14,15,0),
  byrow=TRUE,
  nrow=6,
  dimnames=list(
    DIMNAMES,DIMNAMES
  )
)
print(as.dist(D))  # 列印距離矩陣
########### 使用MASS套件的isoMDS函式#################
library(MASS)
MDS<- isoMDS(  # 使用距離矩陣進行多元尺度分析
  d=as.dist(D),   # 距離矩陣(input data)
  k=2          # 期望的空間維度
)  
print(MDS)   #  列印MDS物件
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
print(stress(D,MDS$points)*100)  # 列印Stress 係數
###########依座標繪分析圖##############
mds<-as.data.frame(  # 將座標點資料轉成data frame 物件
  MDS$points
)
colnames(mds)<-c("Dim.1","Dim.2")
library(ggpubr)
g<-ggscatter(   # 產生散佈圖
  data=mds,  # 知覺資料  
  x="Dim.1", # x 軸對應於資料之欄位
  y="Dim.2", # y 軸對應於資料之欄位
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
print(g)
#######逆時鐘旋轉座標############
theta<- pi/3*2.5  # 旋轉角度
rottheta<-matrix(     # 逆時鐘theta角度旋轉矩陣各向量
  c(cos(theta),sin(theta),-sin(theta),cos(theta)),
  nrow=2)
mds<-as.data.frame(  # 將旋轉後座標點資料轉成data frame 物件
  t(rottheta%*%t(MDS$points))
)
colnames(mds)<-c("Dim.1","Dim.2")
##############