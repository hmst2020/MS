DIMNAMES<-c('A','B','C','D','E','F') # 糖果棒名稱
M<-matrix(    # 糖果棒之相似資料
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
as.dist(M)   # 列出調查資料

M.std<-scale(as.dist(M),center=TRUE,scale=TRUE)  # 將調查資料標準化
DIST<-dist(M.std) # 依標準化之資料重新計算距離(相似性)
DIST  # 於console 列出物件DIST

########### 方法一 #################
CMDSCALE<-cmdscale(    # 計量(Metric)多元尺度分析函式
  d=DIST,     # 距離物件(dist) 
  k=2,        # 壓縮之維度
  eig=TRUE    # 是否計算特徵空間資料
)


########## 特徵值列表整理 ################
eig<-round(CMDSCALE$eig,digits=8)
eig.df<-data.frame(eig=eig,prop=eig/sum(eig))
eig.df<-transform(eig.df,cum=cumsum(eig.df$prop))
print(eig.df)  # 列印特徵值占比累計表
plot(  # 列印特徵值折線圖
  x=1:nrow(eig.df), # 特徵值維度
  y=eig.df$cum,     # 特徵值維度累計比例
  type='b',   # 標示點與連線
  pch=16,     # 標示點之形狀(請參閱show_point_shapes函式)
  xlab='特徵值維度',
  ylab='累計佔比'
)
#####################################
sdis<-dist(CMDSCALE$points)  # 計算相似性數據導出的距離
stress<-(sum((DIST-sdis)^2)/sum(DIST^2))^0.5 # 計算壓力係數值
print(stress) # 列印壓力係數值

colnames(CMDSCALE$points) <- c("Dim.1", "Dim.2") # 賦予欄位名稱
mds<-as.data.frame(  # 將座標點資料轉成data frame 物件
  CMDSCALE$points
) 
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
    )+
  xlab(    # x軸標籤文字
    paste0('Dim.1(',round(eig[1]/sum(eig)*100,digits=2),'%)')
    )+
  ylab(    # x軸標籤文字
    paste0('Dim.2(',round(eig[2]/sum(eig)*100,digits=2),'%)')
    )
print(g)     # 列印散佈圖

########### 方法二 #################
library(MASS)
MDS<- isoMDS(d=DIST)  # 使用標準化之距離資料進行多元尺度分析
print(MDS)   #  列印MDS物件

mds<- as.data.frame(MDS$points)  # 將座標點資料轉成data frame 物件
colnames(mds) <- c("Dim.1", "Dim.2") # 賦予欄位名稱
library(ggpubr)
g<-ggscatter(   # 列印散佈圖
  data=mds,  # 知覺資料  
  x="Dim.1", # x 軸對應於資料之欄位
  y="Dim.2", # x 軸對應於資料之欄位
  label=rownames(mds), # 文字標示資料依據
  shape=18, # 標示點之形狀
  size=2,   # 點的大小
  repel=TRUE # 避免臨界資料其文字標示於圖外
)+
  geom_hline(yintercept=0,linetype="dashed", color = "#5634AE")+
  geom_vline(xintercept=0,linetype="dashed", color = "#6543AF")
print(g)    # 列印散佈圖

###################end of 4.1########################################



ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/candy_bar.svg", # 檔案名稱及位置
  g,           # ggplot 繪圖物件
  scale = 1  # 繪圖板尺規範圍擴增倍數
)