#############################################
# 讀取學生的酒類喜好問卷調查資料            #
#############################################
path<- 'data/preferences_32.csv'  # 資料檔案指定於工作目錄之相對路徑
q<-read.csv(path)  # 讀取問卷調查資料
q<-data.frame(
  q[,2:length(q)],  # 資料第二欄起為酒類喜好值
  row.names=q[,1]  # 擷取資料第一欄為rownames
)
head(q) # 列印前六筆
tail(q) # 列印倒數六筆

km<-kmeans(  #  原始觀察值分群
  x=q,
  centers=3,
  iter.max = 10,
  nstart = 25
)
options(width=180) # 將console output 寬度調整容納長資料寬度
sink(  # 將console output 轉向至文字檔
  file="E:/temp/km.txt",
  type="output"
)
km  # 列印kmeans分群結果
sink()  # 結束console轉向

library(factoextra)
fviz<-fviz_cluster( # 將分群結果產生可視化物件
  object=km,     # 各object 依kmeans產生的分群結果
  data=q,         # 依酒類排列的標準化觀察值
  geom=c('point','text'), # 幾何圖形包含點標示與其文字
  repel=TRUE,     # 重疊文字是否錯開並加上引線
  show.clust.cent=TRUE,  # 是否標示各群組中心點
  pointsize=1,    # 幾何圖點標示符大小
  labelsize=10,   # 幾何圖標示之文字大小
  stand = TRUE,   # 主成分分析前是否處理標準化
  ellipse=TRUE,   # 是否繪出群組外框，形狀依ellipse.type
  ellipse.type='convex', # 群組外框框架型態
  main='Student cluster'  # 圖標題
)
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/fviz.svg",  #存檔目錄及檔名
  fviz,           # ggplot 繪圖物件
  scale = 1.5  # 繪圖板尺規範圍擴增倍數
)
#####################依啤酒品牌區分群組########################
km.beer<-kmeans(  #  原始觀察值分群
  x=t(q),
  centers=3,
  iter.max = 10,
  nstart = 25
)
options(width=180) # 將console output 寬度調整容納長資料寬度
sink(  # 將console output 轉向至文字檔
  file="E:/temp/km.beer.txt",
  type="output"
)
km.beer  # 列印kmeans分群結果
sink()  # 結束console轉向

library(factoextra)
fviz.beer<-fviz_cluster( # 將分群結果產生可視化物件
  object=km.beer,     # 各object 依kmeans產生的分群結果
  data=t(q),         # 依酒類排列的標準化觀察值
  geom=c('point','text'), # 幾何圖形包含點標示與其文字
  repel=TRUE,     # 重疊文字是否錯開並加上引線
  show.clust.cent=TRUE,  # 是否標示各群組中心點
  pointsize=1,    # 幾何圖點標示符大小
  labelsize=10,   # 幾何圖標示之文字大小
  stand = TRUE,   # 主成分分析前是否處理標準化
  ellipse=TRUE,   # 是否繪出群組外框，形狀依ellipse.type
  ellipse.type='norm', # 群組外框框架型態
  main='Beer cluster'  # 圖標題
)
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/fviz_beer.svg",  #存檔目錄及檔名
  fviz.beer,           # ggplot 繪圖物件
  scale = 1.5  # 繪圖板尺規範圍擴增倍數
)
######################## end of 3.5 ####################