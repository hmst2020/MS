path<- 'data/preferences_32.csv'  # 資料檔案指定於工作目錄之相對路徑
q<-read.csv(path)  # 讀取問卷調查資料
q<-data.frame(
  q[,2:length(q)],  # 資料第二欄起為酒類喜好值
  row.names=q[,1]  # 擷取資料第一欄為rownames
)
head(q)           # 列印前六筆
writeLines("\n")  # 輸出跳行符號
tail(q)           # 列印倒數六筆

km3<-kmeans(  #  觀察值分群函式
  x=q,         #  分群之資料來源
  centers=3,    #  指定分群數
  iter.max = 10,  # 最多迭代計算次數
  nstart = 25   # 初始隨機分組之取組數(決定盡快達成穩定分組)
)
options(width=180) # 將console output 寬度調整容納長資料寬度
sink(  # 將console output 轉向至文字檔
  file="E:/temp/km3.txt",
  type="output"
)
km3  # 列印kmeans分群結果
sink()  # 結束console轉向

library(FactoMineR)
library(factoextra)
res.pca<-PCA(  # 產生PCA 物件
  X=q,         # data frame物件(列為學生，行為啤酒品牌)
  ncp=ncol(q)  # 維度數
)
print(res.pca) # 列出PCA 此list物件的內容
print(res.pca$eig) #  列出特徵值及其排序

plot(  # 列印特徵值折線圖
  x=res.pca$eig[,3],
  type='b',   # 標示點與連線
  pch=16,      # 標示點之形狀(請參閱show_point_shapes函式)
  xlab='特徵值維度',
  ylab='累計佔比'
)

g<-fviz_pca_biplot(
  res.pca,         #
  repel=TRUE,
  mean.point=FALSE,
  col.ind=km2$cluster%>%factor,  # object 群組之顏色依據
  title = "Preference Analysis(PCA - Biplot)",
  labelsize=5,
  alpha.ind=1,
  pointshape=16,  # 標示點之形狀(請參閱show_point_shapes函式)
  addEllipses=TRUE,                # 是否將群組打圈，形狀依ellipse.type
  ellipse.type = "norm" # 群組形狀convex為群組內外圍各點連成多邊形
)
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/mdspref_biplot.svg",  #存檔目錄及檔名
  g,           # ggplot 繪圖物件
  scale = 1.5  # 繪圖板尺規範圍擴增倍數
) 
################# end of 4.2################