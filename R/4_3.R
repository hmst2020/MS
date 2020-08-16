df<- data.frame(   #  建立品牌受訪彙總資料
  c(3.10,2.35,3.23,2.9,2.65),
  c(2.55,2.68,2.69,2.62,3.17),
  c(2.85,2.84,2.79,2.74,3.19),
  c(2.98,3.21,2.77,2.70,2.71),
  c(2.28,2.07,2.73,2.57,1.72),
  c(2.66,3.27,2.65,2.66,2.23),
  c(2.42,2.62,2.13,2.41,2.81),
  c(2.22,2.22,1.99,2.09,2.50)
)
rownames(df)<-c(      # 賦予列名稱(也是object名稱)
  '丹堤','星巴克','85 度 C',
  '壹咖啡',
  '理想點(準則權重)')
colnames(df)<-c(      # 賦予行名稱
  '種類樣式多','獨具風格','整體表現優異',
  '具有良好形象','招牌印象深刻','知名度高',
  '專業表現','可信度高')
km2gp<-kmeans(  #  觀察值分群函式
  x=df,         #  分群之資料來源
  centers=3,    #  指定分群數
  iter.max = 10,  # 最多迭代計算次數
  nstart = 25   # 初始隨機分組之取組數(決定盡快達成穩定分組)
)
options(width=180) # 將console output 寬度調整容納長資料寬度
sink(  # 將console output 轉向至文字檔
  file="E:/temp/km2gp.txt",
  type="output"
)
km2gp  # 列印kmeans分群結果
sink()
library(factoextra)
library(dplyr)
####################### 方法一 ######################
pca<-FactoMineR::PCA(     # 主成分分析函式
  X=df,       # 分析資料
  scale.unit=TRUE,  # 是否將資料標準化再分析
  ncp=nrow(df)      # 保留分析維度資料
)
g<-fviz_pca_biplot(   #  產生object及其變數(屬性)雙標繪圖物件
  X=pca,              #  依據物件(pca或prcomp物件皆可)
  geom = c("point","text"),  # 點及文字標示object位置
  ggtheme = theme_gray(),    # 使用之布景主題
  title = "咖啡品牌知覺定位(PCA - Biplot)", # 圖標題
  col.ind=km2gp$cluster%>%factor,  # object 群組之顏色依據
  repel=TRUE,                      # 重疊文字是否錯開並加上引線
  mean.point=FALSE,                # 是否顯示各組之中心點
  labelsize=3,                     # 文字大小
  alpha.ind=1,                     # 顏色之透明度
  pointshape=16,  # 標示點之形狀(請參閱show_point_shapes函式)
  addEllipses=TRUE,                # 是否將群組打圈，形狀依ellipse.type
  ellipse.type = "convex" # 群組形狀convex為群組內外圍各點連成多邊形
)
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/pca_biplot.svg",  #存檔目錄及檔名
  g,           # ggplot 繪圖物件
  scale = 1.5  # 繪圖板尺規範圍擴增倍數
) 
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
#################end of 4.3 ##################################
library(ggbiplot)
g<-ggbiplot(
  pca,
  var.scale=1,
  label.repel=TRUE,
  loadings.label.repel=TRUE,
  varname.size=4  # 屬性(變數)字體大小
)
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/pca_biplot_1.svg",  #存檔目錄及檔名
  g,           # ggplot 繪圖物件
  scale = 1.5  # 繪圖板尺規範圍擴增倍數
)
######################################
colnames(df)<-c(      # 賦予行名稱
  '產品種類樣式多','產品獨具風格','整體產品表現優異',
  '品牌具有良好形象','招牌醒目印象深刻','品牌知名度高',
  '品牌的專業表現','品牌可信度高')