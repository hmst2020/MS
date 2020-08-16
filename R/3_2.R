###########歐氏距離公式計算#####################
part1<- (580-530)^2 +(550-550)^2
part1^0.5

#############################################
# 學生資料與分布圖                          #
#############################################
# R環境選項變數設定
options(width=180) # 將console output 寬度調整容納長資料寬度
# 學生成績資料
enroll<- data.frame(  # data.frame函式建構觀察值
  GPA  =c(3.0,3.2,3.4,3.7,3.8,
          4.0,3.1,3.7,3.6,3.5,
          3.4,3.5,3.6,3.4,3.0),
  TOEFL=c(580,530,570,600,630,
          590,570,580,570,540,
          570,550,550,580,550),
  GMAT =c(550,550,570,580,600,
          620,540,540,560,570,
          570,520,530,640,540),
  Work =c(2,0,6,1,0,
          0,2,0,4,3,
          0,4,4,0,1)
)
# 繪出入學申請成績(TOEFL、GMAT)分佈圖
plot(   # 產生繪圖物件
  x=enroll$TOEFL,  # x 軸資料欄
  y=enroll$GMAT,   # y 軸資料欄
  type='n',  # 資料不繪出
  xlab='TOEFL',    # x 軸標籤文字
  ylab='GMAT',     # y 軸標籤文字
  main="Student's TOEFL & GMAT"    # 標題文字
)
text( # 於繪圖物件標示學生代號
  x=enroll$TOEFL,   # 同上
  y=enroll$GMAT,    # 同上
  labels=rownames(enroll)   # 學生代號
)
###########################################
# 將不同尺規度量的資料標準化(standarlize) #
###########################################
enroll.std<- scale(  # 標準化觀察值
  x=enroll,
  center=TRUE,  # 一致位移以0為中心
  scale=TRUE    # 以標準差為一致之單位
)
print(enroll.std) # 列印標準化之結果
print(summary(enroll.std)) # 列印彙總之結果
##############################################
# 以歐氏距離(Euclidean distance)分析(未分群) #
##############################################
dist.euclidean <- dist( # 計算交互之歐氏距離
  x=enroll.std,         # 標準化資料
  method = "euclidean", # 計算歐氏距離
  diag = TRUE,          # 含對角線(對應自身距離)之數字
  upper = TRUE          # 為閱讀方便上半部數字亦印出
)
options(width=500) # 將console output 寬度調整容納長資料寬度
sink(  # 將console output 轉向至文字檔
  file="E:/temp/dist_euclidean.txt",
  type="output"
)
print(dist.euclidean)  # 將歐氏距離output
sink() # 關閉轉向
#############################################
# 繪出歐氏多階群組分析                      #
#############################################
cluster.e <- hclust( # 以全階方式進行群組分析
  dist.euclidean,
  method = "complete"
)
plot(  # 繪出以歐氏距離分析的多階群組圖
  x=cluster.e,
  xlab='Cluster',
  ylab='Euclidean distance',
  main='Hierarchical cluster Analysis',
)
################本例結束#################################


#################################################
# 以馬氏距離(Mohanlanobis distance)分析(未分群) #
#################################################
dist.manhattan <- dist( # 計算交互之馬氏距離
  x=enroll.std,
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
  x=enroll,
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
