library(HistData)
N<-Nightingale[  # 只取繪圖需要欄位
  c('Date','Year',
    'Disease.rate','Wounds.rate','Other.rate')]
library(tidyr)
N.pivot<-pivot_longer(  # 樞紐轉換
  data=N,  # 資料集(data frame)
  cols=c('Disease.rate','Wounds.rate','Other.rate'), # 導出欄位依據
  names_to="cause",  # 導出欄位名稱
  values_to="death_rate") # 導出值
library(ggplot2)
g <- ggplot(  # 產生繪圖物件
  data= N.pivot,  # 繪圖資料
  aes(     # 外觀設定
    x= factor(Date),   # x 軸為日期
    y= death_rate,    # y 軸為死亡率
    fill = cause)     # 填色依據欄位
)+
ggtitle(# 標題文字
  'Death Rate in the Crimea(Apr,1854 ~ Mar,1856)'
)+ 
xlab('Date')+  # x 軸標籤文字
ylab('Death Rate')+ # y 軸標籤文字
theme(   # 繪圖樣式主題設定
  axis.title.x= element_text(  #x 軸標題
    color = "#111111",   # 文字顏色
    size = 10,           # 文字大小
    face = "bold"        # 文字粗細
  ),
  axis.text.x= element_text( # x 軸上刻度文字
    angle = 90 # 逆時鐘旋轉角度
  )
)
g<-g+
  geom_bar(  # 疊加條狀圖層
    width = 1,  # 條狀寬度
    stat = "identity",  # 條狀高度依y(death_rate)值
    position = "stack", # 依x軸將y軸值相加堆疊條狀高度
    colour = "black"    # 框線顏色
  )   
g<-g+
  geom_text( # 疊加文字標示於直方圖塊上
    data=N.pivot, # 文字資料來源
    mapping = aes(
      x= factor(Date),   # x 軸對應於日期
      y= death_rate,     # 
      label=death_rate     # 標示的文字
    ),
    check_overlap = TRUE,  # 避開文字重疊
    angle = 45,    # 文字反時鐘旋轉角度
    color= 'blue', # 文字的顏色
    size =3,       # 文字的大小(mm)
    vjust = 0.5     # 文字位置垂直向上調整幾個字高
  )
ggsave(  # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/bar_N.svg",  #存檔目錄需存在，否則會有錯誤拋出
  g,           # ggplot 繪圖物件
  scale = 1    # 繪圖板尺規範圍擴增倍數
) 

g<-g+scale_y_sqrt()  # 調整尺規
ggsave(
  file="E:/temp/bar_Nsqrt.svg",  # 同上
  g,                             # 同上
  scale = 1    # 同上
)

plot(scales::sqrt_trans(),xlim= c(0,4)) # 平方根轉換圖

g<-g+
  coord_polar( # 將條狀圖座標置換成極地圈緯線圖
    start = pi,  # x(factor(Date))軸原點處
    clip = "on"  # 繪圖比例受限於繪圖板大小
  )
ggsave(
  file="E:/temp/rose_Nsqrt.svg",  #存檔目錄需存在，否則會有錯誤拋出
  g,
  scale = 2    # 繪圖板尺規擴增倍數
)
#############end of 2.1################################

N.year<-aggregate(x=list(N$Disease.rate,N$Wounds.rate,N$Other.rate),by=list(N$Year),FUN=mean)
colnames(N.year)<-c('Year','Disease.rate','Wounds.rate','Other.rate')
N.pivot<-pivot_longer(  # 樞紐轉換
  data=N.year,  # 資料集(data frame)
  cols=c('Disease.rate','Wounds.rate','Other.rate'), # 導出欄位依據
  names_to="cause",  # 導出欄位名稱
  values_to="death_rate") # 導出值
g <- ggplot(  # 產生繪圖物件
  data = N.pivot,  # 繪圖資料
  aes(x =factor(Year),   # 外觀設定
      y = death_rate, 
      fill = cause)
)+
  ggtitle('Death Rate in the Crimea(1854 ~ 1856)')+
  xlab('Year')+
  ylab('Death Rate')+
  theme(   # 繪圖樣式主題設定
    axis.title.x= element_text(  #x 軸標題
      color = "#111111", 
      size = 10, 
      face = "bold"
    ),
    axis.text.x= element_text( #x 軸文字
      angle = 90 # 逆時鐘旋轉角度
    )
  )
g<-g+
  geom_bar(
    width = 1, 
    stat = "identity",
    position = "stack", 
    colour = "black")
g<-g+
  scale_y_sqrt()
g<-g+
  coord_polar(start = pi,clip = "on")
print(g)
ggsave(
  file="E:/temp/rose_NYear.svg",  #存檔目錄需存在，否則會有錯誤拋出
  g,
  scale = 1.5
) 
dev.off()
#############################################

gather(N,key = "Cause", value = "Deaths", -Date)
dplyr::mutate(N.pivot,Month = gl(12, 1, 72, labels = month.abb[c(4:12, 1:3)]))
D<-format(Nightingale$Date, "%b %Y")
rose.diag(
  x=Nightingale[c('Date','Month','Year','Disease.rate','Wounds.rate','Other.rate')],
  xlab = "X", ylab = "Y",
  names=Nightingale$Date
)




x <- circular(runif(50, 0, 2*pi))
rose.diag(x, bins = 18, main = 'Uniform Data')
points(x)

# Generate von Mises data and create several rose diagrams.
x <- rvonmises(n=50, mu=circular(0), kappa=5, control.circular=list(zero=pi/4))
y <- rose.diag(x, bins=18) # Points fall out of bounds.
points(x, plot.info=y, stack=TRUE)
y <- rose.diag(x, bins=18, prop=1.5, shrink=1.5) # Adjust optional parameters to fit
######## all points on plot.
points(x, plot.info=y, stack=TRUE)

# Add the rose diag to a plot
plot(x)
rose.diag(x, bins=12, add=TRUE, col=2)