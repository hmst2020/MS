##############covid-19 data from WHO official web site###########
#### https://covid19.who.int ######
colors<-c('red','yellow','green','white')  # 嚴重度顏色區分
sev.label<-c('High','Medium','Low','Normal') # 嚴重度圖示文字
covid_path='data/WHO-COVID-19-global-data.csv'  # 疫情資料檔路徑
covid<-read.csv(covid_path)  # 將csv格式資料讀入變數
library(data.table)
covid<-na.omit(covid)      # 去除含有遺漏之資料列
x.covid<-dplyr::filter(    # 過濾無法對應世界地圖及無標示資料日期之無效資料
  covid,covid$Country !='Other' & nchar(covid$Date_reported)!=1
)
setDT(covid) # 將data frame物件轉換成data.table 物件
recent_data<-covid[ # 只取各國資料中回報日期之最後一筆
  ,.SD[
    which.max(as.Date(Date_reported)) # 回報日期最大(後)者
    ],
  keyby=Country_code    # 依國別碼
  ]
recent_data<- transform(  # 於recent_data增加一death_rate欄位值
  recent_data,
  death_rate=recent_data$Cumulative_deaths/sum(recent_data$Cumulative_deaths)
)
recent_data<-recent_data[  # 使用排序函式依death_rate降冪重新排序
  order(
    death_rate,
    decreasing=TRUE),
  ]
recent_data<-  transform(  #於recent_data再增加一cum_rate欄位值
  recent_data,
  cum_rate=cumsum(  # 使用累進函式依列累加death_rate
    recent_data$death_rate
  )
)
recent_data$severity<-'4' # 預設severity值為4(嚴重度最小，對應於colors變數位置指標)
recent_data[which(  # 更新累計死亡人數佔比前70%之severity值為1
  recent_data$cum_rate<=0.7),
  ]$severity<-'1' 
recent_data[which(  # 更新累計死亡人數佔比前70%~95%之severity值為2
  recent_data$cum_rate<=0.95 & recent_data$cum_rate>0.7),
  ]$severity<-'2'
recent_data[which(  # 更新累計死亡人數佔比95%之後之severity值為3
  recent_data$cum_rate>0.95),
  ]$severity<-'3'
options(width=180) # 將console output 寬度調整容納長資料寬度
sink(  # 將console output 轉向至文字檔
  file="E:/temp/recent_data.txt",
  type="output"
)
head(recent_data) # 列印前六筆
writeLines("\n")  # 輸出跳行符號
tail(recent_data) # 列印倒數六筆
sink() # 關閉console轉向

#### https://geojson-maps.ash.ms/ ######
library(geojsonio)  # 載入處理GeoJSON資料套件
path<- 'data/world.geo.json'
world.sp <- geojson_read( # 將下載之縣市GeoJSON圖資讀入變數
  x= path,    # GeoJSON資料來源(檔案路徑)
  what = "sp" # 指定回傳Spatial class 之物件
)
world.data<-world.sp@data[  # 取出本例所需相關欄位(國別代碼、國名、洲名等)
  c('name_long','pop_est','continent','geounit','iso_a2')
  ]
options(width=180) # 將console output 寬度調整容納長資料寬度
sink(  # 將console output 轉向至文字檔
  file="E:/temp/world_data.txt",
  type="output"
)
head(world.data)   # 列印前六筆
writeLines("\n")   # 輸出跳行符號
tail(world.data)   # 列印倒數六筆
sink()  # 關閉轉向

library(broom) # 載入轉換tibble(data.frame 的擴充物件)資料套件
world_map <- tidy( # 將sp物件轉換為data.frame物件
  world.sp,  # 資料對象
  region = "geounit" # 群(group)欄位的依據
)
##### 參照world.data於world_map增加name_long欄位######
world_map$name_long<-world.data$name_long[ 
  match(world_map$id,world.data$geounit) # world_map及world.data的連結(join)欄位
  ]
world_map$severity<-'white'  # world_map增加一欄位severity並設其預設值
library(sqldf) 
world_map<-   # 使用SQL指令更新world_map的severity欄位值(注意其join欄位)
  sqldf(c("UPDATE world_map  
             SET severity = (SELECT recent_data.severity
                          FROM recent_data
                          WHERE world_map.id = recent_data.country
                           )
             WHERE EXISTS (SELECT 1
                           FROM recent_data
                           WHERE world_map.id = recent_data.country
                           )",
          "select * from world_map")
)
world_map<-   # 使用SQL指令更新world_map的severity欄位值(注意其join欄位)
  sqldf(c("UPDATE world_map
             SET severity = (SELECT recent_data.severity
                          FROM recent_data
                          WHERE world_map.name_long = recent_data.country
                           )
             WHERE EXISTS (SELECT 1
                           FROM recent_data
                           WHERE world_map.name_long = recent_data.country
                           )",
          "select * from world_map")
  )
options(width=180) # 將console output 寬度調整容納長資料寬度
sink(  # 將console output 轉向至文字檔
  file="E:/temp/world_map.txt",
  type="output"
)
head(world_map)   # 列印前六筆
writeLines("\n")   # 輸出跳行符號
tail(world_map)   # 列印倒數六筆
sink()  # 關閉轉向


library(ggplot2) # 載入繪圖套件
g <- ggplot(      # 使用繪圖函式產生繪圖物件
  data=world_map  # 繪圖資料
  )+  
  geom_polygon( # 繪出多邊形資料
    mapping=aes( # 外觀設定
      x = long,  # x軸的經度欄位
      y = lat,   # y軸的緯度欄位
      group = group, # 封閉區塊(polygon)同群欄位
      fill=severity), # 依據severity欄位值對應colors變數內顏色填入
    colour="black", # 國界線條顏色
    show.legend = TRUE # 將圖例繪出
  )+
  labs(title='世界COVID-19疫情地圖',  # 設定圖表名稱及xy各軸標籤
       x ="經度", y = "緯度")+
  scale_fill_manual( # 圖例指定內容
    name='Severity', # 圖例標題名
    values =colors,  # 圖例及填入顏色依據colors這vector
    labels=sev.label  # 圖例值依據sev.label這vector的位置對照
  )
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/covid-19_map.svg",  #存檔目錄需存在，否則會有錯誤拋出
  g,           # ggplot 繪圖物件
  scale = 1.5  # 繪圖板尺規範圍擴增倍數
) 

covid.sum<-aggregate(  # 依日期及WHO地區別彙總新增死亡病例
  x=list(New_deaths=covid$New_deaths), # 彙總欄位，及彙總後欄位名稱
  by=list(    # 彙總依據，及彙總後欄位名稱
    Date_reported=as.Date(covid$Date_reported),
    WHO_region=covid$WHO_region),
  FUN=sum  # 彙總使用加總(sum)函式
)
options(width=180) # 將console output 寬度調整容納長資料寬度
sink(  # 將console output 轉向至文字檔
  file="E:/temp/covid_sum.txt",
  type="output"
)
head(covid.sum)   # 列印前六筆
writeLines("\n")   # 輸出跳行符號
tail(covid.sum)   # 列印倒數六筆
sink()  # 關閉轉向

g<-ggplot(      # 使用繪圖函式產生繪圖物件
  data=covid.sum,    # 繪圖資料
  aes(     # 外觀設定
    x= as.Date(factor(Date_reported)),   # x 軸為日期
    y= New_deaths,    # y 軸為新增死亡人數
    fill = WHO_region)     # 填色依據欄位
)+
ggtitle('COVID-19 Daily Deaths')+ # 圖標題
xlab('Date')+                     # x軸標題
ylab('New deaths')+               # y軸標題
theme(   # 繪圖樣式主題設定
  axis.title.x= element_text(  # x 軸標題文字
    color = "#111111",         # 文字顏色
    size = 10,                 # 文字大小
    face = "bold"              # 文字粗細
  ),
  axis.text.x= element_text( #x 軸文字
    angle = 45 # 逆時鐘旋轉角度
  )
)+
scale_x_date(  # 設定日期格式之x軸尺規
  breaks=scales::date_breaks(width="1 month"), # 按月分隔
  labels=scales::date_format("%Y-%m"))+        # 尺規標示文字
geom_bar(  # 疊加條狀圖層
  stat = "identity",  # 條狀高度依y(New_deaths)值
  position = "stack" # 依x軸將y軸值相加堆疊條狀高度
)   
ggsave(   # 繪出至檔案(存檔目錄需存在，否則會有錯誤拋出)
  file="E:/temp/covid-19_sum.svg",  #存檔目錄需存在，否則會有錯誤拋出
  g,           # ggplot 繪圖物件
  scale = 1.5  # 繪圖板尺規範圍擴增倍數
) 
######################end of 2.4##############################################



covid_path='data/WHO-COVID-19-global-data.csv'
covid<-read.csv(covid_path)
library(data.table)
covid<-na.omit(covid)
covid<-dplyr::filter(covid,covid$Country !='Other' & nchar(covid$Date_reported)!=1)
setDT(covid)
recent_data<-covid[,.SD[which.max(as.Date(Date_reported))],keyby=Country_code]
g <- ggplot(  # 產生繪圖物件
  data = recent_data,  # 繪圖資料
  aes(x =Country,   # 外觀設定
      y = as.integer(Cumulative_cases))
)+
scale_y_continuous(
  labels = function(x) format(x, scientific = FALSE)
)+
ggtitle('Cumulative Caes by Country')+
xlab('Country')+
ylab('Cases')+
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
print(g)


merged<-merge(x=world.data,y=recent_data,by.x='iso_a2',by.y='Country_code')
merged<-transform(
  merged,
  death_rate=merged$Cumulative_deaths/merged$pop_est
)
n<-apply(merged,2,function(x){print(x)})