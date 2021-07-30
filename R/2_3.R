TD<- readRDS(file = "data/Tdistress.rds")   # 本例資料載入
n<- 6   # O-ring 總數量
t<- c(30,90)  # 可能的氣溫範圍
plot(          # 繪製資料散佈圖
  x=TD$temperature,   # x軸自變數資料
  y=TD$damage,        # y軸應變資料
  type='p',           # 點狀圖
  ylab='意外發生數',  # y軸文字標籤
  xlab='氣溫',        # x軸文字標籤
  main='熱損壞數',    # 標題文字
  ylim=c(0,n),        # y軸尺標範圍
  xlim=t              # x軸尺標範圍
)
###########二項式羅吉斯迴歸######################
logit.f<-glm(          # 使用廣義塑模函式來建模
  formula=damage/n ~ temperature,    # 自變數與因變數公式
  data=TD,                           # 資料物件
  family=binomial(link='logit'))      # Logit模型的二項式迴歸
print(logit.f$coefficients)  # 列印迴歸模型係數
###########準二項式羅吉斯迴歸######################
logit.f<-glm(          # 使用廣義塑模函式來建模
  formula=damage/n ~ temperature,    # 自變數與因變數公式
  data=TD,                           # 資料物件
  family=quasibinomial(link='logit')) # Logit模型的二項式迴歸
print(logit.f$coefficients) # 列印迴歸模型係數
beta0<-logit.f$coefficients[1]   # 常數項
beta1<-logit.f$coefficients[2]   # 變數項係數
###################
pt <- function(b0,b1,t){     # 定義羅吉斯函數
  return (1/(1+exp(-(b0+b1*t))))
}
#par(new=TRUE)  # 設定圖形參數
plot(      # 繪出30~90度模型圖
  x= seq(t[1],t[2],5), #TD$temperature,
  y=n*pt(beta0,beta1,seq(t[1],t[2],5)),
  type='l',
  ylab='意外發生數',  # y軸文字標籤
  xlab='氣溫',        # x軸文字標籤
  ylim=c(0,n),
  xlim=t
)
text( # 將plot繪出的圖疊加文字
  x=50, # 文字對應x軸位置 
  y=n*pt(beta0,beta1,45),
  paste0('','6P(t)'),
  adj=c(-0.4,0)
)
arrows(
  x0=50,
  y0=n*pt(beta0,beta1,45),
  x1=45,
  y1=n*pt(beta0,beta1,45),
  angle=20,
  col = 'blue',
  lwd=2
)
#########方法二############################
LL.f <- function(B,DT) {    # 自訂log概似值計算函式
  X<- matrix(               # 羅吉斯函數的自變數矩陣
    c(rep(1,length(DT$temperature)),
      DT$temperature),
    nrow=length(DT$temperature),
    byrow=FALSE)
  p<-1/(1+1/exp(X%*%B))     # 勝算機率
  sum(                      # 回傳計算結果(方程式2.12)
    (log(choose(n,DT$damage))+
        DT$damage*log(p)+(n-DT$damage)*log(1-p)
      )
  )
}
OPT<-optim(                 #  優化(optimization)函式 
  par=as.matrix(c(0,0)),    #  係數初始值矩陣
  fn=LL.f,                  #  自訂函式值計算
  control=list(fnscale=-1), #  求最大估計值
  DT=list(damage=TD$damage,temperature =TD$temperature) # 實際發生之資料
)
print(OPT$par)   # 列印係數結果
###############end of 2.3############################