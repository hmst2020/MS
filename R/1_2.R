library(dplyr)
freq<-c(8,38,61,54,42,27,10)   # 發生頻次
defs<-0:(length(freq)-1)       # 不良焊點數

avg<-sum(freq*defs)/sum(freq)   # 計算平均數
exp_pdf<-dpois(defs,avg)  # 期望發生機率
exp_pdf<-exp_pdf/sum(exp_pdf)           # 使期望發生機率總和等於1

chi<-chisq.test(               # 卡方檢定
  x=freq,                      # 發生頻次
  p=exp_pdf)                   # 期望發生機率
print(chi)              # 列印檢定結果

qchisq(.95, df=6)       # 卡方臨界值

str(chi)                # 一窺物件屬性

sum((chi$observed- chi$expected)^2/     # 驗證卡方值
      chi$expected)==chi$statistic
 


###############end of 1_2.R########################