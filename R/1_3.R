X<- matrix(    # 以矩陣物件表達列聯表資料
  c(17,45,32,60,80,12,12,20,15),
  ncol=3) 
dimnames(X)<-list(     # 列與行命名
  c('滿意','還可以','很差'),
  c('無香菜拌調味','少量香菜拌調味','多量香菜拌調味'))
cbind(   # 列印列聯表資料內容含列、行合計
  rbind(X,'Total'=colSums(X)), # 行合計
  'Total'=rowSums(rbind(X,'Total'=colSums(X))))  # 列合計

chitbl<-chisq.test(  # 卡方檢定
  x=X)   # 矩陣物件資料
print(chitbl)        # 列印檢測結果

qchisq(    # 計算臨界值(critical value)
  p=.95,   # 信心水準
  df=chitbl$parameter)  # 自由度
####### end of 1_3.R#############