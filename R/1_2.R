A<-matrix(      # 共變異數矩陣
  c(1,-2,0,
    -2,5,0,
    0,0,2),
  nrow=3
)
ex<-eigen(A)   # 特徵分解
eig.df<-data.frame(           # 計算特徵值佔比
  eig=ex$values,
  prop=ex$values/sum(ex$values)) 
eig.df<-transform(eig.df,cum=cumsum(eig.df$prop))   # 增加累計佔比欄
print(eig.df)  # 列印特徵值占比累計表
print(ex$vectors)    # 特徵向量
round(t(ex$vectors)%*%ex$vectors,8)  # 驗證正交
colSums(ex$vectors^2)    # 驗證模長

corA<-cov2cor(A)         # 將共變異數矩陣標準化
eigA<-eigen(corA)        # 特徵分解
eigA.df<-data.frame(           # 計算特徵值佔比
  eig=eigA$values,
  prop=eigA$values/sum(eigA$values)) 
eigA.df<-transform(eigA.df,cum=cumsum(eigA.df$prop)) 
print(eigA.df)  # 列印特徵值占比累計表
##################### end of 1_2.R #########