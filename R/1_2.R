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
##################### end of 4_3 #########
mm<-pracma::charpoly(A, info = TRUE)  # 計算特徵多項式係數
p<-polynom::polynomial(rev(mm$cp))     # 依多項式係數產生特徵多項式物件
library(polynom)
eigv<-rev(solve(p))  # 依特徵多項式物件解特徵值及排序
library(matlib)
library(matrixcalc)
for (i in 1:length(eigv)){
  km<-eigv[i]*diag(nrow(A))-A # λI − A 的 matrix
  is.singular.matrix(km, tol = 1e-08)  # 測試km是否為奇異矩陣(determinant=0)
  qr(A)$rank  # 測試xx矩陣的Rank value(列秩)，線性獨立或線性相依
  #library(pracma)
  #p <- pracma::Poly(eigv)
  #c<-polyval(p, eigv)
  c<-rep(0,nrow(A)) # constants of equation 
  showEqn(km, c)  # 列印出eigv[x]特徵向量
  Solve( 
    km,   # 若為奇異矩陣則有無限多組解
    c,    # c為零向量
    fractions = TRUE)
}
##########################

round(solve(ex$vectors)%*%A%*%ex$vectors,6)    # 特徵值對角矩陣
Y<- ex$vectors%*%diag(ex$values)  # 主成分矩陣

Z<-A%*%ex$vectors
cov(Z)
Y<-eig$vectors%*%diag(eig$values)
cov(Y)