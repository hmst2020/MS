############觀察值#################
ob<- data.frame(    # 將觀察值建構data frame 物件
  x1=c(6,-4,2,-4),  # 第一行行名與向量值
  x2=c(4,2,-1,-3),  # 第二行行名與向量值
  row.names=1:4     # 列名為1~4 代表分群對象的4個object
)
gp.mean<- function(clust.l){  # 計算群之的各觀察維度中心值
  result<- data.frame()
  for (e in clust.l){
    mean<-colMeans(ob[e,])
    result<- rbind(result,t(mean))
    rownames(result)[nrow(result)]<-
      paste0(sprintf('(%s)',e),collapse='')
  }
  return (result)
}

means<-gp.mean(list(c(1,2),c(3,4)))  # 計算初始分群的data frame
print(means)     # 初始分群(1,2)和(3,4) 的平均數
sum((ob - means[c(1,1,2,2),])^2)  # 計算成員各屬性與其各平均值距離平方和
means<-gp.mean(list(c(1,3),c(2,4)))  # 計算初始分群的data frame
print(means)     # 初始分群(1,3)和(2,4) 的平均數
sum((ob - means[c(1,2,1,2),])^2)  # 計算成員各屬性與其各平均值距離平方和


