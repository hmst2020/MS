ob<- readRDS(file = "data/fishNut.rds")   # 本例資料載入
library(dplyr)
ob.sum<- ob %>%  # 增加Sum 欄位
  mutate(
    Sum=rowSums(.[1:3]))
print(ob.sum)  # 列印觀察值內容
#############方法一 以 R 套件base解題###################
############ 步驟一.計算起始分組與結果###################
k<-3    # 設欲分為3個群組
min.s<- min(ob.sum$Sum) # 營養素總和最小者
max.s<- max(ob.sum$Sum) # 營養素總和最大者
ob.sum<- cbind(  # 以初始分群依據將之分為k群
  ob.sum,
  cluster=unlist(lapply(
    data.frame(t(ob.sum$Sum)),
    function(x){
      y<-floor(k*(x-min.s)/(max.s-min.s)+1)  #公式(3.4.1)
      ifelse(y>k,k,y)
    })
  )
)
clust.l<- vector('list',k) # 宣告一list物件做分群紀錄
for (i in 1:nrow(ob.sum)){ # 計算初始各群組的成員
  x<-ob.sum[i,]
  clust.l[[x$cluster]]<-append(
    clust.l[[x$cluster]],rownames(ob.sum)[i])
}
print(ob.sum)  # 列印初始分群
print(clust.l) # 列印初始分群
############本例函式宣告###########################
clust.m<- function(grp.l){  # 計算群之的各觀察維度中心值
  result<- data.frame()
  for (e in grp.l){
    mean<-colMeans(ob[e,])
    result<- rbind(result,t(mean))
    rownames(result)[nrow(result)]<-
      paste0(sprintf('(%s)',e),collapse='')
  }
  return (result)
}
####步驟二. 計算各群組之的各觀察維度中心值########
clust.means<-clust.m(clust.l)
print(clust.means)  # 列印初始分群之各觀察維度平均值
####步驟三. 計算初始分群內部距離平方總和########
tot.withinss<-sum((ob - clust.means[ob.sum$cluster,])^2)
print(tot.withinss)  # 列印群組內部距離平方總和
####步驟四. 找出是否有object必要變更歸屬之群組，#########
############以助於降低分群內部距離平方總和，#######
############直至在無需變更為止########################
change<- function(el,fid,tid,grp.l){ # 向量元素el從群fid 轉至 tid
  grp.l[[fid]]<-grp.l[[fid]][!grp.l[[fid]] %in% c(el)]
  grp.l[[tid]]<- append(grp.l[[tid]],el)
  ob.sum[which(rownames(ob.sum)==el),]$cluster<-tid
  return (list(clust.l=grp.l,ob.sum=ob.sum))
}
repeat{
  el<-NULL
  for (i in 1:nrow(ob.sum)){
    r<-ob.sum[i,]
    cid<-r$cluster
    n.li<-nrow(ob.sum[  # 計算原群組的成員數
      which(ob.sum$cluster==cid),]
    )
    decrease<-ifelse( # 計算移轉至新的群組可能的WCSS的遞減(正貢獻)
      (n.li-1)==0,   # 若原群組只剩本身一個成員
      sum((ob[i,]-clust.means[cid,])^2),
      n.li/(n.li-1)*sum( # 計算均值差之平方和之影響值
        (ob[i,]-clust.means[cid,])^2)
      ) 
    for (k in (1:nrow(clust.means))[-cid]){
      n.l<-nrow(ob.sum[   # 計算新群組的成員數
        which(ob.sum$cluster==k),]
      ) 
      increase<- n.l/(n.l+1)*sum(  # 計算均值差之平方和之影響值
        (ob[i,]-clust.means[k,])^2
      ) 
      var<- increase-decrease   # 即判斷公式(3.4.2)裡的R 值
      if (var<0){    # 若轉移群組有利
        el<-rownames(ob.sum[i,])
        l<-change(el,cid,k,clust.l)
        clust.l<- l$clust.l
        ob.sum<- l$ob.sum
        clust.means<-clust.m(clust.l)
        break
      }
    }
    if (length(el)!=0){
      break
    }
  }
  if (length(el)==0){
    break
  }
}
####步驟五. 列出最後分群之結果#########
tot.withinss<-sum(  # 群組內部距離平方總和
  (ob - clust.means[ob.sum$cluster,])^2
)
mean<-colMeans(ob)  # 觀察值依各欄(營養素)計算平均
totss<-sum(     # 各觀察值至各營養素平均值的距離平方總和
  mapply('-', ob, mean)^2
)  
betweenss<- totss-tot.withinss  # 計算群組間距離平方總和
summaries<-list(  #分群指標值彙總
  totss=totss,
  tot.withinss=tot.withinss,
  betweenss=betweenss
)
print(ob.sum[order(ob.sum$cluster),]) # 列印各object歸屬之群組
print(clust.means) # 列印各群組平均值
print(summaries)  # 列印分群彙總指標

##############方法二 以R 套件stats解題######################
km<-kmeans(   # 產生kmeans物件
  x=ob,       # 觀察值物件
  centers=3,  # 指定分群數
  iter.max = 10,   # 限制迴圈次數免於落入無窮迴圈
  nstart = 25) # 依分群數隨機取object歸入之起始套數，此值可讓分群加速穩定
cluster.center<- round(as.data.frame(t(km$centers)),digits=3)
colnames(cluster.center)<- paste0(paste0('group',1:2),'(n=',km$size,')')
print(cluster.center)  # 列印各群組平均值(中心值)
newob<-cbind(ob,Cluster=km[["cluster"]])
print(newob[order(newob$Cluster),])  # 列印各object歸屬之群組
print(km$totss) # 列印合計的sum of square
print(km$tot.withinss)  # 列印群組內部距離平方總和
print(km$betweenss)     # 列印群組間距離平方總和
############# end of 3_4################