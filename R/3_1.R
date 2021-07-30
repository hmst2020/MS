############觀察值& 距離(歐氏)物件#################
ob<- data.frame(    # 將觀察值建構data frame 物件
  V1=c(3,4,4,2,6,7,6),   # 第一行行名與向量值
  V2=c(2,5,7,7,6,7,4),   # 第二行行名與向量值
  row.names=LETTERS[1:7])  # 列名為字母大寫 A ~ G 共7個
print(t(ob))  # 列印觀察值(轉置矩陣)
########## 繪製散佈圖##########
plot(            # 繪製資料散佈圖
  x=ob$V1,       # x軸表示集群變數V1
  y=ob$V2,       # y軸表示集群變數V2
  type='n',      # 點狀圖
  ylab='V2',     # y軸文字標籤
  xlab='V1',     # x軸文字標籤
  main='觀測值散佈圖',    # 標題文字
  ylim=c(0,10),        # y軸尺標範圍
  xlim=c(0,10))         # x軸尺標範圍
text( # 於繪圖物件標示學生代號
  x=ob$V1,   # 同上
  y=ob$V2,    # 同上
  labels=rownames(ob))   # 學生代號
#################
distance <- dist( # 計算交互之歐氏距離並產生距離物件
  x=ob,           # 觀察值物件
  method = "euclidean",    # 計算歐氏距離  
  diag = TRUE   # 對角線是否含值
)
distance<-round(distance,digits=3)  # 距離精度小數點三位
print(distance)    # 列印距離物件
############宣告共用函式######################
d<-as.matrix(distance)  # 距離物件轉成距離矩陣
dist.func<- function(x,y,method){ # 傳入x,y cluster 元素重新計算距離
  if (method %in% c('average')){       # 使用平均法
    return (sum(d[x,y])/length(d[x,y]))
  }else if(method %in% c('single')){   # 使用單一法
    return (min(d[x,y]))
  }else if(method %in% c('complete')){ # 使用完全法
    return (max(d[x,y]))
  }
}
avg.sim<- function(mtx){  # 計算平均群組內距離，mtx: 群組後的matrix
  sim<- 0  # 分群之組內歐氏距離加總
  num<- 0  # 平均的母數
  for (clust in rownames(mtx)){
    x<- unlist(strsplit(clust,'-')) # 解構gpn群組名稱為vector
    if (length(x)>=2){  # 單一成員無群組內距離，不列入計算
      combn.m<-combn(x,2)  # x元素的兩兩歐氏距離的組合矩陣
      for (i in 1:ncol(combn.m)){  # 將組內歐氏距離加總
        sim<- sim+d[combn.m[1,i],combn.m[2,i]]
      }
      num<-num+choose(length(x),2)
    }
  }
  return (ifelse(num==0,0,sim/num))
}
##########變數初始值宣告##################
dis<- distance # 距離矩陣物件，於下列迴圈內使用
m<- as.matrix(distance) # 矩陣物件指定予另一變數，於下列迴圈內使用
print(m)  # 列印初始距離矩陣
#######迴圈處理距離矩陣所有元素(element)直到完成最後一個####
steps<- data.frame()  # 每step結果存放之變數
step.num<-0           # 每step編號
steps<- rbind(steps,data.frame(  # 加入完成的step
  步驟='初始解決方案',最小距離=NA,成對觀測值=NA,
  集群成員=paste0(sprintf('(%s)',rownames(m)),collapse=''),
  集群數=nrow(m),整體相似性=0
  ))
while (length(dis)>=1) {  # 處理距離矩陣至最後一個
  step.num<- step.num+1  # step 編號序碼
  ##### 步驟1 選出矩陣內最近距離者組成一新的 cluster #####
  dis.min<-min(dis) # 選出dis物件裡物件歐式距離最小值
  rowcol<-which(
    m==dis.min,
    arr.ind=TRUE)[1,] # 傳回符合最小值位置的列、行位置
  rname<-rownames(m)[rowcol['row']] # 傳回列、行位置對應的列名
  cname<-colnames(m)[rowcol['col']] # 傳回列、行位置對應的行名
  gpn<- paste0(c(cname,rname),collapse='-') # 新cluster名稱
  ##### 步驟2 計算新cluster與其它本身以外之距離(本例示範平均法)###
  x<- unlist(strsplit(gpn,'-')) # 解構gpn群組名稱為vector
  newRow<- c() # 宣告變數(新cluster 的新距離vector)
  for ( c in 1:ncol(m)){
    if (c %in% rowcol['col'] ||
        c %in% rowcol['row']){
      newRow[c]<- 0
      next      # 新cluster的元素除外
    }
    y<- unlist( # 解構rownames(m)[c]群組名稱為vector elements
      strsplit(rownames(m)[c],'-')) 
    avg.d<-dist.func( # 呼叫dist.func依平均法重新計算歐式距離
      x=x,
      y=y,
      method='average')  # 本例指定平均法
    newRow[c]<- round(avg.d,digits=4)  # 取小數點以下4位
  }
  ##### 步驟3 依計算的新距離產生新距離矩陣###
  m<-cbind(m,newRow) # 合併重新計算之歐式距離
  newRow[length(newRow)+1]<- 0
  m<-rbind(m,newRow)
  if (nrow(m)>3){
    m<- m[ # 從m矩陣同時去除已入cluster的行與列
      -c(rowcol['row'],rowcol['col']), 
      -c(rowcol['col'],rowcol['row'])
      ]
    rownames(m)[nrow(m)]<-gpn # 正式命名新增的rowname
    colnames(m)[ncol(m)]<-gpn # 正式命名新增的colwname
  }else{
    m<- matrix(0,nrow=1,dimnames=list(gpn,gpn))
  }
  dis<- as.dist(m,diag=TRUE) # 將新的陣列轉為dist 物件
  print(dis)  # 列印新的距離矩陣
  writeLines('\n')
  dis.w<-avg.sim(m) # 計算此step為止的平均群組內距離
  steps<- rbind(steps,data.frame(  # 加入完成的step
    步驟=step.num,最小距離=dis.min,成對觀測值=gpn,
    集群成員=paste0(sprintf('(%s)',rownames(m)),collapse=''),
    集群數=nrow(m),整體相似性=dis.w
    ))
  print(steps[nrow(steps),])  # 列印每一step重新計算的歐式距離
  writeLines('\n');writeLines('\n')
}
print(steps,row.names = FALSE) # 列印最後每step之結果
###############集群數的整體相似性度量圖################
plot(
  x=steps$集群數,
  y=steps$整體相似性,
  type='l',
  xlab='集群數',
  ylab='整體相似性',
  main='集群數的整體相似性度量')
abline(v=3,col='blue',lty=2)
###########################
library(factoextra)
optimal.clust<-fviz_nbclust(
  x=ob,
  method='wss',
  FUNcluster=hcut,
  k.max=6,  # 受測者數
  nstart = 5)
print(optimal.clust)  # 列印肘部法折線圖
k<- 3  # 選擇最佳集群數量
###############繪出分群多階圖################
cluster.e <- hclust( # 以平均方式進行群組分析
  distance,
  method = "average")  # 使用平均法
plot(  # 繪出以歐氏距離分析的多階群組圖
  x=cluster.e,
  xlab='Cluster',
  ylab='Euclidean distance',
  main='Hierarchical cluster Analysis')
cutree(cluster.e,k=3)   # 依k=3組列印分組結果
######繪出顏色分群多階圖########
library(dendextend)
den <- as.dendrogram(cluster.e)   # 轉成樹狀物件
den_color <- color_branches(den,h = 3)   # 分成3組顏色
plot(den_color)     # 繪出樹狀圖
################end of 3_1本例結束#################################