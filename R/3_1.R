############觀察值& 距離(歐氏)物件#################
ob<- data.frame(    # 將觀察值建構data frame 物件
  V1=c(3,4,4,2,6,7,6),   # 第一行行名與向量值
  V2=c(2,5,7,7,6,7,4),   # 第二行行名與向量值
  row.names=LETTERS[1:7]  # 列名為字母大寫 A ~ G 共7個
)
case.num<-nrow(ob)   # 觀察值物件之列數
distance <- dist( # 計算交互之歐氏距離並產生距離物件
  x=ob,
  method = "euclidean",  
  diag = TRUE   # 對角線是否含值
)
distance<-round(distance,digits=3)  # 距離精度小數點三位
d<- as.matrix(distance)  # 將距離物件轉成矩陣物件
############宣告共用函式######################
rc<-function(idx,dim) {  # 傳入dist物件指標值idx及矩陣列數dim傳回 row、col
  a<- -1/2;b<- (2*(dim-1)+1)/2;c<-0  # 宣告二次函式各系數
  col<-ceiling(  # 捨小數進位取整數
    -sqrt((idx+(b^2-4*a*c)/(4*a))/a)-(b/(2*a)) # 二次函式已知idx求x(即col)
  )
  row=idx+dim+col*(1+col)/2-col*dim # 已知idx及col求row
  c(row=row,col=col) # 傳回具名vector
}
dist.func<- function(x,y,method){ # 傳入x,y cluster重新計算距離
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
m<- d # 矩陣物件指定予另一變數，於下列迴圈內使用
dis<- distance # 距離矩陣物件，於下列迴圈內使用
print(dis)  # 列印初始距離矩陣
#######迴圈處理距離矩陣所有元素(element)直到完成最後一個####
steps<- data.frame()  # 每step結果存放之變數
step.num<-0           # 每step編號
while (length(dis)>=1) {  # 處理距離矩陣至最後一個
  step.num<- step.num+1  # step 編號序碼
  ##### 步驟1 選出矩陣內最近距離者組成一新的 cluster #####
  dis.min<-min(dis) # 選出dis物件裡物件歐式距離最小值
  idx<-which(  # 找出符合歐式距離最小的元素位置
    dis==dis.min 
  )
  rowcol<-rc(  # 呼叫rc函式傳回m陣列第幾行列，如函式說明
    idx=idx[1], # 元素位置
    dim=nrow(m) # 目前陣列的維度(初始為7x7，隨著迴圈漸減)
  ) 
  rname<-rownames(m)[rowcol['row']] # 傳回rowcol對應的rowname
  cname<-colnames(m)[rowcol['col']] # 傳回rowcol對應的colname
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
      strsplit(rownames(m)[c],'-')
    ) 
    avg.d<-dist.func( # 呼叫dist.func依平均法重新計算歐式距離
      x=x,
      y=y,
      method='average'  # 本例指定平均法
    )
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
    )
  )
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
  main='集群數的整體相似性度量'
)
abline(v=3,col='blue',lty=2)
###############繪出分群多階圖################
cluster.e <- hclust( # 以平均方式進行群組分析
  distance,
  method = "average"  # 使用單一法
)
plot(  # 繪出以歐氏距離分析的多階群組圖
  x=cluster.e,
  xlab='Cluster',
  ylab='Euclidean distance',
  main='Hierarchical cluster Analysis',
)

################本例結束#################################




#############暫時沒用#############################
dist.seq<- function(row,col){  # 傳入row、col傳回dist物件元素的序碼
  tot.row<-case.num-1
  col*(tot.row+tot.row-col+1)/2-case.num+row
}
##############觀察值分布圖########################
plot(   # 產生繪圖物件
  x=ob$V1,
  y=ob$V2,
  type='n',
  xlab='觀察值V1',
  ylab='觀察值V2',
  main="觀察值分布圖",
  xlim = c(1,10),
  ylim = c(1,10)
)
text( # 於繪圖物件標示受測者代號
  x=ob$V1,
  y=ob$V2,
  labels=rownames(ob)
)
#####################################
