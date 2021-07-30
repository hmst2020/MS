items_to_recommendations<-c(1,5,seq(10,100,10))  # 各推薦數
number_neighbors<-30    # KNN個數
n_fold<-3   # k-摺疊數
items_to_keep<- 10  # 假設已知客戶購得商品
eval_sets<- evaluationScheme(    # 建立評估方案
  data=bin_matrix,               # 傳入參數值(二元評比矩陣)
  method="cross-validation",     # 分成k-fold 交叉驗證
  k=n_fold,    # 傳入參數值(k-摺疊 數)
  given=min(   # 每客戶保存商品數不得大於min(rowCounts(bin_matrix))
    items_to_keep,
    min(rowCounts(bin_matrix))))
length(eval_sets@runsTrain)   # 訓練組數
getData(eval_sets,'train',run=1)  # 訓練組第一組
getData(eval_sets,'train',run=2)  # 訓練組第二組
getData(eval_sets,'train',run=3)  # 訓練組第三組
getData(eval_sets,'know')    # 測試組
getData(eval_sets,'unknow')  # 驗證組
(nrow(getData(eval_sets,'train'))+    # 驗證訓練組與測試組合計佔比
    nrow(getData(eval_sets,'know')))/
nrow(bin_matrix)
########### 模型比較 ##########
IBCF_Jaccard<-list(   # 推薦模型及其參數
  name='IBCF',
  param=list(
    method='Jaccard',
    k=number_neighbors
  ))
IBCF_cor<-list(     # 推薦模型及其參數
  name='IBCF',
  param=list(
    method='pearson',
    k=number_neighbors
  ))
UBCF_Jaccard<-list(  # 推薦模型及其參數
  name='UBCF',
  param=list(
    method='Jaccard'
  ))
UBCF_cor<-list(      # 推薦模型及其參數
  name='UBCF',
  param=list(
    method='pearson'
  ))
model_to_evaluate<-list(  # 各模型列入評估
  IBCF_Jaccard=IBCF_Jaccard,
  IBCF_cor=IBCF_cor,
  UBCF_Jaccard=UBCF_Jaccard,
  UBCF_cor=UBCF_cor)
list_results<-evaluate(  # 評估方案依各模型進行評估
  x=eval_sets,   # 評估方案
  method=model_to_evaluate,   # 各模型
  n=items_to_recommendations)  # 各推薦數
class(list_results)    # 列示物件類別
show(list_results)     # 列印物件內容

print(getConfusionMatrix(  # 列印二元分類混淆矩陣各值
  list_results$IBCF_Jaccard)) 
avg_matrices<-lapply(   # 依各推薦模型平均其各項指標
  X=list_results,
  FUN=avg)  
print(avg_matrices$IBCF_Jaccard)  # 列印IBCF_Jaccard各項指標平均值

recommenderlab::plot(  # 繪製TPR/FPR ROC 曲線圖    
  x=list_results,     # 資料
  y='ROC',            # 指定圖類型ROC
  annotate=c(1),      # 圖標籤記
  legend='topleft')   # 圖例位置
title('TPR/FPR ROC 曲線圖')   # 圖名稱
recommenderlab::plot(  # 繪製precision-recall曲線圖    
  x=list_results,     # 資料
  y='prec/rec',       # 指定圖類型precision-recall
  annotate=c(1),      # 圖標籤記
  legend='left')   # 圖例位置
title(main='precision-recall曲線圖')   # 圖名稱
########### 模型之參數比較 ##########
k_vector<-c(5,10,20,30,40,50)   # 各KNN值
model_to_evaluate<-lapply(      # IBCF各模型KNN參數列入評估
  X=k_vector,
  FUN=function(k){
    res<-list(
      name='IBCF',
      param=list(
        method='Jaccard',
        k=k
      ))
    return (res)
  })
names(model_to_evaluate)<-paste0(  # 為各模型命名
  'IBCF_k_',k_vector)
list_results<-evaluate(          # 各模型進行評估及結果
  x=eval_sets,                   # 評估方案
  method=model_to_evaluate,      # 各模型
  n=items_to_recommendations)    # 各推薦數
avg_matrices<-lapply(   # 依各推薦模型平均其各項指標
  X=list_results,       # 各模型評估結果
  FUN=avg)              # 平均函式
print(avg_matrices$IBCF_k_20)  # 列印IBCF_k_20各項指標平均
recommenderlab::plot(  # 繪製TPR/FPR ROC 曲線圖    
  x=list_results,     # 資料
  y='ROC',            # 指定圖類型ROC
  annotate=c(1),      # 圖標籤記
  legend='topleft')   # 圖例位置
title('ROC(TPR/FPR) 曲線圖')   # 圖名稱
recommenderlab::plot(  # 繪製precision-recall曲線圖    
  x=list_results,     # 資料
  y='prec/rec',       # 指定圖類型precision-recall
  annotate=c(1),      # 圖標籤記
  legend='left')   # 圖例位置
title('precision-recall曲線圖')   # 圖名稱