data<-read.csv("balance-scale.csv",stringsAsFactors=F)
data_struct<-data[1:550,]
#贝叶斯算�?
bayes<-function(condition,data){
class<-unique(data[[1]])
print(class)
n<-length(class)
result<-""
max<-0
for(i in 1:n){
  print(class[i])
  data_class<-data[which(data[1]==class[i]),2:length(data)]
  pro_class<-length(data_class[[1]])/length(data[[1]])
  pro_condition<-probability(condition,class[i],data_class)
  print(pro_class*pro_condition)
  if(max<pro_class*pro_condition){
    max<-pro_class*pro_condition
    result<-class[i]
  }
}
print(result)
}

#计算条件概率
probability<-function(factor,class,data){
  count_total<-as.numeric(lengths(data[1]))
  n<-length(factor)
  pro<-1
  for(i in 1:n){
    pro<-pro*(length(data[which(data[i]==factor[i]),1])/count_total)
  }
  return(pro)
}
bayes(c(5,3,1),data)