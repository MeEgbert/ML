data<-read.csv("balance-scale.csv",stringsAsFactors=F)
#璁＄畻缁欏畾鏁版嵁闆嗙殑�??
calc_Info<-function(data){
  nument<-length(data[,1])
  key<-table(data[,1])
  info<-0
  prob<-key/nument
  for(i in 1:length(prob))
    info=info-prob[i]*log(prob[i],2)
  return(info)
}

#鍒掑垎鐗瑰緛鍙橀�?
split<-function(data,variable,value){  
  result<-data.frame()  
  for(i in 1:length(data[,1])){  
    if(data[i,variable]==value)  
      result<-rbind(result,data[i,-variable]) 
  }  
  return(result)  
}  

#閫夋嫨鏈€浣冲垝�??
choose<-function(data){
  numvariable<-length(data[1,])
  base_Info<-calc_Info(data)
  best_InfoGain<-0
  best_Variable<-0
  infogain<-0
  featlist<-c()
  uniquevals<-c()
  for(i in 2:numvariable){
    featlist<-data[,i]
    uniquevals<-unique(featlist)
    new_info<-0
    for(j in 1:length(uniquevals)){
      subset<-split(data,i,uniquevals[j])
      prob<-length(subset[,1])/length(data[,1])
      new_info<-new_info+prob*calc_Info(subset)
    }
    infogain<-base_Info-new_info
    if(infogain>best_InfoGain){
      best_InfoGain<-infogain
      best_Variable<-i
    }
  }
  return(best_Variable)
}


majorityCnt <- function(data){  
  count <-as.numeric(table(data))  
  majorityList<-levels(as.factor(data))  
  if(length(count) == 1){  
    return (majorityList[1])  
  }else{  
    key <-max(count)  
    return (majorityList[which(count == key)][1])  
  }  
} 

creatTree <- function(data){  
  decision_tree <-list()  
  if(length(unique(data[,1]))==1)  
    return (rbind(decision_tree,as.character(unique(data[,1]))))  
  if(length(data) == 1){  
    decision_tree <-rbind(decision_tree,majorityCnt(data[,1]))  
    return (decision_tree)  
  }  
  bestFeature <-choose(data)  
  labelFeature <-names(data[bestFeature])  
  decision_tree = rbind(decision_tree,labelFeature)  
  level<-levels(as.factor(data[,choose(data)]))
  temp_tree = data.frame()  
  for(i in 1:length(level)){  
    dataSet = split(data,bestFeature,level[i])
    node<-paste(labelFeature,"==",level[i])
    decision_tree = rbind(decision_tree,node)  
    temp_tree = creatTree(dataSet)  
    decision_tree = rbind(decision_tree,temp_tree)  
  }  
  
  return (decision_tree)  
}  

creatTree_2 <- function(data){  
  decision_tree <-list()  
  if(length(unique(data[,1]))==1){
    return (as.character(unique(data[,1])))
  }  
    
  if(length(data) == 1){  
    return (majorityCnt(data[,1]))  
  }  
  bestFeature <-choose(data)  
  labelFeature <-names(data[bestFeature])  
  decision_tree<-rbind(decision_tree,labelFeature)  
  level<-levels(as.factor(data[,choose(data)]))
  for(i in 1:length(level)){  
    dataSet <-split(data,bestFeature,level[i])
    node<-paste(labelFeature,"==",level[i])
    decision_tree <-rbind(decision_tree,node)
    pro_factor<-majorityCnt(dataSet[,1])
    N_t<-length(dataSet[[1]])#节点样本数量
    class<-as.character(unique(dataSet[,1]))
    H_t<-0                    #经验�?
    for(j in 1:length(class)){
      data_class<-dataSet[which(dataSet[1]==class[i]),]
      ded<-length(data_class)/N_t
      H_t<-H_t-(ded*log(ded,2))
    }
    print(H_t)
    if(N_t<20){
      decision_tree[[length(decision_tree)]][2]<-list(pro_factor)
    }
    else if(H_t<0.4){
      decision_tree[[length(decision_tree)]][2]<-list(pro_factor)
    }else{
      temp_tree <-creatTree_2(dataSet) 
      #decision_tree = c(decision_tree,list(temp_tree))
      decision_tree[[length(decision_tree)]][2]<-list(temp_tree)
    }
    
  }  
  return (decision_tree)  
}  
result<-creatTree_2(data)