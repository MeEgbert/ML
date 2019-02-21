data<-read.csv("balance-scale.csv",stringsAsFactors=F)

#计算给定数据集的�?
calc_Info<-function(data){
  nument<-length(data[,1])
  key<-table(data[,1])
  info<-0
  prob<-key/nument
  for(i in 1:length(prob))
    info=info-prob[i]*log(prob[i],2)
  return(info)
}

#划分特征变量
split<-function(data,variable,value){  
  result<-data.frame()  
  for(i in 1:length(data[,1])){  
    if(data[i,variable]==value)  
      result<-rbind(result,data[i,-variable]) 
  }  
  return(result)  
}  

#选择最佳划�?
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

#建立决策�?
buildtree<-function(data){
  if(choose(data)==0){
    print(as.character(unique(data[,1])))
    }
  else{
    k<-choose(data)
    print(names(data[k]))
    level<-unique(data[,choose(data)])
    if(length(level)==1)
      print(as.character(unique(data[,1])))
      
    else{
      for(i in 1:length(level)){
        node<-paste(names(data[k]),"=",level[i])
        print(node)
        data1<-split(data,choose(data),level[i])
        key<-length(unique(data1[,1]))
        if(key==1){
          print(as.character(unique(data1[,1])))
          next
        }
        else 
          {
            if(length(data1)==1){
              print(as.character(unique(data1[,1])))
            }
        else
        {
          buildtree(data1)
          }
          
          }
      }
    }
  }
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
result<-creatTree(data)
