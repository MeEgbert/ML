data<-iris
data<-data[,-5]
#数据标准化
standard<-function(data){
  n<-length(data)
  m<-nrow(data)
  for(i in 1:n){
    max_factor<-max(data[,i])
    min_factor<-min(data[,i])
    for(j in 1:m){
      data[j,i]<-(data[j,i]-min_factor)/(max_factor-min_factor)
    }
  }
  return (data)
}
##初始化聚类中心
init_center<-function(n,k){
  c<-0
  while(1){
    c<-round(runif(k,1,n))
    if(length(unique(c))==k){
      break
    }
  }
  return(c)
}
#计算损失值
deduce<-function(data,center){
  n<-nrow(data)
  dist<-as.matrix(dist(data))
  class<-data.frame()
  for(i in 1:n){
    class[i,1]<-min(dist[i,center])
  }
  lose<-sum(class[,1])
  return(lose)
}
###PAM
PAM<-function(data,k){
  n<-nrow(data)
  m<-ncol(data)
  data<-standard(data)                          #数据标准化
  class<-data.frame()
  dist<-as.matrix(dist(data))                   #构建距离矩阵（欧氏距离）
  center<-init_center(n,k)                      #初始化聚类中心
  count<-0
  while(1){                                     #迭代聚类中心
    count<-count+1
    print(count)
    for(i in 1:n){                              #样本根据中心及距离矩阵进行聚类
      class[i,1]<-which.min(dist[i,center])
      class[i,2]<-min(dist[i,center])
    }
    lose<-sum(class[,2])
    temp_c<-center
    for(i in 1:n){                              
      token<-0
      new_ded<-0
      if(i%in%temp_c){
        next
      }
      else
        {
          temporary<-temp_c
        for(j in 1:k){                          #循环替换中心，试探最佳替换位置
          temp_c[j]<-i
          new_ded<-deduce(data,temp_c)          #计算替换中心后的损失值
          if(new_ded>lose){
            temp_c<-temporary
          }
          else{
            lose<-new_ded
            temp_c<-temporary
            token<-j
          }
        }
        }
      if(token!=0){                             #确定是否可替换中心
        temp_c[token]<-i                        #替换中心
      }
    }
    if(length(intersect(center,temp_c))==k){   #当中心不再变换，停止迭代
      break
    }
    else
      {
      center<-temp_c
    }
  }
  return(class)#返回聚类结果
}
test<-PAM(data,3)
