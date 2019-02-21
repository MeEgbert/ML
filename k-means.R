data<-iris
data<-data[,1:4]
test<-standard(data)
#数据标准???
standard<-function(data){
  n<-length(data)
  m<-unique(lengths(data))
  for(i in 1:n){
    max_factor<-max(data[,i])
    min_factor<-min(data[,i])
    for(j in 1:m){
      data[j,i]<-(data[j,i]-min_factor)/(max_factor-min_factor)
    }
  }
  return (data)
}

#根据已有分类结果计算类中???
cal_center<-function(data){
  n<-length(data)
  class<-unique(data[,n])
  m<-length(class)
  center<-data.frame()
  for(i in 1:m){
    class_factor<-class[i]
    case<-data[which(data[,n]==class_factor),1:n-1]
    for(j in 1:length(case)){
      center[i,j]<-mean(case[,j])
    }
    center[i,n]<-class_factor
  }
  return(center)
}
#判断类中心是否已收敛
judge<-function(center_pre,center_back){
  n<-unique(lengths(center_pre))
  m<-length(center_pre)
  total<-0
  for(i in 1:n){
    total<-total+cal_dist(center_pre[i,-m],center_back)
  }
  if(total<0.00001){
    return(TRUE)
  }
  return(FALSE)
}
#计算欧式距离
cal_dist<-function(data_factor,center_factor){
  n<-length(data_factor)
  dist<-0
  for(i in 1:n){
    dist<-dist+(data_factor[1,i]-center_factor[1,i])^2
  }
  dist<-sqrt(dist)
  return(dist)
}
#确定所属类中心
sort<-function(data,center){
  n<-length(data)
  m<-unique(lengths(data))
  k<-unique(lengths(center))
  min_dist<-0
  for(i in 1:m){
    min_dist<-0
    for(j in 1:k){
      if(i==1&j==1){
        dist<-cal_dist(data[i,],center[j,])
      }else{
        dist<-cal_dist(data[i,1:length(data)-1],center[j,])
      }
      
      if(j==1){
        min_dist<-dist
        data[i,n+1]<-center[j,n+1]
      }
      else if(min_dist>dist){
        min_dist<-dist
        data[i,n+1]<-center[j,n+1]
      }
    }
  }
  return(data)
}

#初始化聚类中???
init_center<-function(n,k){
  center<-data.frame()
  for(i in 1:k){
    for(j in 1:n){
      center[i,j]<-runif(1,0,1)
    }
    center[i,n+1]<-i
  }
  return(center)
}
#主函???
k_means<-function(data,k,c){
  data<-standard(data)
  n<-length(data)
  m<-unique(lengths(data))
  center_pre<-init_center(n,k)
  while(TRUE){       #保证初始化的聚类中心能将样本分成k
    data<-sort(data,center_pre)
    count<-unique(data[,length(data)])
    data<-data[,1:length(data)-1]
    if(length(count)==k){
      break
    }
    center_pre<-init_center(n,k)
  }
  for(i in 1:c){
    if(i==1){
      data<-sort(data,center_pre)
    }
    center_back<-cal_center(data)
    if(judge(center_pre,center_back)==TRUE){
      return(data)
    }else{
      center_pre<-center_back
      data<-sort(data[,1:length(data)-1],center_pre)
    }
  }
  return(data)
}
kmeans<-k_means(data,3,35)