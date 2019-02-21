#初始化数据
init<-function(data){
  product<-unique(data[[2]])
  m<-length(product)
  init_data<-list()
  for(i in 1:m){
    result<-data[which(data[[2]]==product[i]),]
    c<-list(as.vector(result[,1]))
    names(c)<-product[i]
    init_data<-c(init_data,c)
  }
  return(init_data)
}
#判断是否包含元素
isInclude<-function(list_first,list_second){
  token<-FALSE
  for(i in 1:length(list_first)){
    if(all(list_second%in%list_first[[i]])){
      token<-TRUE
      break
    }
  }
  return(token)
}
#构建N项候选项频繁集(N>=2)
struct_CK<-function(pre_LK){
  back_LK<-list()
  n<-length(pre_LK)
  for(i in 1:n){
    lk_1<-pre_LK[[i]]
    pre<-lk_1[1:(length(lk_1)-1)]
    m<-length(pre)
    for(j in i:n){
      lk_2<-pre_LK[[j]]
      back<-lk_2[1:(length(lk_2)-1)]
      list_ck<-union(pre,back)
      if(length(list_ck)==m+1){
        if(length(back_LK)==0){
          back_LK<-c(back_LK,list(list_ck))
          next
        }
        if(m==1){
          back_LK<-c(back_LK,list(list_ck))
        }else{
          if(!isInclude(back_LK,list_ck)){
            back_LK<-c(back_LK,list(list_ck))
          }
        }
        
      }
    }
  }
  print("4")
  return(back_LK)
}#剪枝
cut<-function(unLK_pre,CK){
  
  n1<-length(unLK_pre)
  n2<-length(CK)
  print(n1)
  print("w")
  back_ck<-list()
  if(n1==0||n2==0){
    return(CK)
  }
  for(i in 1:n2){
    token<-0
    for(j in 1:n1){
      if(all(unLK_pre[[j]][1:length(unLK_pre[[j]])-1]%in%CK[[i]])){
        token<-1
        break
      }
    }
    if(token==0){
      back_ck<-c(back_ck,list(CK[[i]]))
    }
  }
  return(back_ck)
}

#构建频繁集和非频繁集
sort_CK<-function(CK,sup){
  back_LK<-list()
  back_noLK<-list()
  n<-length(CK)
  for(i in 1:n){
    if(sup<=CK[[i]][length(CK[[i]])]){
      back_LK<-c(back_LK,list(CK[[i]]))
    }
    else{
      back_noLK<-c(back_noLK,list(CK[[i]]))
    }
  }
  lst<-c(list(back_LK),list(back_noLK))
  return(lst)
}
#计算支持度
calculate_sup_2<-function(LK,init_data,count){
  n<-length(LK)
  for(i in 1:n){
    m<-length(LK[[i]])
    c<-c()
    for(j in 1:m){
      if(j==1){
        a<-LK[[i]][j]
        c<-init_data[[a]]
        next
      }
      a<-LK[[i]][j]
      c<-intersect(c,init_data[[a]])
    }
    if(m==1){
      LK[[i]][m+1]<-length(c)/count
    }else{
      LK[[i]][m+1]<-round((length(c)/count),3)
    }
  }
  return(LK)
}


#频繁项集
Freq<-function(data,init_data,sup){
  lk_1<-as.list(unique(data[[2]]))
  count<-length(unique(data[[1]]))
  support<-list()
  t<-0
  while(TRUE){
    t<-t+1
    print(t)
    if(length(support)==0){
      lk_1<-calculate_sup_2(lk_1,init_data,count)#构造n项集
      lk_1<-sort_CK(lk_1,sup)                    #筛选频繁集和非频繁
      unlk<-lk_1[[2]]                             #提取非频繁集
      support<-c(support,list(lk_1[[1]]))
      lk_1<-lk_1[[1]]
      print(length(lk_1))
    }else{
      lk_1<-struct_CK(lk_1)                      #构造n项集
      print(length(lk_1))
      lk_1<-cut(unlk,lk_1)          #剪枝
      if(length(lk_1)!=0){           #剪枝后不为空   
      lk_1<-calculate_sup_2(lk_1,init_data,count)#计算支持度
      lk_1<-sort_CK(lk_1,sup)                    #筛选频繁集合非频繁集
      if(length(lk_1[[1]])==0){
        break
        }
      else{
        support<-c(support,list(lk_1[[1]]))
        unlk<-lk_1[[2]]
        lk_1<-lk_1[[1]]
        print(length(lk_1))
      }
      }else{
        break
    }
    }
  }
  return(support)
}
result<-Freq(use,init_data,sup)
r<-confidence(result,0.6)
