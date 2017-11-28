
# library(readr)
# movies_train<-read_csv("~/Desktop/[ADS]Advanced Data Science/fall2017-project4-group9/data/eachmovie_sample/data_train.csv")[,-1]
# movies_test<-read_csv("~/Desktop/[ADS]Advanced Data Science/fall2017-project4-group9/data/eachmovie_sample/data_test.csv")[,-1]

# tran_movies_train<-matrix(0,max(movies_train$User),max(movies_train$Movie))
# 
# for(i in 1:nrow(movies_train)){
#   tran_movies_train[movies_train$User[i],movies_train$Movie[i]]=train_movies_train$Score[i]
# }
# 
# tran_movies_test<-matrix(0,max(movies_train$User),max(movies_train$Movie))
# for(i in 1:nrow(movies_test)){
#   tran_movies_test[movies_test$User[i],movies_test$Movie[i]]=movies_test$Score[i]
# }
# save(tran_movies_test,tran_movies_train,file="~/Desktop/tran_movies.RData")
# train_data<-tran_movies_train
# rm(tran_movies_train)
# 
# train_data<-train_data[-which(rowSums(train_data)==0),]
# train_data<-train_data[,-which(colSums(train_data)==0)]


grouped_users<-split(movies_train,as.factor(movies_train$User))
#grouped_users<-lapply(grouped_users,function(df){paste0("mov_",df$Movie)})
grouped_users<-lapply(grouped_users,function(df){df$Movie})
#names(grouped_users)<-paste("user_",names(grouped_users))


grouped_item<-split(movies_train,as.factor(movies_train$Movie))
grouped_item<-lapply(grouped_item,function(df){df$User})
#names(grouped_users)<-paste("user_",names(grouped_users))
save(grouped_item,grouped_users,file="~/Desktop/grouped_data.RData")

#################################
########## start here ##########
################################

library(readr)
movies_train<-read_csv("~/Desktop/[ADS]Advanced Data Science/fall2017-project4-group9/data/eachmovie_sample/data_train.csv")[,-1]
movies_test<-read_csv("~/Desktop/[ADS]Advanced Data Science/fall2017-project4-group9/data/eachmovie_sample/data_test.csv")[,-1]
load("~/Desktop/grouped_data.RData")

cross_mat<-as.matrix(read_csv("~/Desktop/[ADS]Advanced Data Science/fall2017-project4-group9/output/train_movie.csv"))
rownames(cross_mat)<-as.character(cross_mat[,1])
cross_mat<-cross_mat[,-1]
#colnames(cross_mat)<-names(grouped_item)
train_data<-cross_mat
cross_mat<-ifelse(cross_mat>0,1,0)


## Bipartite SimRank
n_user <- length(unique(movies_train$User))
n_item  <- length(unique(movies_train$Movie))


#### user mat
users_mat<-matrix(0,n_user,n_user)
old_users_mat<-users_mat
diag(users_mat)<-1
colnames(users_mat)<-rownames(cross_mat)
rownames(users_mat)<-rownames(cross_mat)

###intersect user mat
inter_users_mat<-matrix(0,n_user,n_user)
colnames(inter_users_mat)<-rownames(cross_mat)
rownames(inter_users_mat)<-rownames(cross_mat)
diag(inter_users_mat)<-rowSums(cross_mat)
for(i in 1:(nrow(inter_users_mat)-1)){
  print(i)
  for(j in (i+1):nrow(inter_users_mat)){
    inter_users_mat[i,j]=length(intersect(grouped_users[[i]],grouped_users[[j]]))/(length(grouped_users[[i]])*length(grouped_users[[j]]))
    inter_users_mat[j,i]=inter_users_mat[i,j]
  }
}

f<-function(vec){
  return(rownames(inter_users_mat)[order(vec,decreasing = T)[2:1001]])
}
prune_user<-apply(inter_users_mat,2,f)



#### Item matrix
item_mat<-matrix(0,n_item,n_item)
old_item_mat<-item_mat
diag(item_mat)<-1
colnames(item_mat)<-colnames(cross_mat)
rownames(item_mat)<-colnames(cross_mat)

###intersect iterm mat
inter_item_mat<-matrix(0,n_item,n_item)
colnames(inter_item_mat)<-colnames(cross_mat)
rownames(inter_item_mat)<-colnames(cross_mat)
diag(inter_item_mat)<-colSums(cross_mat)

for(i in 1:(nrow(inter_item_mat)-1)){
  print(i)
  for(j in (i+1):nrow(inter_item_mat)){
    inter_item_mat[i,j]=length(intersect(grouped_item[[i]],grouped_item[[j]]))/(length(grouped_item[[i]])*length(grouped_item[[j]]))
    inter_item_mat[j,i]=inter_item_mat[i,j]
  }
}

f<-function(vec){
  return(rownames(inter_item_mat)[order(vec,decreasing = T)[2:401]])
}

prune_item<-apply(inter_item_mat,2,f)
save(prune_item,prune_user,file="~/Desktop/prune_data.RData")


#######load prune data
load("~/Desktop/prune_data.RData")
item_len<-sapply(grouped_item,length)
user_len<-sapply(grouped_users,length)

r <- 0.9
s_uv <- 0
s_cd <- 0
nodes <- c()
niter <- 1000

converged <- function(sim, sim_old, eps = 10^-4){
  if (any(abs(sim - sim_old) >= eps)) {return (FALSE)}
  else {return (TRUE)}
}


##each row of old_item_mat1 has to / the the user number of the movie this row represents

t1=Sys.time()
for(iter in 1:niter){
  print(iter)
  if(converged(users_mat,old_users_mat)){break}
  old_users_mat <- users_mat
  
  
  for (c in 1:(n_item-1)){
    print(paste(iter,"item",c))
    node_related_c <- grouped_item[[c]]
    node_c <- names(grouped_item)[c]
    
    start<-Sys.time()
    selected_items<-prune_item[,node_c][as.numeric(prune_item[,node_c])>as.numeric(node_c)]
    #selected_items<-(c+1):n_items
    z=t(cross_mat[,selected_items])%*%users_mat[,as.character(node_related_c)]
    z=rowSums(z)
    print(Sys.time()-start)
    item_mat[node_c,selected_items]<-r*z/(item_len[selected_items]*length(node_related_c))
    item_mat[selected_items,node_c]<-item_mat[node_c,selected_items]
    
    
    # f<-function(vec){
    #   return(sum(related_user[,as.character(vec)]))
    # }
    # 
    # start<-Sys.time()
    # z<-sapply(grouped_item[(c+1):n_item],f)
    # print(Sys.time()-start)
    # 
    
    # for (d in (c+1):n_item){
    #   print(paste("d=",d))
    #   node_related_d <- grouped_item[[d]]
    #   node_d <- names(grouped_item)[d]
    #   
    #   if (node_c == node_d) {next} else {s_cd=0.0}
    #   
    #   related_user<-users_mat[as.character(node_related_c),as.character(node_related_d)]
    #   s_cd=sum(related_user)
    #   item_mat[node_c,node_d] = r * s_cd / (length(node_related_c)*length(node_related_d))
    #   item_mat[node_c,node_d]=item_mat[node_c,node_d]
    # }
  }
  
  for (u in 1:(n_user-1)){
    # if(u%%10==0){
    #   print(paste(iter,"user",u))
    # }
    node_related_u <- grouped_users[[u]]
    node_u <- names(grouped_users)[u]
    
    start<-Sys.time()
    selected_users<-prune_user[,node_u][as.numeric(prune_user[,node_u])>as.numeric(node_u)]
    y=cross_mat[selected_users,]%*%item_mat[,as.character(node_related_u)]
    y=rowSums(y)
    if(u%%10==0){
      print(paste(iter,"user",u))
      print(Sys.time()-start)
    }
    users_mat[node_u,selected_users]<-r*y/(user_len[selected_users]*length(node_related_u))
    users_mat[selected_users,node_u]<-users_mat[node_u,selected_users]
  }
  
  
  iter <- iter + 1
  
}
iter
t2=Sys.time()
t2-t1

save(item_mat,users_mat,file = "~/Desktop/prune_iter4.RData")

##predict
test_mat<-as.matrix(read_csv("~/Desktop/[ADS]Advanced Data Science/fall2017-project4-group9/output/test_movie.csv"))
rownames(test_mat)<-as.character(test_mat[,1])
test_mat<-test_mat[,-1]
#colnames(test_mat)<-names(grouped_item)

mean_wo_0<-function(vec){
  return(mean(vec[vec>0]))
}

var_wo_0<-function(vec){
  return(var(vec[vec>0]))
}


vars=apply(train_data,1,var_wo_0)
var_min=min(vars)
var_max=max(vars)

predicton<-function(a,j,select.neighbor=100){
  p=mean_wo_0(train_data[a,])
  sim_users<-rownames(users_mat)[order(users_mat[a,],decreasing = T)[2:1001]]
  sim_users<-sim_users[(train_data[sim_users,j]!=0)]
  if(length(sim_users)==0){
    return(p)
  }
  if(length(sim_users)==1){
    sim_means=mean_wo_0(train_data[sim_users,])
    sim_vars=var_wo_0(train_data[sim_users,])
    if(users_mat[a,sim_users]==0){
      return(p)
    }
  }else{
    sim_means<-apply(train_data[sim_users,],1,mean_wo_0)
    sim_vars<-apply(train_data[sim_users,],1,var_wo_0)
  }
  
  w=(users_mat[a,sim_users]/sum(users_mat[a,sim_users]))
  
  ## variance weight
  w_var.weight=users_mat[a,sim_users]*((sim_vars-var_min)/var_max)/sum((sim_vars-var_min)/var_max)
  
  if(select.neighbor!=0){
    if(length(w)>select.neighbor){
      ##select neighbors
      w_selected.nei=w[1:select.neighbor]/sum(w[1:select.neighbor])
      ## select neighbors + variance weight
      w_var.weight_selected.nei<-w_var.weight[1:select.neighbor]/(w[1:select.neighbor])
    }else{
      select.neighbor<-length(w)
      w_selected.nei=w
      w_var.weight_selected.nei=w_var.weight
    }
  }
  
  
  ### remove the similar user with 0 rate for the movie j
  
  #original weights
  # p1=p+sum(w*(train_data[sim_users,j]-sim_means))
  # p1_zscore=p+var_wo_0(train_data[a,])*sum(w*(train_data[sim_users,j]-sim_means)/sim_vars)
  
  
  #variance weights
  # p2=p+sum(w_var.weight*(train_data[sim_users,j]-sim_means))
  # p2_zscore=p+var_wo_0(train_data[a,])*sum(w_var.weight*(train_data[sim_users,j]-sim_means)/sim_vars)
  
  #select neighbor
  p3=p+sum(w_selected.nei*(train_data[sim_users[1:select.neighbor],j]-sim_means[1:select.neighbor]))
  p3_zscore=p+var_wo_0(train_data[a,])*sum(w_selected.nei*(train_data[sim_users[1:select.neighbor],j]-sim_means[1:select.neighbor])/sim_vars[1:select.neighbor])
  
  #select neighbor + variance weight
  p4=p+sum(w_var.weight_selected.nei*(train_data[sim_users[1:select.neighbor],j]-sim_means[1:select.neighbor]))
  p4_zscore=p+var_wo_0(train_data[a,])*sum(w_var.weight_selected.nei*(train_data[sim_users[1:select.neighbor],j]-sim_means[1:select.neighbor])/sim_vars[1:select.neighbor])
  
  #return(c(p1,p1_zscore,p2,p2_zscore,p3,p3_zscore,p4,p4_zscore))
  return(c(p3,p3_zscore,p4,p4_zscore))
}

movies_test$p1=NA
movies_test$p1_zscore=NA

movies_test$p2=NA
movies_test$p2_zscore=NA

movies_test$p3=NA
movies_test$p3_zscore=NA

movies_test$p4=NA
movies_test$p4_zscore=NA

movies_test$err=NA

movies_test_200nei<-movies_test
movies_test_200nei[,c("p3","p3_zscore","p4","p4_zscore")]<-NA
movies_test_50nei<-movies_test
movies_test_50nei[,c("p3","p3_zscore","p4","p4_zscore")]<-NA

for(i in 1:nrow(movies_test)){
  if(i%%100==0){print(i)}
  x=movies_test[i,]
  #y=predicton(x$User,x$Movie)
  #movies_test[i,4:11]<-predicton(as.character(x$User),as.character(x$Movie))

  movies_test_200nei[i,c("p3","p3_zscore","p4","p4_zscore")]<-predicton(as.character(x$User),as.character(x$Movie),select.neighbor = 200)
  movies_test_50nei[i,c("p3","p3_zscore","p4","p4_zscore")]<-predicton(as.character(x$User),as.character(x$Movie),select.neighbor = 50)
}

#test_results<-movies_test
#save(test_results,file="~/Desktop/test_results_ROC4_100nei.RData")

## MAE
cal_mae<-function(pred,score){
  return(mean(abs(round(pred)-score),na.rm = T))
}

cal_mae(movies_test$p1,movies_test$Score)
cal_mae(movies_test$p1_zscore,movies_test$Score)

cal_mae(movies_test$p2,movies_test$Score)
cal_mae(movies_test$p2_zscore,movies_test$Score)

cal_mae(movies_test$p3,movies_test$Score)
cal_mae(movies_test$p3_zscore,movies_test$Score)

cal_mae(movies_test$p4,movies_test$Score)
cal_mae(movies_test$p4_zscore,movies_test$Score)
## ROC
library(pROC)
cal_roc<-function(pred,score){
  pred=ifelse(round(pred)>=4,1,0)
  score=ifelse(score>=4,1,0)
  roc_obj <- roc(pred, score)
  return(auc(roc_obj))
}

cal_roc(movies_test$p1,movies_test$Score)
cal_roc(movies_test$p1_zscore,movies_test$Score)

cal_roc(movies_test$p2,movies_test$Score)
cal_roc(movies_test$p2_zscore,movies_test$Score)

cal_roc(movies_test$p3,movies_test$Score)
cal_roc(movies_test$p3_zscore,movies_test$Score)

cal_roc(movies_test$p4,movies_test$Score)
cal_roc(movies_test$p4_zscore,movies_test$Score)

result_table<-matrix(NA,8,4)
result_table<-as.data.frame(result_table)
colnames(result_table)<-c("algorithm","Normalization","MEA","ROC")
result_table$algorithm<-c("SimRank","SimRank","SimRank+Var.Weight","SimRank+Var.Weight","SimRank+Sel.Nei","SimRank+Sel.Nei","SimRank+Var.Weight+Sel.Nei","SimRank+Var.Weight+Sel.Nei")
View(result_table)
result_table$Normalization<-rep(c("dev.from.mean","z-score"),4)

result_table$MEA<-c(cal_mae(movies_test$p1,movies_test$Score),
                    cal_mae(movies_test$p1_zscore,movies_test$Score),
                    cal_mae(movies_test$p2,movies_test$Score),
                    cal_mae(movies_test$p2_zscore,movies_test$Score),
                    cal_mae(movies_test$p3,movies_test$Score),
                    cal_mae(movies_test$p3_zscore,movies_test$Score),
                    cal_mae(movies_test$p4,movies_test$Score),
                    cal_mae(movies_test$p4_zscore,movies_test$Score))


result_table$ROC<-c(cal_roc(movies_test$p1,movies_test$Score),
                    cal_roc(movies_test$p1_zscore,movies_test$Score),
                    cal_roc(movies_test$p2,movies_test$Score),
                    cal_roc(movies_test$p2_zscore,movies_test$Score),
                    cal_roc(movies_test$p3,movies_test$Score),
                    cal_roc(movies_test$p3_zscore,movies_test$Score),
                    cal_roc(movies_test$p4,movies_test$Score),
                    cal_roc(movies_test$p4_zscore,movies_test$Score))

save(result_table,test_results,file="~/Desktop/test_results_ROC4_100nei.RData")







