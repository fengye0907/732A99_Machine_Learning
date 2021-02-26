##2.1##########################################################################
best_subset_selection <- function(X, Y, Nfolds){
  # create all the possible combinations of features
  m <- ncol(X)
  idx <- 1:(2^m-1)
  t <- vector()
  mat <- sapply(idx, function(id){
    t <- rbind(t, as.integer(intToBits(id)))
  })
  mat <- mat[1:m,]
  # calculate all the costs for each combinations
  cost <- idx
  for(i in idx){
    X_e <- as.matrix(X[,(1:m)[as.logical(mat[,i])]])
    cost[i] <- k_cross_validation(X_e, Y, Nfolds)
  }
  # select the combination with the smallest cost
  best <- which(cost==min(cost))

  # print and plot
  cat("Key features: ",colnames(X)[(1:m)[as.logical(mat[,best])]] )
  cat("\nMin cost: ",min(cost))
  dt <- data.frame(combination=idx, CV_scores=cost, Number_of_features=colSums(mat))
  require(ggplot2)
  ggplot(data=dt, aes(y=CV_scores,x=Number_of_features))+
    geom_point()+
    geom_point(data=dt[best,],aes(y=CV_scores,x=Number_of_features),color="red")
}

k_cross_validation <- function(X_e, Y, Nfolds){
  set.seed(12345)
  n <- nrow(X_e)
  id <- floor(n/Nfolds)
  ids <- list()
  ids_remain <- 1:n
  # set N groups
  for(i in 1:Nfolds){
    if(i!=Nfolds){
      id0 <- sample(1:length(ids_remain), size = id)
    }else{
      id0 <- 1:length(ids_remain) # selected positions in the remain index
    }
    ids[[i]] <- ids_remain[id0]
    ids_remain <- ids_remain[-id0]
  }
  # calculate the total MSE
  SSE <- 0
  for (i in 1:Nfolds) {
    id <- ids[[i]]
    Xtrain <- X_e[-id,]
    Xtest <- X_e[id,]
    Ytrain <- Y[-id]
    Ytest <- Y[id]
    X_ext <- cbind(1,Xtrain)
    mdl <- solve(t(X_ext)%*%X_ext)%*%t(X_ext)%*%Ytrain
    X_ext <- cbind(1,Xtest)
    Ypred <- X_ext%*%mdl
    SSE <- SSE + t((Ypred-Ytest))%*%(Ypred-Ytest)
  }
  SSE/Nfolds
}

##2.2##########################################################################
X <- as.matrix(scale(swiss[,2:6]))
Y <- as.numeric(swiss[,1])
Nfolds <- 5
best_subset_selection(X,Y,Nfolds)

