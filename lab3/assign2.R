library(kernlab)
set.seed(1234567890)
data(spam)
data <- spam; rm(spam)
n=dim(data)[1]
id=sample(1:n, floor(n*0.4))
train=data[id,]
id1=setdiff(1:n, id)
id2=sample(id1, floor(n*0.3))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

#2.1#####################################################
mdl1 <- ksvm(type~.,data=train,
            kernel="rbfdot",kpar=list(sigma=0.05),
            C=0.5)
mdl2 <- ksvm(type~.,data=train,
             kernel="rbfdot",kpar=list(sigma=0.05),
             C=1)
mdl3 <- ksvm(type~.,data=train,
             kernel="rbfdot",kpar=list(sigma=0.05),
             C=5)

id <- which(colnames(data)=="type")
e1 <- mean(predict(mdl1,valid[,-id])!=valid[,id])  #0.08695652
e2 <- mean(predict(mdl2,valid[,-id])!=valid[,id])  #0.07826087
e3 <- mean(predict(mdl3,valid[,-id])!=valid[,id])  #0.07681159

# choose the best model

#2.3###########################################################################
ntrain <- rbind(train,valid)
mdl1 <- ksvm(type~.,data=ntrain,
             kernel="rbfdot",kpar=list(sigma=0.05),
             C=0.5)
mdl2 <- ksvm(type~.,data=ntrain,
             kernel="rbfdot",kpar=list(sigma=0.05),
             C=1)
mdl3 <- ksvm(type~.,data=ntrain,
             kernel="rbfdot",kpar=list(sigma=0.05),
             C=5)
e1 <- mean(predict(mdl1,test[,-id])!=test[,id])  #0.09341057
e2 <- mean(predict(mdl2,test[,-id])!=test[,id])  #0.08689356
e3 <- mean(predict(mdl3,test[,-id])!=test[,id])  #0.08182476

e1 <- mean(predict(mdl1,ntrain[,-id])!=ntrain[,id])
e2 <- mean(predict(mdl2,ntrain[,-id])!=ntrain[,id])
e3 <- mean(predict(mdl3,ntrain[,-id])!=ntrain[,id])

#2.4###########################################################################
mdl3

#2.5###########################################################################
# cost of constraints violation (default: 1) this is the ‘C’-constant of the regularization term in the Lagrange formulation.
# large C: High Variance
# small C: High Bias
  