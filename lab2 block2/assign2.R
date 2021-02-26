
#2.1###########################################################################
# Sys.setlocale(locale = "English")
set.seed(12345)
data <- read.csv("data.csv",sep = ";")
idx <- sample(nrow(data),floor(nrow(data)*0.7))
train <- data[idx,]
test <- data[-idx,]

library(pamr)
rownames(data)=1:nrow(data)
xtrain=t(train[,colnames(train)!="Conference"])
ytrain=train[,"Conference"]
mydata=list(x=xtrain,y=as.factor(ytrain),
            geneid=as.character(1:nrow(xtrain)), 
            genenames=rownames(xtrain))

model=pamr.train(mydata)
cvmodel <- pamr.cv(model, mydata)

df <- as.matrix(data.frame(error=cvmodel$error,th=cvmodel$threshold))

df <- df[order(df[,1]),]
pamr.plotcen(model, mydata, threshold=df[1,2])
vib1<- pamr.listgenes(model,mydata,threshold = df[1,2],genenames = TRUE)[,2]
# 231
# vib1[1:10,]

xtest=t(test[,colnames(test)!="Conference"])
ypred <- pamr.predict(model,xtest,threshold = df[1,2],type = "class")
ytest=as.factor(test[,"Conference"])
mean(ytest!=ypred) #0.1

ypred <- pamr.predict(model,xtrain,threshold = df[1,2],type = "class")
mean(ytrain!=ypred) #0.09

#2.2###########################################################################
library(glmnet)
set.seed(12345)
mdl221 <- cv.glmnet(t(xtrain),as.factor(ytrain),family="binomial",alpha=0.5)
p221 <- predict(mdl221,t(xtest),type="class",s="lambda.1se")
mean(p221!=ytest)   # 0.15
p221 <- predict(mdl221,t(xtrain),type="class",s="lambda.1se")
mean(p221!=ytrain)   # 0.1136364
coef <- as.matrix(coef(mdl221,s=mdl221$lambda.1se))
vib2 <- names(coef[coef[,1]!=0,])
# vib2 <- vib2[-1]
length(vib2)  # 12

library(kernlab)
mdl222 <- ksvm(x=t(xtrain),y=as.factor(ytrain),kernel="vanilladot")
p222 <- predict(mdl222,t(xtest))
mean(p222!=ytest)  # 0.05
p222 <- predict(mdl222,t(xtrain))
mean(p222!=ytrain)  # 0.05
vib3 <- coef(mdl222)[[1]]
length(vib3) # 43

#2.3###########################################################################
x <- data[,colnames(train)!="Conference"]
y <- data[,colnames(train)=="Conference"]
j=1:ncol(x)
pvalues <- sapply(j,FUN=function(i){
  pvalue <- t.test(x[,i]~Conference,data = data, 
                   alternative = "two.sided")$p.value
  pvalue
})
idx <- order(pvalues)
pvalues <- as.data.frame(pvalues[idx])
pvalues$index <- idx

a <- 0.05
pvalues$required <- pvalues[,1]< a*as.numeric(rownames(pvalues))/ncol(x)
L <- max(rownames(pvalues[pvalues$required==TRUE,]))
rej <- pvalues[1:L,]$index
rej_f <- colnames(x)[rej]

