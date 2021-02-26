
#2.1#####################################################
library(readxl)
data <- read_xls("creditscoring.xls")
data$good_bad <- as.factor(data$good_bad)
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

#2.2#####################################################
library(tree)
library(rpart)
t21=tree(good_bad~., data=train,
         split = c("deviance"))
plot(t21)
text(t21, pretty=0)
p211 <- predict(t21, train, type = "class")
p212 <- predict(t21, test, type = "class")
e211 <- mean(p211!=train$good_bad)
e211  # 0.212
e212 <- mean(p212!=test$good_bad)
e212  # 0.268

t22=tree(good_bad~., data=train,
         split = "gini")
plot(t22)
text(t22, pretty=0)
p221 <- predict(t22, train, type = "class")
p222 <- predict(t22, test, type = "class")
e221 <- mean(p221!=train$good_bad)
e221  # 0.238
e222 <- mean(p222!=test$good_bad)
e222  # 0.372

# since both training and test misclassification rates are smaller 
t2 <- t21

#2.3########################################################
dt3 <- as.data.frame(matrix(nrow = 14,ncol = 3))
for (i in 2:15) {
  ptree <- prune.tree(t2,best=i)
  p31 <- predict(ptree,type="tree")
  p32 <- predict(ptree,newdata=valid,type="tree")
  dt3[i-1,] <- c(i, deviance(p31),deviance(p32))
}
# dt3
names(dt3) <- c("number of leaves","training","validation")
library(reshape2)
dt3 <- melt(as.data.frame(dt3), id="number of leaves")
library(ggplot2)
ggplot(data=as.data.frame(dt3),
       aes(x=`number of leaves`,y=value,color=variable))+
  geom_line()+
  geom_point()+
  xlab("number of leaves")+
  ylab("deviance")

# when the number of leaves is 4, the validation deviance is the smallest
t3 <- prune.tree(t2,best=4)
plot(t3)
text(t3, pretty=0)
nodes <- as.numeric(rownames(t3$frame))
max(rpart:::tree.depth(nodes))  
# depth 3, variables history duration savings
p31 <- predict(t3, train, type = "class")
p32 <- predict(t3, test, type = "class")
mean(p31!=train$good_bad)          # 0.252
mean(p32!=test$good_bad)           # 0.256

#2.4########################################################
library(MASS)
library(e1071)

b=naiveBayes(good_bad~., data=train)
p41=predict(b, newdata=train)
p42=predict(b, newdata=test)
mat_train <- table(train$good_bad,p41,dnn = c("Labels","prediction"))
# prediction
# Labels bad good
# bad   95   52
# good  98  255
mat_test <- table(test$good_bad,p42,dnn = c("Labels","prediction"))

1-(sum(diag(mat_train))/dim(train)[1]) #0.3
1-(sum(diag(mat_test))/dim(test)[1])   #0.316
# pruned tree is better than Naive Bayes

#2.5########################################################
dt5 <- as.data.frame(matrix(nrow = 19,ncol = 5))
dt5[,1] <- seq(.05,0.95,0.05)
for (i in 1:19) {
  p51 <- predict(t3, test, type="vector")
  p52 <- predict(b, test, type="raw")
  pp51 <- factor(p51[,2]>dt5[i,1],labels = c("bad","good"),levels = c("FALSE","TRUE"))
  pp52 <- factor(p52[,2]>dt5[i,1],labels = c("bad","good"),levels = c("FALSE","TRUE"))
  mat_51 <- table(test$good_bad,pp51,dnn = c("Labels","prediction"))
  mat_52 <- table(test$good_bad,pp52,dnn = c("Labels","prediction"))
  temp <-c(mat_51[2,2]/sum(mat_51[2,]), mat_51[1,2]/sum(mat_51[1,]), 
           mat_52[2,2]/sum(mat_52[2,]), mat_52[1,2]/sum(mat_52[1,]))
  dt5[i,2:5] <- temp
}
names(dt5) <- c("pi","TPR", "FPR","TPR", "FPR")
dt55 <- rbind(dt5[,2:3],dt5[,4:5])
dt55$group <- c(rep("optimal tree",19),rep("naive bayes",19))
library(ggplot2)
ggplot(dt55, aes(x=FPR,y=TPR,color=group))+
  geom_line()+
  labs(title="ROC curves")
# the NB is better


#2.6########################################################
b=naiveBayes(good_bad~., data=train)
p41=predict(b, newdata=train,type = "raw")
p41 <- ifelse(p41[,1]*10>p41[,2],"bad","good")
mat_train <- table(train$good_bad,p41,dnn = c("Labels","prediction"))
mat_train[1,2] <- mat_train[1,2]
mat_train
1-(sum(diag(mat_train))/dim(train)[1]) #0.546
# prediction
# Labels bad good
# bad  137   10
# good 263   90

p42=predict(b, newdata=test,type = "raw")
p42 <- ifelse(p42[,1]*10>p42[,2],"bad","good")
mat_test <- table(test$good_bad,p42,dnn = c("Labels","prediction"))
mat_test
# prediction
# Labels bad good
# bad   71    5
# good 122   52

1-(sum(diag(mat_train))/dim(train)[1]) #0.546
1-(sum(diag(mat_test))/dim(test)[1])   #0.508

