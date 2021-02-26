##1.1##########################################################################
data <- readxl::read_xlsx("spambase.xlsx")
n=dim(data)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.5))
train <- data[id,]
test <- data[-id,]
rm(n,id)

##1.2##########################################################################
##logistic regression
glm <- glm(formula = Spam~., family = binomial(link = "logit"), data = train)
ptrain <- predict(glm, train, type = "response")
ptest <- predict(glm, test, type = "response")
train$predict <- as.numeric(ptrain>0.5)
test$predict <- as.numeric(ptest>0.5)

mat_train <- table(train$Spam,train$predict,dnn = c("Labels","prediction"))
mat_test <- table(test$Spam,test$predict,dnn = c("Labels","prediction"))
error_train <- round(1-(sum(diag(mat_train))/dim(train)[1]),3)
error_test <- round(1-(sum(diag(mat_test))/dim(test)[1]),3)
print(paste0("train error rate: ", error_train))
print(paste0("test error rate: ", error_test))
# the error rate of training set is round 16.3% and of test set is round 17.7%. 
# it is good that two rates are low and close to each other.

##1.3##########################################################################
train$predict <- as.numeric(ptrain>0.9)
test$predict <- as.numeric(ptest>0.9)

mat_train <- table(train$Spam,train$predict,dnn = c("Labels","prediction"))
mat_test <- table(test$Spam,test$predict,dnn = c("Labels","prediction"))
error_train <- round(1-(sum(diag(mat_train))/dim(train)[1]),3)
error_test <- round(1-(sum(diag(mat_test))/dim(test)[1]),3)
print(paste0("train error rate: ", error_train))
print(paste0("test error rate: ", error_test))
# The error rate of training set is round 30.7% and of test set is round 31.2%.
# All the error rates are higher than the results in step 2, because the new 
# rule makes the result more difficult to get 1. Therefore, the both training
# and test sets are hard to get spam email when predicting.

##1.4##########################################################################
require(kknn)
train$Spam <- as.factor(train$Spam)   # the prediction should be factor variable, not numeric
test$Spam <- as.factor(test$Spam)

mdl <- kknn(Spam~.,train = train, test = train, k = 30)
ptrain <- mdl$fitted.values
mdl <- kknn(Spam~.,train = train, test = test, k = 30)
ptest <- mdl$fitted.values
train$predict <- ptrain
test$predict <- ptest

mat_train <- table(train$Spam,train$predict,dnn = c("Labels","prediction"))
mat_test <- table(test$Spam,test$predict,dnn = c("Labels","prediction"))
error_train <- round(1-(sum(diag(mat_train))/dim(train)[1]),3)
error_test <- round(1-(sum(diag(mat_test))/dim(test)[1]),3)
print(paste0("train error rate: ", error_train))
print(paste0("test error rate: ", error_test))

# The error rate of training set is round 45% and of test set is round 32.8%.
# Both error rates are higher than the results in step 2.

##1.5##########################################################################
mdl <- kknn(Spam~.,train = train, test = test, k = 1)
ptest <- fitted(mdl)
test$predict <- ptest

mat_train <- table(train$Spam,train$predict,dnn = c("Labels","prediction"))
mat_test <- table(test$Spam,test$predict,dnn = c("Labels","prediction"))
error_train <- round(1-(sum(diag(mat_train))/dim(train)[1]),3)
error_test <- round(1-(sum(diag(mat_test))/dim(test)[1]),3)
print(paste0("train error rate: ", error_train))
print(paste0("test error rate: ", error_test))
# The error rate of training set is round 47.8% and of test set is round 36.7%.
# Both error rates are higher than the results in step 4.
