library(mboost)
library(randomForest)
library(ggplot2)
library(partykit)
library(doParallel)

data <- read.csv2(file = "spambase.csv", header = TRUE)
data$Spam <- as.factor(data$Spam)
n=dim(data)[1]
set.seed(12345)
id <- sample(1:n, floor(n/3*2))
train <- data[id,]
test <- data[-id,]

#adaboost##########################################################
errors1 <- vector()
errors2 <- vector()
for (i in 1:10) {
  mdl_bb <- blackboost(
    formula = Spam~.,
    family = AdaExp(),
    data = train,
    control = boost_control(mstop = i*10)
  )
  p1 <- predict(mdl_bb, test, type = "class")
  p2 <- predict(mdl_bb, train, type = "class")
  e1 <- mean(p1!=test$Spam)
  errors1[i] <- e1
  e2 <- mean(p2!=train$Spam)
  errors2[i] <- e2
}
errors31 <- data.frame(x = rep(1:10,2),
                       v = c(errors1,errors2), 
                       group = c(rep("test",10),rep("train",10)))
ggplot(data = errors31, aes(x=x, 
                           y=v, 
                           color=group))+
  geom_line()+
  geom_point()+
  xlab("number of weak learners")+
  ylab("test error rates")

#randomforst#######################################################
errors1 <- vector()
errors2 <- vector()
set.seed(12345)
cl <- parallel::makeCluster(getOption("cl.cores", 4))
registerDoParallel(cl)
for (i in 1:10) {
  mdl_rf <- randomForest(
    formula = Spam~.,
    data = train,
    ntree = i*10,
    parallel = TRUE
  )
  p1 <- predict(mdl_rf, test, type = "class")
  p2 <- predict(mdl_rf, train, type = "class")
  e1 <- mean(p1!=test$Spam)
  errors1[i] <- e1
  e2 <- mean(p2!=train$Spam)
  errors2[i] <- e2
}
errors32 <- data.frame(x = rep(1:10,2),
                      v = c(errors1,errors2), 
                      group = c(rep("test",10),rep("train",10)))
stopCluster(cl)
ggplot(data = errors32, aes(x=x, 
                           y=v, 
                           color=group))+
  geom_line()+
  geom_point()+
  xlab("number of trees")+
  ylab("test error rates")
