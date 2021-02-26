
##1.1########################################################################
data <- read.csv(file = "australian-crabs.csv",header = TRUE)
library(ggplot2)
ggplot(data,aes(x=CL,y=RW,color=sex))+
  geom_point()

##1.2########################################################################
library(MASS)
f <- sex~CL+RW
mdl_lda <- lda(formula=f,data=data)
p <- predict(mdl_lda, data[,c(6,5)])
predict_sex <- p$class
ggplot(data,aes(x=CL,y=RW,color=predict_sex))+
  geom_point()

e <- mean(predict_sex!=data$sex)
e
library(RSNNS)
table(data[,2],predict_sex,dnn = c("targets","predictions"))

# several points on the right bottom change

##1.3########################################################################
mdl_lda1 <- lda(formula=f,data=data, prior=c(0.1,0.9))
p1 <- predict(mdl_lda1, data[,c(6,5)])
predict_sex1 <- p1$class
ggplot(data,aes(x=CL,y=RW,color=predict_sex1))+
  geom_point()
e1 <- mean(predict_sex1!=data$sex)
e1
table(data[,2],predict_sex,dnn = c("targets","predictions"))

# more crabs are predicted as male

##1.4#######################################################################
mdl_glm <- glm(formula = f, family = binomial(link = "logit"), data = data)
predict_sex2 <- predict(mdl_glm, data, type = "response")
predict_sex2 <- factor(predict_sex2>0.5,labels = c("Female","Male"))
ggplot(data,aes(x=CL,y=RW,color=predict_sex2))+
  geom_point()
e2 <- mean(predict_sex2!=data$sex)
e2
table(data[,2],predict_sex,dnn = c("targets","predictions"))

# decision boundary
lm1 <- lm(RW~CL,data=data[data$sex=="Female",])
lm2 <- lm(RW~CL,data=data[data$sex=="Male",])
mdl_db <- colMeans(rbind(lm1$coefficients,lm2$coefficients))
x=min(data$CL):max(data$CL)
db <- cbind(1,x)%*%mdl_db
dt <- as.data.frame(cbind(x=x,y=db))
ggplot()+
  geom_point(data=data,aes(x=CL,y=RW,color=predict_sex2))+
  geom_line(data=dt,aes(x=x,y=V2))
