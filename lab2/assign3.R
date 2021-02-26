
#3.1#####################################################
data <- read.csv2("State.csv")
data <- data[order(data[,"MET"]),]
mdl <- lm(EX ~ poly(MET, degree=3), data = data)
pp <- as.data.frame(predict(mdl,data))
library(ggplot2)
ggplot(data = data,aes(x=MET,y=EX))+
  geom_point()+
  # stat_smooth()+
  geom_line(data=pp,aes(x=data$MET,y=pp[,1]))

mse1 <- sum(abs(predict(mdl)-data[,"EX"])^2)/48 #2445.625
# polynomial

#3.2#####################################################
library(tree)
dt2 <- data[,c("MET","EX")]
t2 <- tree(formula = EX~MET, data = dt2,
           control = tree.control(nobs =  nrow(data), minsize = 8))
set.seed(12345)
cv.res=cv.tree(t2)
plot(cv.res$size, cv.res$dev, type="b",
     col="red")  # size = 3 (number of terminal nodes)

ptree <- prune.tree(t2,best=3)
mse2 <- sum(abs(predict(ptree)-data[,"EX"])^2)/48 #2529.049
plot(ptree)
text(ptree, pretty=0)
melt_point <- dt2
melt_point$pred_EX <- predict(ptree)
library(reshape2)
melt_point <- melt(melt_point, id="MET")
ggplot(data = melt_point, aes(x=MET,y=value,color=variable))+
  geom_point()


re22 <- data.frame(residuals=residuals(ptree))
ggplot(re22,aes(x=residuals))+
  geom_histogram(aes(y=..density..),
                 colour="black",
                 fill="white",
                 bins=30)+
  geom_density(alpha=.2, fill="#FF6666")

# the residuals seem not to be a normal distribution, which means
# that the tree is not good enough

# overfitting

#3.3############################################################################
library(boot)
fun <- function(dt, ind){
  t1 <- tree(formula = EX~MET, data = dt[ind,],
             control = tree.control(nobs =  nrow(dt), minsize = 8))
  ptree <- prune.tree(t1,best=3)
  return(predict(ptree, newdata = dt2))
}
set.seed(12345)
b3 <- boot(dt2,fun, R=5000) 
envel <- envelope(b3)
dt3 <- as.data.frame(t(envel$point))
library(ggplot2)
ggplot()+
  geom_point(data = melt_point, aes(x=MET,y=value,color=variable))+
  geom_line(data=dt3,aes(x=data$MET,y=V1))+
  geom_line(data=dt3,aes(x=data$MET,y=V2))
# bumpy, not smooth

#3.4############################################################################
rng <- function(data, mle) {
  data1=data
  n=nrow(data)
  data1$EX=rnorm(n,predict(mle, newdata=data1),sd(residuals(mle)))
  return(data1)  # add noise to the data
}
set.seed(12345)
b4 <- boot(dt2, fun, R=1000, mle = ptree,
           sim="parametric", ran.gen = rng)

envel <- envelope(b4)
dt4 <- as.data.frame(t(envel$point))

fun2 <- function(dt){
  t1 <- tree(formula = EX~MET, data = dt,
             control = tree.control(nobs =  nrow(dt), minsize = 8))
  ptree <- prune.tree(t1,best=3)
  p <- predict(ptree, newdata = dt2)
  n=length(p)
  predictedP=rnorm(n, mean = p, sd = sd(residuals(ptree)))
  return(predictedP)  # add noise to the prediction
}

set.seed(12345)
b42 <- boot(dt2, fun2, R=5000, mle = ptree,
           sim="parametric", ran.gen = rng)

envel2 <- envelope(b42)
dt42 <- as.data.frame(t(envel2$point))
ggplot()+
  geom_point(data = melt_point, aes(x=MET,y=value,color=variable))+
  geom_line(data=dt4,aes(x=data$MET,y=V1))+
  geom_line(data=dt4,aes(x=data$MET,y=V2))+
  geom_line(data=dt42,aes(x=data$MET,y=V1),color="blue")+
  geom_line(data=dt42,aes(x=data$MET,y=V2),color="blue")
