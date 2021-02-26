
#4.1#####################################################
mydata <- read.csv2("NIRspectra.csv")
data1=mydata
data1[,"Viscosity"]=c() # remove targets
res=prcomp(data1,scale=FALSE)
lambda=res$sdev^2 #eigenvalues
# lambda  # 126 lambda

screeplot(res) # plots the variances against the number of the principal component

proportion <- sprintf("%2.3f",lambda/sum(lambda)*100) # proportions of variations

# "%2.3f" is precision
sum(as.numeric(proportion)[1:2]) # 99.595>99

library(ggplot2)
dt1 <- as.data.frame(res$x[,1:2])
ggplot(data = dt1, aes(x=PC1,y=PC2))+
  geom_point()
# ofcourse there are outliers in this plot, which is also the reason 
# why the variance of PC1 is so big.

#4.2#####################################################
U <-res$rotation
split.screen(c(1,2))
screen(1)
plot(U[,1], main="Traceplot, PC1")
screen(2) 
plot(U[,2], main="Traceplot, PC2")

#4.3#####################################################
library(fastICA)
set.seed(12345)
res2 <- fastICA(data1, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
        method = "R") #ICA
summary(res2)
W1 <-  (res2$K%*%res2$W)
split.screen(c(1,2))
screen(1)
plot(W1[,1],main="Traceplot, ICA1")
screen(2)
plot(W1[,2],main="Traceplot, ICA2")

D <- as.matrix(data1) %*% W1

dt3 <- as.data.frame(D)
ggplot(data = dt3, aes(x=V1,y=V2))+
  geom_point()

# The results of PCA and ICA 



