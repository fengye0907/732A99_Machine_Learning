
##3.1##########################################################################
require(ggplot2)
data <- readxl::read_xlsx("tecator.xlsx")
ggplot(data=data, aes(x=Protein,y=Moisture))+
  geom_point()+
  geom_smooth()

##3.2-3.3######################################################################
# polynomial regression
data <- scale(data);set.seed(123);
id <- sample(1:nrow(data),nrow(data))
train <- as.data.frame(data[id[1:107],c("Protein","Moisture")])
val <- as.data.frame(data[id[108:215],c("Protein","Moisture")])
MSE_train <- vector()
MSE_val <- vector()
for(i in 1:6){
  md <- lm(Moisture ~ poly(Protein, degree=i), data = train)
  pred1 <- predict(md,train)
  MSE_train[i] <- mean((train[,2]-pred1)^2)
  pred2 <- predict(md,val)
  MSE_val[i] <- mean((val[,2]-pred2)^2)
}
dt <- data.frame(train=MSE_train, val=MSE_val, av=(MSE_train+MSE_val)/2)
require(plotly)
plot_ly(dt, x=1:6,y=~train, mode = "markers+lines", type = "scatter", name="train") %>%
  add_trace(y=~val, name="validation") %>%
  add_trace(y=~av, name="average") %>%
  layout(xaxis = list(title = "degree"), 
         yaxis=list(title= "MSE"))
# MSE can represent the average errors occur between the predicted and 
# real labels, which is the reason why we use it to fit models.

data1 <- data
for (i in 1:6) {
md <- lm(Moisture ~ poly(Protein, degree=i), data = train)
data1[,104+i] <- predict(md,data)
}
ggplot(data)+
  geom_point(aes(x=Protein,y=Moisture))+
  geom_line(data=data1, aes(x=Protein,y=V105))+
  geom_line(data=data1, aes(x=Protein,y=V106))+
  geom_line(data=data1, aes(x=Protein,y=V107))+
  geom_line(data=data1, aes(x=Protein,y=V108))+
  geom_line(data=data1, aes(x=Protein,y=V109),color="red")+
  geom_line(data=data1, aes(x=Protein,y=V110))
# the 5th model is the best, since its average MSE of training and validation
# sets are smallest compared with other models.

##3.4##########################################################################
require(MASS)
data_fat <- as.data.frame(data[,-c(1,103,104)])
md_AIC <- lm(Fat ~ ., data = data_fat)
mdl_AIC <- stepAIC(md_AIC, direction = 'both',trace = FALSE )
cat("Number of remaining variables:",length(mdl_AIC$coefficients),"\n")
summary(mdl_AIC)

pAIC <- predict(mdl_AIC, data_fat[,1:100])
eAIC <- mean((pAIC-data_fat$Fat)^2 )

##3.5##########################################################################
require(glmnet)
X <- as.matrix(data_fat[,1:100])
Y <- as.matrix(data_fat[,101])
md_RR <- glmnet(X, Y, alpha = 0, family = "gaussian")
plot(md_RR, xvar="lambda", label=TRUE)

# When lambda is larger, all the coefficients would tend to 0.

##3.6##########################################################################
md_LASSO <- glmnet(X, Y, alpha = 1, family = "gaussian")
plot(md_LASSO, xvar="lambda", label=TRUE)

# Compared with the paths in step 5, all the coefficients will go to 0 finally,
# but some of the coefficients can jump away from 0 sometimes. 

##3.7##########################################################################
cvfit=cv.glmnet(X, Y,family = "gaussian", alpha = 1, type.measure = "mse",
                lambda = seq(0,0.01,0.000001))
c(cvfit$lambda.min,cvfit$lambda.se)
p <- predict(cvfit, newx=as.matrix(data_fat[,1:100]), s="lambda.min")
co <- coef(cvfit,s=cvfit$lambda.1se)
length(co@x)
plot(cvfit, ylab="cv(MSE)")

pAIC <- predict(mdl_AIC, data_fat[,1:100])
eAIC <- mean((pAIC-data_fat$Fat)^2 )
eLASSO <- mean((p-data_fat$Fat)^2 )
mse <- data.frame(AIC_MSE=eAIC, LASSO_MSE=eLASSO)
mse
