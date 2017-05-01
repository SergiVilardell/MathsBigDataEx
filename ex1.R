library(mnormt)
library(tidyverse)
library(rgl)
library(sparr)


#Create mean and cov values 1

mu_1 <- c(3, 1, 13)
sigma_1 <- matrix(c(5, 1.574, -3.352, 1.574, 2, 1.241,-3.352, 1.241, 6),nrow = 3,ncol = 3)

#Data set 1

data_1 <- rmnorm(n=10^3,mu_1, sigma_1)
data_1 <- as.data.frame(data_1)
group<- rep(1,10^3)
data_1$group <- group
colnames(data_1) <- c("X1","X2","X3","G")

#Create mean and cov values 2

mu_2 <- c(7, -2, 10)
sigma_2 <- matrix(c(3, -1.385, 2.939, -1.385, 1, -1.979, 2.939, -1.979, 8),nrow = 3,ncol = 3)

#Data set 2

data_2 <- rmnorm(n=10^3,mu_2, sigma_2)
data_2 <- as.data.frame(data_2)
group<- rep(2,10^3)
data_2$group <- group
colnames(data_2) <- c("X1","X2","X3","G")

#Merge the datasets

data <- rbind(data_1, data_2)
colnames(data) <- c("X1","X2","X3","G")

#Linear regression data1
mod0 <- lm(X3~X1,data=data_1)

#Function to write the lm equation

lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 4),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(R)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

#Plot
ggplot(data = data_1, aes(x = X1, y = X3)) +
  geom_point() +
  geom_smooth(method=lm, se = FALSE)+
  geom_text(aes(x = 7.5, y = 20, label = lm_eqn(mod0)), parse = TRUE)

plot(mod0,1:6)

sum.mod <- summary(mod0)

# confidence band
conf.band<-predict(mod0 ,interval="confidence")
head(conf.band) 
# prediction, lower and upper bounds
# prediction band
pred.band<-predict(mod0,interval="prediction")
yban<-data.frame(conf.band,pred.band[,-1]) 
# the first column was repeated

matplot(data_1$X1, yban, ylim=c(min(yban),max(yban)),
        lty = c(1,1,1,2,2), col=c(1,2,2,4,4), type = "l",
        ylab = "ylab",xlab="xlab",
        main="confidence bands (red) and prediction bands (blue)", 
        cex.main=.8)
points(data_1$X1,data_1$X3,pch=20,cex=.8)

#Multiple linear model

mod1 <- lm(X3~X1+X2,data=data_1)

input <- data.frame(X1 = 2, X2 = 0)
input.conf <- predict.lm(mod1, input, interval ="confidence")
input.pred <- predict.lm(mod1, input, interval ="prediction")

plot(data,col=c("red","blue")[data$G])

#Coplot

coplot(X3~X1|as.factor(G), data)

#Better with ggplot
ggplot(data, aes(x = X1, y = X3)) + 
  geom_point(aes(colour=G),show_guide = FALSE) + 
  facet_wrap(~ G)


plot3d(data,col=c("red","blue")[data$G])

#Last

data1 <- subset(data_1, select = c("X1", "X2","G"))



bivariate.density(data1 ,xrange = c(-4,11),yrange = c(-4,6))
