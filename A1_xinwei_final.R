rm(list = ls())

library(tidyverse)
library(data.table)
library(stringr)
library(dplyr)

datjss <- read.csv("~/Desktop/ECON613/A1/datjss.csv")
datsss <- read.csv("~/Desktop/ECON613/A1/datsss.csv")
datstu <- read.csv("~/Desktop/ECON613/A1/datstu.csv")

# Part1
# Exercise 1

## Number of students
length(datstu$X)

## Number of schools
numberofschools<-unique(datsss$schoolcode)
numberofschools<- data.frame(numberofschools)
nrow(numberofschools)

## Number of programs
length(unique(unlist(datstu[11:16])))

## Number of choices (school,program)

a<-data.frame(datstu$schoolcode1,datstu$choicepgm1)
b<-data.frame(datstu$schoolcode2,datstu$choicepgm2)
c<-data.frame(datstu$schoolcode3,datstu$choicepgm3)
d<-data.frame(datstu$schoolcode4,datstu$choicepgm4)
e<-data.frame(datstu$schoolcode5,datstu$choicepgm5)
f<-data.frame(datstu$schoolcode6,datstu$choicepgm6)

colnames(a)[colnames(a) == "datstu.schoolcode1"] <- "schoolcode"
colnames(a)[colnames(a) == "datstu.choicepgm1"] <- "choicepgm"
colnames(b)[colnames(b) == "datstu.schoolcode2"] <- "schoolcode"
colnames(b)[colnames(b) == "datstu.choicepgm2"] <- "choicepgm"
colnames(c)[colnames(c) == "datstu.schoolcode3"] <- "schoolcode"
colnames(c)[colnames(c) == "datstu.choicepgm3"] <- "choicepgm"
colnames(d)[colnames(d) == "datstu.schoolcode4"] <- "schoolcode"
colnames(d)[colnames(d) == "datstu.choicepgm4"] <- "choicepgm"
colnames(e)[colnames(e) == "datstu.schoolcode5"] <- "schoolcode"
colnames(e)[colnames(e) == "datstu.choicepgm5"] <- "choicepgm"
colnames(f)[colnames(f) == "datstu.schoolcode6"] <- "schoolcode"
colnames(f)[colnames(f) == "datstu.choicepgm6"] <- "choicepgm"

datachoice<-bind_rows(a,b,c,d,e,f)
numberofchoices <- unique(datachoice)
datachoice<-datachoice[!(is.na(numberofchoices$schoolcode)),]
nrow(numberofchoices)

## Missing Test score
sum(is.na(datstu$score))

## Apply to the same school (different programs)
frame <- data.frame(datstu)
frame$n = abs(frame$schoolcode1 - frame$schoolcode2) + abs(frame$schoolcode2 - 
          frame$schoolcode3) + abs(frame$schoolcode3 - frame$schoolcode4) + 
          abs(frame$schoolcode4 - frame$schoolcode5) + abs(frame$schoolcode5 - frame$schoolcode6)
length(which(frame$n==0))

## Apply to less than 6 choices
complete6choices <- sum(complete.cases(frame[,5:10]))
numberofstudents - complete6choices


# Exercise 2

datsss2<-datsss
datsss2<-datsss2[!(is.na(datsss2$ssslong)),]
datsss2<-datsss2[,-1]
datsss2<-as.data.frame(unique(datsss2))

data2<-merge(x=numberofchoices,y=datsss2,by="schoolcode",all.x = TRUE, all.y = FALSE)

datstu_clear<-datstu
datstu_clear<-datstu_clear[!(is.na(datstu_clear$rankplace)),] #delete observations with missing value in rankplace

for (i in 1:nrow(datstu_clear)){
  if (datstu_clear$rankplace[i]==1){
    datstu_clear$schoolcode[i]<-datstu_clear$schoolcode1[i]
    datstu_clear$choicepgm[i]<-datstu_clear$choicepgm1[i]
  }
  if (datstu_clear$rankplace[i]==2){
    datstu_clear$schoolcode[i]<-datstu_clear$schoolcode2[i]
    datstu_clear$choicepgm[i]<-datstu_clear$choicepgm2[i]
  }
  if (datstu_clear$rankplace[i]==3){
    datstu_clear$schoolcode[i]<-datstu_clear$schoolcode3[i]
    datstu_clear$choicepgm[i]<-datstu_clear$choicepgm3[i]
  }
  if (datstu_clear$rankplace[i]==4){
    datstu_clear$schoolcode[i]<-datstu_clear$schoolcode4[i]
    datstu_clear$choicepgm[i]<-datstu_clear$choicepgm4[i]
  }
  if (datstu_clear$rankplace[i]==5){
    datstu_clear$schoolcode[i]<-datstu_clear$schoolcode5[i]
    datstu_clear$choicepgm[i]<-datstu_clear$choicepgm5[i]
  }
  if (datstu_clear$rankplace[i]==6){
    datstu_clear$schoolcode[i]<-datstu_clear$schoolcode6[i]
    datstu_clear$choicepgm[i]<-datstu_clear$choicepgm6[i]
  }
}

datstu_clear<-datstu_clear[!(datstu_clear$rankplace == 99),]

data2_final<-datstu_clear %>% 
  group_by(schoolcode,choicepgm) %>% 
  summarise(cutoff=min(score),quality = mean(score),size = n()) 

data2_final<-merge(x=data2,y=data2_final,by= c("schoolcode", "choicepgm"))


# Exercise 3

data3<-merge(x=datstu_clear,y=data2_final,by= c("schoolcode", "choicepgm"))
data3<-merge(x=data3,y=datjss,by="jssdistrict",all.x = TRUE, all.y = FALSE)

colnames(data3)[colnames(data3) == "point_x"] <- "jsslong"
colnames(data3)[colnames(data3) == "point_y"] <- "jsslat"

data3$distance<-0

for (i in 1:nrow(data3)){
  data3$distance[i]<-sqrt((69.172 * (data3$ssslong[i]-data3$jsslong[i])*cos(data3$jsslat[i]/57.3))^2 + (69.172 * (data3$ssslat[i] - data3$jsslat[i]))^2)
}

# Exercise 4

# Group by ranked choice
data3<-data3[!(is.na(data3$score)),]
data3<-data3[!(is.na(data3$distance)),]

data3 %>% 
  group_by(rankplace) %>% 
  summarise(cutoff=min(score),quality = mean(score),distance=mean(distance))

# Group by quantile
installed.packages("devtools")
devtools::install_github("moodymudskipper/cutr")
library(cutr)

data3$quantile <- smart_cut(data3$score, 4, "g", output = "numeric")

data3$quantile <- replace(data3$quantile, data3$quantile==1, "0%-25%")
data3$quantile <- replace(data3$quantile, data3$quantile==2, "25%-50%")
data3$quantile <- replace(data3$quantile, data3$quantile==3, "50%-75%")
data3$quantile <- replace(data3$quantile, data3$quantile==4, "75%-100%")

data3 %>% 
  group_by(quantile) %>% 
  summarise(cutoff=min(score),quality = mean(score),distance=mean(distance))


# Part2
rm(list = ls())

# Exercise 5
obs <- 10000
X1 <- runif(obs, max = 3, min = 1)
X2 <- rgamma(obs,3,scale = 2)
X3 <- rbinom(obs,1,0.3)
eps <- rnorm(obs, mean = 2, sd = 1)
Y <- 0.5 + 1.2*X1 - 0.9*X2 + 0.1*X3 + eps
ydum <- ifelse(Y > mean(Y),1,0)
mydata <- data.frame(cbind(Y,ydum,X1,X2,X3,eps))

# Exercise 6

# Correlation between x1 and y, which is about 0.20 and is very different from 1.2.

cor(Y,X1)

# Regression of Y on X

# Regression of Y on X
cons <- matrix(1,10000,1)
X <- matrix(c(X1,X2,X3),10000,3)
X <- cbind(cons,X)

r1<- solve(t(X) %*% X) %*% t(X)%*% Y
r1

# Calculation of standard error
se <- Y - r1[1]-r1[2]*X1 - r1[3]*X2 - r1[4]*X3
se

# Exercise 7

# Optimizing Probit
X <- cbind(1,X1,X2,X3)
y <- as.matrix(Y)
probit.llike <- function(b., y. = ydum, X. = X){
  phi <- pnorm(X.%*%b.)
  phi[phi==1] <- 0.9999 # avoid NaN of log function
  phi[phi==0] <- 0.0001
  f <- sum(y.*log(phi))+sum((1-y.)*log(1-phi))
  f <- -f
  return(f)
}

result.p <- optim(par = c(0,0,0,0), probit.llike)
result.p$par

# Optimizing Logit
logit.llike <- function(b., y. = ydum,X. = X){
  gamma <- plogis(X%*%b.)
  f <- sum(y.*log(gamma))+sum((1-y.)*log(1-gamma))
  f <- -f
  return(f)
}
result.l <- optim(par = c(0,0,0,0), logit.llike)
result.l$par

# Optimizing Linear - same with OLS

result.lp <- lm(ydum~X1+X2+X3)
summary(result.lp)


## Exercise 8

## Compute Marginal Effect of X of probit

probit.ME <- function(df){
  result <- glm(ydum ~ X1 + X2 + X3, family=binomial(link = "probit"),df)
  ME <- mean(dnorm(X%*%coef(result)))*coef(result)
  return(ME)
}
probit.ME(mydata)

## Compute Marginal Effect of X of Logit
logit.ME <- function(df){
  result <- glm(ydum ~ X1 + X2 + X3, family=binomial(link = "logit"),df)
  ME <- mean(dlogis(X%*%coef(result)))*coef(result)
  return(ME)
}
logit.ME(mydata)

## Compute the Standard Error

jacobian <- function(fun,par){
  d <- 1e-8
  par. <- matrix(par,length(par),length(par)) 
  J <- (apply(par. + diag(d,length(par)),2,fun)-apply(par.,2,fun))/d
  return(J)
}

## Compute the Standard Error of ME (Probit)
result.p.glm <- glm(ydum ~ X1 + X2 + X3, family = binomial(link = "probit"), data = mydata)

J <- jacobian(function(result) mean(dnorm(X%*%result))*result, coef(result.p.glm)) # "jacobian" defined in EX3
cov_matrixv <- vcov(result.p.glm)
se.p <- sqrt(diag(J%*%cov_matrixv%*%t(J)))
se.p

## Compute the Standard Error of ME (Logit)
result.l.glm <- glm(ydum ~ X1 + X2 + X3, family = binomial(link = "logit"), data = mydata)

J <- jacobian(function(result) mean(dlogis(X%*%result))*result, coef(result.l.glm))
cov_matrixv <- vcov(result.l.glm)
se.l <- sqrt(diag(J%*%cov_matrixv%*%t(J)))
se.l