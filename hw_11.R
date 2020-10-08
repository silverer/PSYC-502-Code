setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
source('helpful_formulas.R')

####Set up####
X <- read.csv('data/Trend.csv')

N <- nrow(X)
u<- matrix(rep(1, N), N, 1)
#Get the column-wise means
means <- t(X) %*% u/N
plot(means)
#Create matrix of means of nrow(X)
means_m <- u %*% t(means)

#Get deviation scores to compute covariance
X_dev <- X - means_m
X_dev
X_dev_2 <- scale(X, scale=FALSE)
X_dev_2
sum(X_dev_2[1,]) #Columns sum to 0
sum(X_dev_2[,1]) #Rows sum to 0

#Get covariance (Sxy = x’y/(N-1))
C <- t(X_dev_2)%*%(X_dev_2)/(N-1)
#Also gives covariance
round(cov(X),4)

df <- nrow(X)-1
####Linear component####

#For review, go over the slide ”Covariance” in the “Correlated Observations” presentation. 
#That shows how to get the variance of a linear combination. 
#Then, if you can find the mean of the linear combination, 
#you can divide the mean by sqrt(variance/N) to get a t
w <- c(-5, -3, -1, 1, 3, 5)
#Get the variance of the contrast
#Pre and post multiplying
varL <- t(w) %*% C %*% w
#Same result as multiplying X by w
varL <- var(X%*%w)
#Get the mean of the linear contrast
L <- w %*%means
#Get the std error of the linear contrast
sL <- sqrt(varL/nrow(X))
sL
t.test.stat <- L/sL
t.test.stat
df <- nrow(X)-1
p <- 2*pt(abs(t.test.stat), df, lower.tail = FALSE)
p

####Quadratic component####

w <- c(5, -1, -4, -4, -1, 5)
varL <- t(w) %*% C %*% w
#Get the mean of the linear contrast
L <- w %*%means
#Get the std error of the linear contrast
sL <- sqrt(varL/nrow(X))
sL
t.test.stat <- L/sL
t.test.stat
p <- 2*pt(abs(t.test.stat), df, lower.tail = FALSE)

p





