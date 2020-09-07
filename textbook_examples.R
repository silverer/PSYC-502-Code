#Set directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
library(psych)
#Source filepath script
source('data_paths.R')
#Source function script
source('helpful_formulas.R')

#Matrix operations from Lecture Sept 1
x <- matrix(c(1,2,4,5), 4, 1)

matrix.stats(x)

u <- rep(1,4)#Multiplier

sum <- t(x) %*% u
mean <- sum/length(x)
means <- matrix(rep(mean, 4), 4, 1)
d <- x - means #Deviation scores

#Transpose deviations and multiply by itself (e.g., square it) and sum them together
sum.squares <- t(d) %*% d #%*% sums the product, whereas * just returns the cell-wise product
d %*% d #Doesn't work because it expects (j*k) and (k*j) elements

variance <- sum.squares/(length(x)-1)
std.dev <- sqrt(variance)

#Chapter 3 demonstrations
A0 <- c(2,4,5,6,6,6,6,10,10,10,10,10,12,12,12,12,12)

D0<- c(2, 3,4,4,9,10,16,10,8,10,11,12,13,22,23,24,25)

percentile.formula(D0, .75)

heights <-   c(1,
3,
4,
5,
7,
9,
10,
12,
14,
22)
percentile.formula(heights, .25)

y <- c(9,
       12,
       9,
       13,
       9,
       14,
       16,
       6)


mean <- sum(y)/length(y)
means <- rep(mean, length(y))
var <- variance(y)

z <- c(12,
       13,
       14,
       15,
       9,
       10,
       16,
       10,
       8,
       10,
       11,
       12,
       13,
       22,
       23,
       24,
       25)

z <- sort(z)
percentile.formula(z, .25)
percentile.formula(z, .75)

# HW 4
# Using R matrix operations,
# 
# Show that the sum of squares of the following numbers is 45. (2, 4, 5)
# 
# Create a matrix 3 x 1 matrix with these numbers (2, 4, 5) and a 3 x 1 matrix with these numbers (2, 7, 0). Use R to subtract the second from the first resulting min a 3 x 1 matrix with the following elements:
#   
#   0,
# 
# -3,
# 
# 5

x <- matrix(c(2, 4, 5), 3, 1)
x
x.ssq <- t(x) %*% x
x.ssq

y <- matrix(c(2, 7, 0), 3, 1)
x-y

mean.x <- x.sum/length(x)
means <- matrix(rep(mean.x, length(x)), length(x), 1)

devs <- x - means
ssq <- t(devs) %*% devs

