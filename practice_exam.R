setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
source('helpful_formulas.R')

a1 <- c(4,5,4,4,7)
a2 <- c(4,4,5,5,6)
a3 <- c(8,8,6,8,9)
b1 <- c(4,7,9,8,9)
b2 <- c(8,5,8,6,6)

g.contrast <- c(1/3, 1/3,1/3,-0.5,-0.5)

n <- length(a1)
k <- length(g.contrast) 
df <- k*(n-1)
means <- c(mean(a1),mean(a2),mean(a3),mean(b1),mean(b2))
means
variances <- c(var(a1),var(a2),var(a3),var(b1),var(b2))
MSE <- mean(variances) 

#Get the SE of the contrast but adjusted for linear contrast coefficients
#Sum of squares of coefficients = g^2
seL <- sqrt(sum(g.contrast*g.contrast*MSE/n))
t.stat <- qt(.975,df) #assuming 95% CI

L <- sum(g.contrast*means)
L

UL <- L+t.stat*seL
LL <-  L-t.stat*seL
#Mean diff:
L

#Lower CI:
LL

#Upper CI:
UL
#Test statistic = statistic/std err of statistic
t.stat.test <- L/seL
#Find the area of the distribution above the t-statistic and multiply by 2 to get both tails
p.val <- 2*pt(abs(t.stat.test),df,
              lower.tail=FALSE)
t.stat.test
p.val


# g1 <- c(3,4,5,6,4)
# g2 <- c(4,5,7,6,8)
# g3 <- c(5,8,9,9,8)
# dat <- cbind(g1,g2,g3)
# test_set <- data.frame('group_values' = cbind(c(g1,g2,g3)), 
#                         'group' = cbind(c(rep('g1',5), rep('g2', 5), rep('g3', 5))))
# test_set$group <- as.factor(test_set$group)
# contrasts(test_set$group)
# check <- aov(test_set$group_values~test_set$group)
# summary(check)
# 
# n <- length(g1)
# # Calculate group means
# means <- c(mean(g1),mean(g2),mean(g3))
# grand_mean <- mean(dat)
# 
# # Calculate the sum of squares
# ss_a <- n*sum((means-grand_mean)^2)
# c.groups <- as.matrix(contrasts(test_set$group))
# mse <- mean(c(var(g1), var(g2), var(g3)))
# c.groups%*%means
# sqrt(t(c.groups) %*% c.groups*mse/n)*3

e <- c(5,5,7,8,5)
ctrl <- c(4,3,2,5,1)
m_e <- mean(e)
var_e <- var(e)
m_e
var_e

m_ctrl<- mean(ctrl)
var_ctrl <- var(ctrl)
m_ctrl
var_ctrl
n <- length(e)
#Get mean standard error
mse <- (var_e+var_ctrl)/2
#Get the standard error of the mean difference
seM <- sqrt((2*mse)/length(e))
seM

df <- 2*(n-1)

diffM <- m_e-m_ctrl
#Get t-value for test statistic
test.stat <- diffM/seM

p.val <- 2*pt(abs(test.stat),df,
              lower.tail=FALSE)
test.stat
p.val

#Stat for CI
t.stat.confint <- confint.mean.diff(e, ctrl)
t.stat.confint

