
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('helpful_formulas.R')


#Question 18--find 95% and 90% CIs
vals <- c(2, 1.5, 3, 2, 3.5, 1, 0.5, 3, 2, 4)
# A.
m <- mean(vals)
m
std <- sd(vals)
std


sm <- std/sqrt(length(vals))
sm
df <- length(vals)-1
df

t.stat <- qt(.975, df=length(vals)-1)
t.stat

error <- qt(0.975,df=length(vals)-1)*sd(vals)/sqrt(length(vals))
error

t.stat*sm

ll <- m - t.stat*sm
ll

ul <- m + t.stat*sm
ul
#B. 
t.stat <- qt(.95, df)
t.stat

ll <- m - t.stat*sm
ll

ul <- m + t.stat*sm
ul

#25.

am <- read.csv('./data/angry_moods.csv')

#Calculate the 95% confidence interval for the difference between the 
#mean Anger-In score for the athletes and non-athletes
colnames(am)
library(dplyr)
#1 = athletes, 2 = non-athletes
sports <- am %>% 
  dplyr::filter(Sports == 1)

non.sports <- am %>% 
  dplyr::filter(Sports == 2)
#Different n's
nrow(non.sports)
nrow(sports)

m1 <- mean(sports$Anger.In)
m2 <- mean(non.sports$Anger.In)

harm.mean <- 2/((1/nrow(sports))+(1/nrow(non.sports)))
df <- (nrow(sports) -1) + (nrow(non.sports) -1)
sse <- sum((sports$Anger.In - m1)^2) + sum((non.sports$Anger.In - m2)^2)
mse <- (sse/df)
se.mdiff <- sqrt((2*mse)/harm.mean)

m.diff <- m1-m2
ll <- m.diff - qt(0.975, df)*se.mdiff
ul <- m.diff + qt(0.975, df)*se.mdiff
ll
ul


#R matrix operations/contrasts

g1 <- c(2.5,5.5,6.5,3.5,3,3.5,6,5,4,4.5,5,5.5,3.5,6,6.5,3,8,6.5,8,6,6,3,7,8,4,3,2.5,8,4.5,5.5,7.5,6,9,6.5)

g2 <- c(7,3,6,4.5,3.5,4,3,3,3.5,4.5,7,5,5,7.5,2.5,5,5.5,5.5,5,4,5,6.5,6.5,7,3.5,5,3.5,9,2.5,8.5,3.5,4.5,3.5,4.5)

g3 <- c(5.5,4,4,5,6,3.5,3.5,3.5,4,5.5,5.5,4.5,2.5,5.5,4.5,3,3.5,8,5,7.5,8,4,5.5,6.5,5,4,3,5,4,4,6,8,4.5,5.5)

g4 <- c(2,4,4,3,6,4.5,2,6,3,3,4.5,8,4,5,3.5,4.5,6.5,3.5,4.5,4.5,2.5,2.5,4.5,2.5,6,6,2,4,5.5,4,2.5,2.5,3,6.5)


g.contrast <- c(1/3, 1/3,1/3,-1)

n <- length(g1)
k <- length(g.contrast)
df <- k*(n-1)
means <- c(mean(g1),mean(g2),mean(g3),mean(g4))
means
variances <- matrix(c(var(g1),var(g2),var(g3),var(g4)),k,1)
MSE <- mean(variances)
a <- matrix(g.contrast,k,1)
a
seL <- sqrt(t(a)%*%a*MSE/n)
t.stat <- qt(.975,df) #assuming 95% CI
L <- t(a)%*%means
L
#Produces L in a more roundabout way
(1/3)*mean(g1)+(1/3)*mean(g2)+(1/3)*mean(g3)+(-1*mean(g4))

UL <- L+t.stat*seL
LL <-  L-t.stat*seL
seL
print('Mean diff:')
L

print('Lower CI:')
LL

print('Upper CI:')
UL


