#Sets working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Formulas that are useful for homework
source('helpful_formulas.R')

#1.	If scores are normally distributed with a mean of 35 and a standard deviation of 10, what percent of the scores is: 
#(a) greater than 34? 
#(b) smaller than 42? 
#(c) between 28 and 34? 

#Q1

m <- 35
sd <- 10

normdist.area(34, m, sd)
normdist.area(42, m, sd, direction = 'below')
normdist.area.between(28, 34, m, sd)

#Q2
m <- 0
sd <- 1
#multiply by 8 and add 75
new.m <- m * 8 + 75
variance <- sd^2
new.variance <- variance * 8
new.sd <- sqrt(new.variance)

#Q4--standard normal dist
m <- 0
sd <- 1

#a. prop w/in 1 SD of the mean:
normdist.area.between(-1, 1, m, sd)

#b. prop more than 2 SD of the mean:
normdist.area.between(-2, 2, m, sd)

#c. prop between 1.25 and 2.1 standard deviations above the mean
normdist.area.between(1.25, 2.1, m, sd)

#Q5
m <- 70
sd <- 8

qnorm(.85, m, sd)
qnorm(.22, m, sd)

#Q6
m <- 70
sd <- 12
tail.area <- (1-0.65)/2

#bottom limit
qnorm(tail.area, m, sd)
#upper limit
qnorm(1 - tail.area, m, sd)
inverse.normdist.between(0.65, m, sd)

#Q7
m <- 20
sd <- 4
#(a) 28 (b) 18 (c) 10 (d) 23
get.zscore(28, m, sd)
get.zscore(18, m, sd)
get.zscore(10, m, sd)
get.zscore(23, m, sd)
#Checking that fxn works--should = 0
get.zscore(m, m, sd)
#Should = 1
get.zscore(m+sd, m, sd)

#Q8
m <- 71
sd <- 8
#prop <= 65 speed limit
normdist.area(65, m, sd, direction='below')
#prop <= 50
normdist.area(50, m, sd, direction='below')*100

#new speed limit to make only 10% of cars > speed limit
qnorm(1-0.1, m, sd)






