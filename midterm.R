setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
library(DescTools)
source('helpful_formulas.R')

#### QUESTION 2 ####
mt <- read.csv('data/midterm_gpa.csv')

hist(mt$high_GPA, main = 'High school GPA', xlab = 'GPA')
hist(mt$univ_GPA, main = 'University GPA', xlab = 'GPA')

stem(mt$high_GPA)
stem(mt$univ_GPA)

#### QUESTION 3 ####
x <- c(1, 3, 4, 9, 13)

med.x <- median(x)
mean.x <- mean(x)

med.x
mean.x

sum(abs(x-med.x))
sum((x-mean.x)^2)
#Double-check the result
test.abs <- list()
test.sqrd <- list()
for(i in 1:10){
  test.abs[i] <- sum(abs(x-i))
  test.sqrd[i] <- sum((x-i)^2)
}
test.abs
test.sqrd


#### QUESTION 4 ####
#For a normal distribution with a mean of 50 and a standard deviation of 10, 
#what proportion of the distribution is either below 30 or above 65? 
m <- 50
sd <- 10
lower <- normdist.area(30, m, sd, direction='below')
upper <- normdist.area(65, m, sd)
lower+upper

#### QUESTION 5 ####
#Find MAD for the following data: (2, 3, 6, 8, 12, 54, 62)
#Compute median
#For each score, compute abs value of difference btwn score and median
#Compute median of those difference scores
x<- c(2, 3, 6, 8, 12, 54, 62)
m <- median(x)
diffs <- abs(x - m)
median(diffs)

#### QUESTION 7 ####
var1 <- 18
var2 <- 14
n <- 16

mse <- (var1+var2)/2
mse
sqrt((2*mse)/n)
#Same result
sqrt((var1/n)+(var2/n))

#### QUESTION 10 ####
men <- sample(x = 1:10, size  = 10)
women <- sample(x = 1:10, size = 9)
t.test(men,women, var.equal=TRUE)

#### QUESTION 11 ####
df <- read.csv('data/midterm_p11.csv')
library(dplyr)
#setup
g1 <- df %>% 
  dplyr::filter(Condition == 1)
g1 <- g1$y
g2 <- df %>% 
  dplyr::filter(Condition == 2)
g2 <- g2$y
g3 <- df %>% 
  dplyr::filter(Condition == 3)
g3 <- g3$y
g4 <- df %>% 
  dplyr::filter(Condition == 4)
g4 <- g4$y
#Compare 1+3 with 2+4
g.contrast <- c(1,-1,1,-1)

n <- length(g1)
k <- length(g.contrast) 
dof <- k*(n-1)
means <- c(mean(g1),mean(g2),mean(g3),mean(g4))
means
variances <- c(var(g1),var(g2),var(g3),var(g4))
MSE <- mean(variances) 

#Get the SE of the contrast but adjusted for linear contrast coefficients
#Sum of squares of coefficients = g^2
seL <- sqrt(sum(g.contrast*g.contrast*MSE/n))
t.stat <- qt(.975,dof) #assuming 95% CI

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
p.val <- 2*pt(abs(t.stat.test),dof,
              lower.tail=FALSE)
t.stat.test
p.val

#Get the Tukey HSD pairwise comparisons
df$Condition <- as.factor(df$Condition)
res <- aov(df$y~df$Condition)

TukeyHSD(res)

#Compare groups 1, 2, and 3 with group 4
DunnettTest(y ~ Condition, data = df, 
            control=4, conf.level=0.95)

