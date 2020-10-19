setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
source('helpful_formulas.R')
library(dplyr)


df <- read.csv('data/hw_13.csv')
df <- df %>% 
  mutate(a_group = as.factor(a_group),
         b_group = as.factor(b_group))
res <- lm(score~a_group*b_group, data = df)
summary(aov(res))
TukeyHSD(aov(res))

interaction.plot(df$b_group, df$a_group, df$score)
interaction.plot(df$a_group, df$b_group, df$score)


b1 <- df %>% dplyr::filter(b_group==1) %>% dplyr::select(score)
b2 <- df %>% dplyr::filter(b_group==2)%>% dplyr::select(score)
b3 <- df %>% dplyr::filter(b_group==3)%>% dplyr::select(score)
b4 <- df %>% dplyr::filter(b_group==4)%>% dplyr::select(score)
X <- as.data.frame(c(b1,b2,b3,b4))
colnames(X) <- c('score.b1', 'score.b2', 'score.b3', 'score.b4')
N <- nrow(X)
u<- matrix(rep(1, N), N, 1)
#Get the column-wise means
means <- t(X) %*% u/N
C <- round(cov(X),4)
dof <- nrow(X)-1

w <- c(-3, -1, 1, 3)
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
dof <- nrow(X)-1
p <- 2*pt(abs(t.test.stat), dof, lower.tail = FALSE)
p



