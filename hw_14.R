setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
library(dplyr)
library(multcomp)

df <- read.csv("data/polynomial trend problem.csv")

#res.aov <- aov(score ~ time + Error(id/(time)), data=df_long)
#linear.test <- glht(res.aov$`id:time`, linfct = mcp(time = c(-1,0,1)))

save <- FALSE
if(save){
  library(haven)
  write_sav(df, 'trend_problem_hw_14.sav')
}
#### Linear component ####
#weights are from: http://davidmlane.com/hyperstat/B114359.html
X <- as.matrix(df)
w <- c(-1,0,1)

N <- nrow(X)
u <- matrix(c(1,1,1,1,1),5,1)
means_m <- t(X) %*% u /N
means_m
m <- t(w) %*% means_m
m
col_means <- colMeans(X)
L.linear <- t(col_means) %*%w
L.linear

s_sq <- t(w) %*% cov(X) %*% w
sem <- sqrt(s_sq/N)
dof <- nrow(X)-1
#plot(means)
t.stat.linear <- m/sem
t.stat.linear
p.linear <- 2*pt(abs(t.stat.linear), dof, lower.tail = FALSE)
p.linear

#### Quadratic component ####

w <- c(1, -2, 1)
N <- nrow(X)
u <- matrix(c(1,1,1,1,1),5,1)
means_m <- t(X) %*% u /N
means_m
m <- t(w) %*% means_m
m
col_means <- colMeans(X)
L.quadratic <- t(col_means) %*%w
L.quadratic

s_sq <- t(w) %*% cov(X) %*% w
sem <- sqrt(s_sq/N)
dof <- nrow(X)-1
plot(means)
t.stat.quadratic <- m/sem
t.stat.quadratic
p.quadratic <- 2*pt(abs(t.stat.quadratic), dof, lower.tail = FALSE)
p.quadratic

res1 <- aov(Y~drug_by_cond, data=df)
fit.scheffe <- glht(res1, linfct = mcp(drug_by_cond = c(1/4,1/4,
                                                        -1/2,-1/2, 
                                                        1/4,1/4)))

#### Problem 2 ####
df <- read.csv('data/two-way.csv', stringsAsFactors = TRUE)
df <- df %>% 
  mutate(drug_by_cond = as.factor(paste0(Drug, Condition)))
res <- lm(Y~Drug*Condition, data=df)
anova(res)
TukeyHSD(aov(res))

library(multcomp)

res1 <- aov(Y~drug_by_cond, data=df)
fit.scheffe <- glht(res1, linfct = mcp(drug_by_cond = c(1/4,1/4,
                                                        -1/2,-1/2, 
                                                        1/4,1/4)))

summary(fit.scheffe)
# since drug has 2 df and condition has 1 df: 1 x 2 = 2
#df1 <- 4 - 1 #comparing 4 means
#df2 <- 4 * (4-1)
#df1
#df2
df1 <- 3 - 1 #comparing 3 means
df2 <- df1 * (4-1)
#https://stat.ethz.ch/~meier/teaching/anova/contrasts-and-multiple-testing.html#scheff%C3%A9
p_scheffe <- pf((summary(fit.scheffe)$test$tstat)^2 / df1, df1, df2, lower.tail = FALSE)
p_scheffe
f_stat_scheffe <- qf(p_scheffe, df1, df2, lower.tail=FALSE)
f_stat_scheffe
