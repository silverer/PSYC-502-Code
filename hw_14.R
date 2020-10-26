setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
library(dplyr)
library(multcomp)

df <- read.csv("data/polynomial trend problem.csv")

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
plot(means)
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
res2 <- aov(Y~Drug, data=df)
fit.res2 <- glht(res2, linfct = mcp(Drug = c(1/2, -1, 1/2)))
summary(fit.res2)
# since drug has 2 df and condition has 1 df: 1 x 2 = 2
# k(n-1) = 6*(2-1) = 6...?
#df1 <- 4 - 1 #comparing 4 means
#df2 <- 4 * (4-1)
#df1
#df2
df1 <- 3 - 1 #comparing 3 means
df2 <- 3 * (4-1)
#https://stat.ethz.ch/~meier/teaching/anova/contrasts-and-multiple-testing.html#scheff%C3%A9
p_scheffe <- pf((summary(fit.scheffe)$test$tstat)^2 / df1, df1, df2, lower.tail = FALSE)
p_scheffe
f_stat_scheffe <- qf(p_scheffe, df1, df2, lower.tail=FALSE)
f_stat_scheffe

drug_ssq <- anova(res)$`Sum Sq`[1]
drug_ssq
msb <- drug_ssq/df1
msb
mse <- anova(res)$`Mean Sq`[2]
mse
#idk why this is giving me a different answer than line 83
f_val <- msb/mse
f_val
p_scheffe <- pf(f_val, df1, df2, lower.tail = FALSE)
p_scheffe

#### OLD/experimental ####

library(agricolae)
comparison <- scheffe.test(fit.scheffe$model,"drug_by_cond", group=TRUE,console=TRUE,
                           main="test")

#MSB = SSQ/dfn 
msb <- SSQ/df1
msb
mse <- 0.667
means <- as.data.frame(df %>% 
                         group_by(Drug) %>% 
                         dplyr::summarise(mean_int = mean(Y)))

K <- c(1/4,1/4,
       -1/2,-1/2, 
       1/4,1/4)
n <- 4
means <- means$mean_int
#means_contr <- means * K
gm <- mean(means) 
a <- means-gm 
L <- t(a) %*% means
L
SSQ <- 4*L^2/t(a)%*%a 
SSQ
ssq_e <- anova(res1)$`Sum Sq`[2]
dfn <- anova(res1)$Df[2]
dfn

dfd <- anova(res1)$Df[2]*(nrow(df %>% dplyr::filter(drug_by_cond=='AC'))-1)
dfd

dfn <- (3 - 1) + (2 - 1)
dfn
dfd <- g * nrow(df %>% dplyr::filter(drug_by_cond=='AC'))-1
dfd
msb <- ssq_e/dfn
library(reshape2)
df['id'] <- rownames(df)
df_melt <- melt(df, value.name='value', id.vars=c('id'))
df_melt <- df_melt %>% 
  mutate(variable_numeric = ifelse(variable=='T1', 1,
                                   ifelse(variable=='T2', 2, 3)))
w <- c(-1,0,1)
df_melt['linear_weights'] <- c(rep(w[1], 5), rep(w[2], 5), rep(w[3], 5))

w <- c(1, -2, 1)
df_melt['quadratic_weights'] <- c(rep(w[1], 5), rep(w[2], 5), rep(w[3], 5))
#https://crumplab.github.io/statistics/repeated-measures-anova.html
grand_mean <- mean(df_melt$value)
grand_mean

df_melt['diff'] <- df_melt['value'] - grand_mean
df_melt['diff_sq'] <- df_melt$diff^2
ss_total <- sum(df_melt$diff_sq)
ss_total
time_means <- as.data.frame(df_melt %>% 
  group_by(variable) %>% 
  dplyr::summarise(condition_mean = mean(value)))
df_melt <- dplyr::left_join(df_melt, time_means, by = 'variable')

df_melt['diff_gr_gm'] <- df_melt$condition_mean-grand_mean
ss_effect <- sum(df_melt['diff_gr_gm']^2)
ss_error_within <- ss_total-ss_effect

subj_means <-as.data.frame(df_melt %>% 
                             group_by(id) %>% 
                             dplyr::summarise(subj_mean = mean(value))) 
df_melt <- dplyr::left_join(df_melt, subj_means, by = 'id')
df_melt['diff_sub_gm'] <- df_melt$subj_mean-grand_mean
ss_err_subj <- sum(df_melt['diff_sub_gm']^2)
ss_err_rem <- ss_error_within-ss_err_subj
mse_eff <- ss_effect/2 #3 times - 1
mse_rem <- ss_err_rem/((5-1)*(3-1))

lm01 <- lm(value ~ id+ linear_weights, data = df_melt)
anova(lm01)

linear <- c(-1,0,1)
quadratic <- c(1, -2, 1)

# estimate (mean difference)
linest <- mean(as.matrix(df[-4]) %*% linear)
quadest <- mean(as.matrix(df[-4]) %*% quadratic)
est <- c(linest, quadest)

# number of participants
n <- nrow(df)

# sum of squares for the contrast
linssc <- n * linest^2 / sum(linear^2)
quadssc <- n * quadest^2 / sum(quadratic^2)
ssc <- c(linssc, quadssc)

# mean squared error
df_melt$id <- as.factor(df_melt$id)
test <- df_melt
test$linear_weights <- as.factor(test$linear_weights)
test$quadratic_weights <- as.factor(test$quadratic_weights)
test$id <- as.factor(test$id)
test$variable <- as.factor(test$variable)
Anova(lm(value~linear_weights, data=df_melt),type='III')
mse <- anova(lm(value ~ linear_weights + quadratic_weights + id, data = df_melt))$`Mean Sq`[4]

# F-ratio
fratio <- ssc / mse

# number of levels (i.e., timepoints)
nlvls <- nlevels(as.factor(df_melt$variable))

# degrees of freedom
df1 <- 1
df2 <- (nlvls - 1) * (n - 1)

# p-value
p <- 1 - pf(q = fratio, df1 = df1, df2 = df2)

# alpha
alpha <- 0.05

# F critical
fcrit <- qf(p = 1 - alpha, df1 = df1, df2 = df2)

# standard error
linse <- sqrt(mse * sum(linear^2 / n))
quadse <- sqrt(mse * sum(quadratic^2 / n))
se <- c(linse, quadse)

# lower and upper bound
lwr <- c(linest, quadest) - sqrt(fcrit) * se
upr <- c(linest, quadest) + sqrt(fcrit) * se

# table of results
tibble(est, se, fratio, p, lwr, upr)
