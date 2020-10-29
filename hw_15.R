setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
library(dplyr)
library(multcomp)
library(tidyverse)
library(rstatix)
fake <- data.frame("treatment" = c(rep('therapy', 10),
                                   rep('meds', 10),
                                   rep('control', 10)),
                   "t1" = sample(0:50, 30, replace=TRUE),
                   "t2" = sample(20:50, 30, replace=TRUE),
                   "t3" = sample(30:60, 30, replace=TRUE),
                   "t4" = sample(40:70, 30, replace=TRUE),
                   "id" = c(1:30))
fake_long <- fake %>%
  gather(key = "time", value = "score", t1, t2, t3, t4) %>% 
  mutate(time = as.factor(time),
         treatment = as.factor(treatment),
         id = as.factor(id))
res.aov <- anova_test(data = fake_long, dv = score, wid = id, within = time,
                      between = treatment, type=3)
get_anova_table(res.aov, correction='none')
res.aov2 <- aov(score ~ (treatment*time) + Error(id/(time)), data = fake_long)
print(summary(res.aov2))



df <- read.csv("data/hw_15.csv")
df['id'] <- factor(1:nrow(df))

df_long <- df %>%
  gather(key = "time", value = "score", T1, T2, T3) %>% 
  mutate(time = as.factor(time),
         Subject = as.factor(id))

res.aov <- anova_test(data = df_long, dv = score, wid = Subject, 
                      within = time, type=3)
get_anova_table(res.aov, correction='none')
pwc <- df_long %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "none"
  )
bonf_pval <- 0.01/3
pwc <- pwc %>% 
  mutate(p.sig = ifelse(p < bonf_pval, TRUE,FALSE))
pwc

df <- read.csv("data/hw_15.csv")
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
t.stat.linear <- m/sem

p.linear <- 2*pt(abs(t.stat.linear), dof, lower.tail = FALSE)
t.stat.linear
p.linear

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

p.quadratic <- 2*pt(abs(t.stat.quadratic), dof, lower.tail = FALSE)
t.stat.quadratic
p.quadratic
