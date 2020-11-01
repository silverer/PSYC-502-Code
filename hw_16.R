setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
library(dplyr)
library(multcomp)
library(tidyverse)
library(readxl)
library(rstatix)
source('helpful_formulas.R')

df <- read_excel('data/HW 16.xlsx')
df['id'] <- factor(1:nrow(df))

linear.test <- test.trend(df, c("T1","T2","T3"),
                          return.contr.means = TRUE)
linear.test

df$G <- as.factor(df$G)
t.test(df$L1~df$G, var.equal=TRUE)

#### OLD ####
#All these lines are now performed by `test_trend` function in helpful_formulas.R! 
# df <- df %>% 
#   mutate(L1 = -1*T1 + 0*T2 + 1*T3) #Assign contrast
# 
# df %>% 
#   group_by(id) %>% 
#   dplyr::summarise(subject_mean = mean(L1))
# 
# df_long <- df %>%
#   gather(key = "time", value = "score", T1, T2, T3) %>% 
#   mutate(time = as.factor(time),
#          Subject = as.factor(id))
# 
# X <- as.matrix(df %>% dplyr::select(-c(id,G,L1)))
# w <- c(-1,0,1)
# 
# N <- nrow(X)
# L.linear <- t(colMeans(X)) %*% w
# L.linear
# 
# s_sq <- t(w) %*% cov(X) %*% w
# sem <- sqrt(s_sq/N)
# dof <- nrow(X)-1
# t.stat.linear <- L.linear/sem
# 
# p.linear <- 2*pt(abs(t.stat.linear), dof, lower.tail = FALSE)
# t.stat.linear
# p.linear
