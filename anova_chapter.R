setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
source('helpful_formulas.R')

smiles <- read.csv('data/leniency.csv')

smiles$smile <- as.factor(smiles$smile)
library(dplyr)

s1 <- smiles %>% dplyr::filter(smile==1)
s2 <- smiles %>% dplyr::filter(smile==2)
s3 <- smiles %>% dplyr::filter(smile==3)
s4 <- smiles %>% dplyr::filter(smile==4)
mse <- mean(var(s1$leniency),var(s2$leniency),
            var(s3$leniency),var(s4$leniency))

df <- read.csv('data/aov_ch_11.csv')
tmp <- df %>% dplyr::filter(is.na(q4_vals)==FALSE)

means <- tmp %>% 
  group_by(q4_groups) %>% 
  summarise(means = mean(q4_vals), variances = var(q4_vals),
            n = n())
means <- as.data.frame(means)
#MSE = means of variances
mse <- mean(means$variances)
mse
msb <- means$n[1] * var(means$means)
msb

dfn <- nrow(means)-1
dfn
dfd <- nrow(tmp)-nrow(means)
dfd
gm <- mean(tmp$q4_vals)

ssq_error <- dfd*mse
ssq_error
ssq_cond <- dfn * msb
ssq_cond
summary(aov(tmp$q4_vals~tmp$q4_groups))
summary(aov(tmp$q5_vals~tmp$q5_groups))

means <- c(4.5,
7.2,
3.4,
9.1,
1.2)

var(means)

variances <- c(1.33,0.98,1.03,0.78,0.56)
mean(variances)

df.int <- df %>% 
  dplyr::select(c(Age,Cond,Score)) %>% 
  mutate(Age = as.factor(Age),
         Cond = as.factor(Cond),
         Score = as.numeric(Score))
res <- lm(Score~Age*Cond, data = df.int)
summary(aov(res))

