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
summary(aov(df$q4_vals~df$q4_groups))
summary(aov(df$q5_vals~df$q5_groups))

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

