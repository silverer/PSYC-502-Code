setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stats, dplyr, psych, rstatix, readxl, car)
source("helpful_formulas.R")

df <- read_excel('data/fusion.xlsx')
head(df)
#### A ####
res <- t.test(df$Time~df$Condition, var.equal=TRUE)
res

#### B ####
df['log_time'] <- log10(df$Time)

res <- t.test(df$log_time~df$Condition, var.equal=TRUE)
res
new.ll <- 10^res$conf.int[1]
new.ul <- 10^res$conf.int[2]
new.ll
new.ul

g1 <- df %>% filter(Condition == 1)
g2 <- df %>% filter(Condition == 2)
m.ratio <- mean(g1$log_time) - mean(g2$log_time)
m.ratio <- 10 ^ m.ratio
m.ratio

result <- paste0("t(", res$parameter, ') = ',
                 round(res$statistic, 2), ', p = ',
                 round(res$p.value, 3), '. Ratio between geometric means = ',
                 round(m.ratio, 2), ' (95% CI: ',
                 round(new.ll, 2), ', ', round(new.ul, 2), ')')
print(result)

#Calculate t-stat manually

#Use harmonic mean since the n's are different
harm.mean <- 2/((1/nrow(g1))+(1/nrow(g2)))
df <- (nrow(g1) -1) + (nrow(g2) -1)
sse <- sum((g1$log_time - m1)^2) + sum((g2$log_time - m2)^2)
mse <- (sse/df)
se.mdiff <- sqrt((2*mse)/harm.mean)

m.diff <- m1-m2
t.stat.test <- m.diff/se.mdiff
t.stat.test
ll <- m.diff - qt(0.975, df)*se.mdiff
ul <- m.diff + qt(0.975, df)*se.mdiff
ll
ul
