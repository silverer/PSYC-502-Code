setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
library(dplyr)
library(multcomp)


df <- read.csv('data/hw_13.csv')
df <- df %>% 
  mutate(a_group = as.factor(a_group),
         b_group = as.factor(b_group))
res <- lm(score~a_group*b_group, data = df)
summary(aov(res))
TukeyHSD(aov(res))

interaction.plot(df$b_group, df$a_group, df$score)

##22c
df <- df %>% 
  mutate(b_text = paste0('b', b_group),
         a_text = paste0('a', a_group),
         grp_text = paste0(a_text,b_text))
df$grp_text<-as.factor(df$grp_text)


K <- c(-.75,-.25,.25,.75,
       -.75,-.25,.25,.75)
mod <- lm(score~grp_text, data=df)
modSummary <- summary(mod)
modSummary
modPlanned <- glht(mod, linfct = mcp(grp_text = K))
#Dr. Lane's Est = 4.0625 and Std Err = 0.5502
#My estimates are exactly twice as big? Not sure why.
#I did get the same t stat and p-value
summary(modPlanned)

