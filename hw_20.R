setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stats, dplyr, psych, rstatix, readxl, effsize)

df <- read_excel('data/ratings.xls')
df['Condition'] <- as.factor(df$Condition)

effsize::cohen.d(Rating~Condition, data = df)
effsize::cohen.d(Rating~Condition, data = df, hedges.correction = TRUE)


res.aov <- aov(Rating~Condition, data = df)
aov.sum <- anova(res.aov)
ssq_total <- aov.sum$`Sum Sq`[1] + aov.sum$`Sum Sq`[2]
ssq_total
eta_s <- aov.sum$`Sum Sq`[1] / ssq_total
eta_s
#ω2 = (SSeffect – (dfeffect)(MSerror)) / MSerror + SStotal
mse <- aov.sum$`Mean Sq`[2]
omega.sq <- (aov.sum$`Sum Sq`[1]-aov.sum$Df[1]*mse)/(mse + ssq_total)
omega.sq

#hedges' g = (M1-M2)/sqrt(MSE)
#cohen's d = g * sqrt(N/(N-2))
m.diff <- t.test(df$Rating~df$Condition, var.equal = TRUE)
m.diff <- m.diff$estimate[1] - m.diff$estimate[2]
m.diff <- unname(m.diff)
m.diff

hedges <- m.diff/sqrt(mse)
hedges

cohen <- hedges *sqrt(nrow(df)/(nrow(df)-2))
cohen

#cohens_d(df, Rating~Condition, hedges.correction=FALSE)

#### Textbook example: Cohens d and Hedges g ####
animal <- read_excel("data/animals.xls")
animal['GENDER'] <- as.factor(animal$GENDER)
aov.sum <- anova(aov(WRONG~GENDER, animal))
aov.sum
mse <- aov.sum$`Mean Sq`[2]
m.diff <- t.test(animal$WRONG~animal$GENDER, var.equal = TRUE)
m.diff <- m.diff$estimate[1] - m.diff$estimate[2]
m.diff <- unname(m.diff)
m.diff

g <- m.diff/sqrt(mse)
#### Textbook example ####
lncy <- read_excel('data/leniency.xls')
lncy['smile'] <- as.factor(lncy$smile)
res.aov <- aov(leniency~smile, data = lncy)
aov.sum <- anova(res.aov)
ssq_total <- aov.sum$`Sum Sq`[1] + aov.sum$`Sum Sq`[2]
ssq_total
eta_s <- aov.sum$`Sum Sq`[1] / ssq_total
eta_s
#ω2 = (SSeffect – (dfeffect)(MSerror)) / MSerror + SStotal
mse <- aov.sum$`Mean Sq`[2]
omega.sq <- (aov.sum$`Sum Sq`[1]-aov.sum$Df[1]*mse)/(mse + ssq_total)
omega.sq
