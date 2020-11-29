setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stats, dplyr, psych, rstatix, car, multcomp, emmeans, lme4)

df <- read.csv('data/final_prep.csv')
#### Question 9 ####
tmp <- df %>% 
  dplyr::select(starts_with('q9')) %>% 
  dplyr::mutate(subject = as.factor(1:nrow(df))) %>% 
  
tmp <- dplyr::rename_with(tmp, ~gsub("q9_", "", .x, fixed = TRUE))

tmp_long <- tmp %>% 
  gather(key = 'trial', value = 'score', Trial.1, Trial.2, Trial.3) %>% 
  dplyr::mutate(trial = as.factor(trial),
                Group = as.factor(Group),
                subject = as.factor(subject))
#For the following data, Group is a between-subjects variable and 
#Trials is a within-subjects variable. Do a Groups x Trials ANOVA 
#and report the F, df, and p for Groups, Trials, and the Groups x 
#Trials interaction using the Huynh-Feldt correction if applicable). 
#Use the Tukey hsd to test differences among means for “Group.”
res.aov <- anova_test(data = tmp_long, dv = score, wid = subject, 
                      within = trial, between=Group,
                      type = 3)
get_anova_table(res.aov, correction = 'HF')

tmp <- tmp %>% 
  dplyr::mutate(trial.sum = Trial.1 + Trial.2 + Trial.3)
# library(haven)
# write_sav(tmp, ".././test_final_question9.sav")
#Gives same p-values as SPSS post-hoc test but different estimates of mean diffs
TukeyHSD(aov(trial.sum~Group, data = tmp))
#This gives same p-values and estimates of mean diffs as SPSS
mod1<-aov(score ~ Group * trial + Error(subject/trial),
          data = tmp_long)
emmeans(mod1, pairwise~Group)
#spss syntax: 
# GLM Trial.1 Trial.2 Trial.3 BY Group
# /WSFACTOR=trial 3 Polynomial 
# /METHOD=SSTYPE(3)
# /POSTHOC=Group(TUKEY) 
# /CRITERIA=ALPHA(.05)
# /WSDESIGN=trial 
# /DESIGN=Group.
#### Question 4 ####
tmp <- df %>% 
  dplyr::select(starts_with('A')|starts_with('B'))
tmp <- drop_na(tmp)
tmp_long <- tmp %>% 
  gather(key = 'medication', value = 'score', A1, A2, A3, B1, B2)
# library(haven)
# write_sav(tmp_long, "../test_final_question.sav")
cont <- c(-1, -1, -1, 1.5, 1.5)
means <- c(mean(tmp$A1), mean(tmp$A2), mean(tmp$A3), 
           mean(tmp$B1), mean(tmp$B2))

grand.mean <- mean(tmp_long$score)
deviations <- means - grand.mean
s_sq <- deviations ^ 2
p1 <- means * cont
l <- sum(p1)
l_sq <- l^2
nlsq <- (nrow(tmp)*l_sq)/crossprod(cont, cont)
mse <- mean(c(var(tmp$A1), var(tmp$A2), var(tmp$A3), 
              var(tmp$B1), var(tmp$B2)))
f_stat <- nlsq/mse
sqrt(f_stat) #Gives same t-value as contrast test in SPSS

dfn <- 1
dfd <- ncol(tmp) * (nrow(tmp)-1)

p_val <- 1 - pf(f_stat, dfn, dfd) #Gives same p-value as contrast test in SPSS
p_val

tmp_long$medication <- as.factor(tmp_long$medication)
mod <- aov(score~medication, data = tmp_long)
cont.test <- glht(mod, linfct = mcp(medication = cont))
summary(cont.test) #Gives same Estimate, SE, t-value, and p-value as contrast test in SPSS

tmp_long['med.contr'] <- tmp_long$medication
contrasts(tmp_long$med.contr) <- cont
#Gives same results as SPSS ANOVA table with same contrasts
res <- aov(score~med.contr, data = tmp_long)
Anova(res, type = 'III')

#### Question 6 ####
tmp <- df %>% 
  dplyr::select(starts_with('q6'))
tmp <- drop_na(tmp)
df_long <- tmp %>%
  dplyr::select(starts_with('q6')) %>% 
  gather(key = "group", value = "score", q6_g1, q6_g2, q6_g3) %>%
  mutate(group = as.factor(ifelse(grepl('_g1', group), 'Group 1',
                                        ifelse(grepl('_g2', group), 'Group 2',
                                                     'Group 3')))
  )

res <- aov(score~group, data = df_long)
Anova(res, type = 'III')
#get the means for each group
group.means <- c(mean(tmp$q6_g1), mean(tmp$q6_g2),
                 mean(tmp$q6_g3))
#get the grand mean
grand.mean <- mean(df_long$score)
#get the deviations of group means from grand mean 
deviations <- group.means - grand.mean
s_sq <- deviations^2
#multiply n per group by sum of squared deviations
nrow(df)*sum(deviations^2)

c1 <- c(1, 1, -2)
c2 <- c(1, -1, 0)

p1 <- group.means * c1
p2 <- group.means * c2

L1 <- sum(p1)
L1_sq <- L1^2
L2 <- sum(p2)
L2_sq <- L2^2
num.rows <- nrow(tmp)
#(n*L^2)/c'c
ssq_1 <- (num.rows*L1_sq)/crossprod(c1, c1)
ssq_2 <- (num.rows*L2_sq)/crossprod(c2, c2)
ssb <- ssq_1 + ssq_2
ssb

#### Question 11 ####
means <- c(6, 10, 12)
variances <- c(14, 16, 18)
ns <- c(12, 12, 12)
#Simulate data w same statistics to make sure the manual calculations line up
g1 <- mvrnorm(n = 12, c(means[1]), c(variances[1]), empirical=TRUE)
g2 <- mvrnorm(n = 12, c(means[2]), c(variances[2]), empirical=TRUE)
g3 <- mvrnorm(n = 12, c(means[3]), c(variances[3]), empirical=TRUE)
tmp_sim <- data.frame(g1, g2, g3)
tmp_sim <- tmp_sim %>% 
  gather(key = 'group', value = 'score', g1, g2, g3)

res.aov <- aov(score~group, data = tmp_sim)
Anova(res.aov, type = 'III')

#Perform the manual calculations
grand.mean <- mean(means)
deviations <- means - grand.mean
dev_sq <- deviations^2
ssb <- ns[1]*sum(dev_sq)
msb <- ssb / (length(means)-1)
mse <- mean(variances)
f_stat <- msb/mse
dfn <- (length(means)-1)
dfd <- length(means) * (ns[1]-1)

p_val <- 1 - pf(f_stat, dfn, dfd) #Gives same p-value as contrast test in SPSS
p_val

#### Question 8 ####
library(pwr)
co.d <- 5/10
pwr.t.test(d = co.d, power = 0.80, sig.level = 0.05)#Same result as GPower
var1 <- 100
var2 <- 100
sd1 <- 10
sd2 <- 10
cor.within <- 0.58
diff <- 5
#Get the variance of the difference scores
var.diff <- var1 + var2 - (2 * cor.within * sd1 * sd2)
sd.diff <- sqrt(var.diff)
co.d <- diff/sd.diff #Get the effect size
pwr.t.test(d = co.d, power = 0.8, type = 'paired',
           alternative = 'two.sided') #same result as GPower

# Sigma = matrix(c(10,3,3,2),2,2) 
# means = c(4,6) 
# d1 = mvrnorm(n=25, means, Sigma, empirical = TRUE) 
# means2 = c(5,8) 
# d2= mvrnorm(n=25, means2, Sigma, empirical = TRUE) 
# d=rbind(d1,d2) 
# g=c(rep(1,25),rep(2,25)) 
# d=cbind(g,d) 



