setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stats, dplyr, psych, rstatix, readxl, car)

#### Complex ANOVA slides ####
#Change filepath to where your data is stored. 
df <- read_excel('data/ANOVA 3-way design.xlsx')
df['id'] <- factor(1:nrow(df))
#write_sav(df, "data/complex_anovas.sav")
df_long <- df %>%
  gather(key = "time", value = "score", D1T1, D1T2, D1T3, D2T1, D2T2, D2T3) %>%
  mutate(time = as.factor(time),
         Subject = as.factor(id),
         Group = as.factor(Group),
         Day = as.factor(ifelse(grepl("D1", time), 'D1', 'D2')),
         Trial = as.factor(ifelse(grepl("T1", time), 'T1', 
                                  ifelse(grepl("T2", time), 'T2',
                                         'T3')))) %>% 
  select(-c(time,id))
#mv Documents/dataset_name.sav dataset_name.sav 
#write_sav(df_long, "../complex_anovas_long.sav")


#### 2-way within subjects ANOVA (repeated measures) ####

#This produces the same result as slide 2 of Complex ANOVAs
res.aov <- anova_test(data = df_long, dv = score, wid = Subject, 
                      within = c(Day, Trial), #between=Group,
                      type = 3)
get_anova_table(res.aov, correction = 'HF')

#### 2-way within subjects multivariate approach ####
df_m <- df
vnames <- paste0(rep('time.', 6), seq(1:6))
colnames(df_m) <- c('Group', vnames, 'id')

response <- with(df_m, cbind(time.1, time.2, time.3, 
                             time.4, time.5, time.6))
response
mlm1 <- lm(response ~ 1)
mlm1
day_factor <- factor(c("D1", "D1", "D1", "D2", "D2","D2"))
trial_factor <- factor(c("T1", 'T2', 'T3', "T1", 'T2', 'T3'))
options(contrasts = c("contr.poly", 'contr.poly'))
av.two.within <- Anova(mlm1, idata=data.frame(day_factor, trial_factor), 
                        idesign=~day_factor*trial_factor, type = 'III')

summary(av.two.within, multivariate=TRUE)

#This gives you the same info as above but w/o the Huynh-Feldt correction
#Same SSQ as Day x Trial interaction slide
#he Residuals SSQ column is for Error(Day x Trial)
#Unadjusted degrees of freedom for F ratio are in the table as well
#first deg of freedom is in "Day:Trial" column and second is in "Residuals"
res.mod <- aov(score ~ Day * Trial + Error(Subject/(Day*Trial)),
               data = df_long)

res.mod$`Subject:Day:Trial`

#### ANOVA w/2-within subjects factors and 1 between-subjects factor####
res.aov <- anova_test(data = df_long, dv = score, wid = Subject, 
                      within = c(Day, Trial), between=Group,
                      type=3)
get_anova_table(res.aov, correction='HF')
#### Multivariate approach: 2 w/in subj factors and 1 between-subjects factor ####
df_m <- df
vnames <- paste0(rep('time.', 6), seq(1:6))
colnames(df_m) <- c('Group', vnames, 'id')

response <- with(df_m, cbind(time.1, time.2, time.3, 
                              time.4, time.5, time.6))
response
mlm1 <- lm(response ~ df_m$Group)
mlm1
day_factor <- factor(c("D1", "D1", "D1", "D2", "D2","D2"))
trial_factor <- factor(c("T1", 'T2', 'T3', "T1", 'T2', 'T3'))
options(contrasts = c("contr.poly", 'contr.sum'))
av.ok <- Anova(mlm1, idata=data.frame(day_factor, trial_factor), 
               idesign=~day_factor*trial_factor, type = 'III')

tmp <- summary(av.ok, multivariate=TRUE)
tmp1 <- tmp$multivariate.tests
for(n in names(tmp1)){
  print('-------------------------------------------')
  print(tmp1[n])
  print('-------------------------------------------')
}

#SPSS Syntax to get multivariate tests
# GLM D1T1 D1T2 D1T3 D2T1 D2T2 D2T3 BY Group
# /WSFACTOR=D 2 Polynomial T 3 Polynomial 
# /METHOD=SSTYPE(3)
# /PLOT=PROFILE(D*T*Group) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
# /PRINT=DESCRIPTIVE 
# /CRITERIA=ALPHA(.05)
# /WSDESIGN=D T D*T
# /DESIGN=Group.

