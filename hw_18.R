setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stats, dplyr, psych, rstatix, readxl, car, haven)

#
# The data in the file "ANOVA 4-way data.xlsxPreview the document" is from a
# 2 x 2 x 2 x 3 four-way design in which Group and Gender are between-subject
# variables and D and T are within-subject variables .
# Do a four-ANOVA on these data and show both univariate and
# multivariate tests tests of the the within-subject effects.
format_f_p <- function(dfn, dfd, f.stat, p){
  str = paste0("F(", dfn, ",", dfd, ") = ", round(f.stat, 2),
               ', p = ', round(p, 3))
  str
}
df <- read_excel('data/ANOVA 4-way data.xlsx')
df['id'] <- factor(1:nrow(df))
#Save as SPSS to compare outputs
#write_sav(df, ".././HW_18.sav")
#Univariate 4-way anova
df_long <- df %>%
  gather(key = "time", value = "score", D1T1, D1T2, D1T3, D2T1, D2T2, D2T3) %>%
  mutate(time = as.factor(time),
         Subject = as.factor(id),
         Gender = as.factor(Gender),
         Group = as.factor(Group),
         Day = as.factor(ifelse(grepl("D1", time), 'D1', 'D2')),
         Trial = as.factor(ifelse(grepl("T1", time), 'T1', 
                        ifelse(grepl("T2", time), 'T2',
                               'T3')))) %>% 
  select(-c(time,id))
#Univariate
res.aov <- anova_test(data = df_long, dv = score, wid = Subject, 
                      within = c(Day,Trial), between=c(Group,Gender),
                      type=3)
aov.table <- get_anova_table(res.aov, correction='HF')
aov.table
aov.table['F statistic'] <- mapply(format_f_p, aov.table$DFn, 
                  aov.table$DFd, aov.table$F, 
                  aov.table$p)

write.csv(aov.table, 'hw_18_univariate_outs.csv')
#Multivariate
df_m <- df
vnames <- paste0(rep('time.', 6), seq(1:6))
colnames(df_m) <- c('Group','Gender', vnames, 'id')

response <- with(df_m, cbind(time.1, time.2, time.3, 
                             time.4, time.5, time.6))
response
mlm1 <- lm(response ~ df_m$Group * df_m$Gender)
mlm1
day_factor <- factor(c("D1", "D1", "D1", "D2", "D2","D2"))
trial_factor <- factor(c("T1", 'T2', 'T3', "T1", 'T2', 'T3'))

options(contrasts = c("contr.poly", 'contr.poly'))
av.ok <- Anova(mlm1, idata=data.frame(day_factor, trial_factor), 
               idesign=~day_factor*trial_factor, type = 'III')

summary(av.ok, multivariate=TRUE)

# Save results
# create new print function
outtests <- car:::print.Anova.mlm

# allow the function to return the results and disable print
body(outtests)[[16]] <- quote(invisible(tests))
body(outtests)[[15]] <- NULL
# Run the Anova over all tests  
tab <- lapply(c("Pillai", "Wilks"), 
              function(i)  outtests(Anova(mlm1, idata=data.frame(day_factor, 
                                                                 trial_factor), 
                                          idesign=~day_factor*trial_factor, 
                                          type = 'III', test.statistic = i)))

tab <- do.call(rbind, tab)
#Create a pretty results table
rnames <- rownames(tab)[1:16]
pil.stats <- tab$`test stat`[1:16]
wilk.stats <- tab$`test stat`[17:nrow(tab)]
result.table <- tab[1:16,]
result.table["Wilk's Lambda"] <- wilk.stats
result.table["Pillai's Trace"] <- pil.stats
result.table <- result.table %>% dplyr::select(-`test stat`)
result.table['F statistic'] <- mapply(format_f_p,
                                      result.table$`num Df`,  
                                      result.table$`den Df`,
                                      result.table$`approx F`,
                                      result.table$`Pr(>F)`)

write.csv(result.table, 'HW_18_multivariate_v1.csv')
#SPSS Syntax to get multivariate tests
# GLM D1T1 D1T2 D1T3 D2T1 D2T2 D2T3 BY Group
# /WSFACTOR=D 2 Polynomial T 3 Polynomial 
# /METHOD=SSTYPE(3)
# /PLOT=PROFILE(D*T*Group) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
# /PRINT=DESCRIPTIVE 
# /CRITERIA=ALPHA(.05)
# /WSDESIGN=D T D*T
# /DESIGN=Group.

