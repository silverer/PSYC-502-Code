library(stats)
library(ggplot2)
library(plotly)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('data_paths.R')

df <- read.csv(paste0(data, 'hw_2_q_3.csv'))
df$Group <- factor(df$Group,
                  levels = c('Non-players','Beginners',
                             'Tournament players'),ordered = TRUE)

p <- ggplot(df, aes(x = Group, y = Score)) +
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", 
              shape=3, size=3,show_guide = FALSE)
ggplotly(p)

adhd <- read.csv(paste0(data,'adhd.csv'))
p <- ggplot(adhd, aes(x = Dose, y = Score)) +
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", 
               shape=3, size=3,show.legend = FALSE)
ggplotly(p)

sat <- read.csv(paste0(data, 'sat.csv'))
n_bins <- 2*nrow(sat)^(1/3)
n_bins

p <- ggplot(sat, aes(high_GPA))+
  geom_histogram(bins = 11)+
  xlab("High School GPA")
print(p)

stem(sat$high_GPA)

p <- ggplot(sat, aes(univ_GPA))+
  geom_histogram(bins = 11)+
  xlab("University GPA")
print(p)

stem(sat$univ_GPA)


