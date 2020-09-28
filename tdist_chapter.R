tmp <- c(1,
4,
5,
5,
7,
9,
10,
11,
12,
13,
14,
14,
17,
19,
20,
23,
24,
24,
24,
29)
m <- mean(tmp)
sd <- sd(tmp)
se.mean <- sd(tmp)/sqrt(length(tmp))
t.val <- qt(.975, df=length(tmp)-1)  

m + t.val*se.mean
