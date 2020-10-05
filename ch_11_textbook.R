setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stats)
source('helpful_formulas.R')

y <- c(-2,
1,
3,
2,
-1,
0,
4,
6)
sd(y)

se <- sd(y)/sqrt(length(y))
se

t.stat <-(mean(y)-0)/se
t.stat

p.val <- 2*pt(-abs(t.stat),df=length(y)-1)
p.val

y <- c(1.06,
1.54,
0.31,
1.54,
3.16,
1.37,
0.18,
2.16)

se <- sd(y)/sqrt(length(y))
t.stat <-(mean(y)-0)/se
t.stat

p.val <- 2*pt(-abs(t.stat),df=length(y)-1)
p.val

y<- c(1.00,
-0.64,
0.50,
0.16,
0.91,
-0.85,
-0.73,
1.60)

se <- sd(y)/sqrt(length(y))
t.stat <-(mean(y)-0.5)/se
t.stat

p.val <- 2*pt(-abs(t.stat),df=length(y)-1)
p.val


y <- c(0.30,
1.64,
-2.53,
1.14,
1.46,
2.60,
0.91,
0.56)
se <- sd(y)/sqrt(length(y))
t.stat <-(mean(y)-0.5)/se
t.stat

p.val <- 2*pt(-abs(t.stat),df=length(y)-1)
p.val

t.stat <- 2.34
p.val <- 2*pt(-abs(t.stat),df=(4-1)+(4-1))
p.val

G1<- c(59,
58,
64,
41,
45,
45,
42,
31)

G2 <- c(55,30,34,50,31,47,57,37)
mse <- (var(G1) + var(G2))/2 #Use variance to get mean squared error
se.est <- sqrt((2*mse)/length(G1))

t.stat <- (mean(G1) - mean(G2))/se.est

dof <- length(G1) + length(G2) - 2


