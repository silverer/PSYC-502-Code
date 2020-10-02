setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('helpful_formulas.R')

g1=c(7,8,7,8,9,5)
g2=c(6,5,7,4,5,6)
g3=c(4,6,5,4,7,3)
g4=c(9,8,9,8,7,6)
n = length(g1)
k=4
#Degrees of freedom for the error
dfe=k*(n-1)
means = c(mean(g1), mean(g2), mean(g3), mean(g4))
variances = matrix(c(var(g1), var(g2), var(g3), var(g4)),k,1)
MSE = mean(variances)

#Each column of B is one contrast (so there are 3)
B = rbind(c(1, 1, 1),
          c(1,-1,-1),
          c(-1, 1,-1),
          c(-1,-1,1))

#Multiply coefficients by group means
L = t(B)%*%means
#Get the crossproduct
d = diag(t(B)%*%B)
#Multiply crossproduct by mean squared error to esitmate Std Err of contrasts
seL=sqrt(MSE*d/n)
#Get the t-stat for each of the 3 contrasts
t.stat=L/seL
#Get the p-values for each of the 3 contrasts
p=2*pt(abs(t.stat), dfe, lower.tail = FALSE)
#Get the 
t1=qt(.975,dfe)
UL = L+t1*seL
LL =  L-t1*seL

