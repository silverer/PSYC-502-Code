#Sets working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Formulas that are useful for homework
source('helpful_formulas.R')

#3. m1 = 5, m2 = 4, std = 1.5, n = 12 what's the prob that m1 - m2 >= 1.5?
se.means <- sqrt(((2*1.5^2)/12))
se.means

mean.diff <- 1
normdist.area(1.5, mean.diff, se.means)


#8. m = 20, std = 10, what's the prob that 2 nums will have mean diff > 5?
se.means <- std.err.meandiff(10^2, 10^2, 1, 1)
se.means

sd.meandiff <- sqrt(100*2)
sd.meandiff

#The mean GPA for students in School A is 3.0; the mean GPA for students 
#in School B is 2.8. The standard deviation in both schools is 0.25. 
#The GPAs of both schools are normally distributed. 
#If 9 students are randomly sampled from each school what's the prob that:

#the sample mean for School A will exceed that of School B by 0.5 or more
se.means <- std.err.meandiff(0.25^2, 0.25^2, 9, 9)
se.means
mean.diff <- 3.0 - 2.8
mean.diff

normdist.area(0.5, mean.diff, se.means)*100
mean=mean.diff; sd=se.means
lb=0.5; ub=30

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="Mean diffs", ylab="",
     main="Normal Distribution", axes=FALSE)
i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Mean diff <",ub,") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(mean.diff - (se.means*4), mean.diff + (se.means*4), se.means), pos=0)

#the sample mean for School B > School A
mean.diff <- 2.8-3.0

normdist.area(0, mean.diff, se.means, 
              direction='above')

mean.diff <- 3.0-2.8

normdist.area(0, mean.diff, se.means, 
              direction='below')
#-.2, .118, 
mean=mean.diff; sd=se.means
lb=-.01; ub=30

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="IQ Values", ylab="",
     main="Normal Distribution", axes=FALSE)
i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Mean diff <",ub,") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(mean.diff - (se.means*4), mean.diff + (se.means*4), se.means), pos=0)

