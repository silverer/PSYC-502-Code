
#Nums: vector containing observed values
geom.mean <- function(nums){
  product = prod(nums)
  return(product^(1/3))
}

#Nums: vector containing observed values
#Percentile: desired percentile rank (e.g., 0.25 for 25th percentile)
percentile.formula <- function(nums, percentile){
  rank.raw = percentile * length(nums)+1
  nums = sort(nums)
  ir = as.integer(rank.raw)
  fr = rank.raw - ir
  if(fr == 0){
    return(nums[ir])
  }
  num1 = nums[ir]
  num2 = nums[ir+1]
  result = fr * (num2 - num1) + num1
  return(result)
}

#Calculates the probability of at least n.successes (number successes) 
#over n.trials (number of trials) given the p.success (probability of
#success)
cumul.binom.atleast <- function(n.trials, p.success,
                                n.successes){
  probs = rep(0, length(n.successes))
  k = 1
  for(i in n.successes:n.trials){
    temp1 = factorial(n.trials)/(factorial(i)*factorial(n.trials-i))
    temp2 = p.success^i
    temp3 = (1-p.success)^(n.trials-i)
    probs[k] = temp1*temp2*temp3
    k = 1 + k
  }
  return(sum(probs))
}

#Returns either mean, standard deviation, or variance (stat.type)
#of a binomial probability
binom.stats <- function(n.trials, p.success, stat.type = 'std dev'){
  if(stat.type=='mean'){
    return(n.trials*p.success)
  }else if(stat.type == 'variance'){
    return((n.trials*p.success)*(1-p.success))
  }else{
    return(sqrt((n.trials*p.success)*(1-p.success)))
  }
  
}

mad.outliers <- function(x, filter.outliers = FALSE){
  med.x = median(x)
  mad.x = mad(x)
  mad.scores = abs(x-med.x)/mad.x
  if(filter.outliers){
    return(x[mad.scores>2.24])
  }else{
    return(mad.scores)
  }
}
#test = mad.outliers(c(2,2,3,3,3,4,4,4,10000,10000))

#Similar to psych::describe
matrix.stats <- function(nums, 
                          multiplier = 1){
  #Create a matrix from nums that's length(nums) rows by one column
  nums.mat = matrix(nums, length(nums), 1)
  #if there's a list of multipliers, compute a weighted average
  #if not, then create a matrix filled with the multiplier arg
  if(is.numeric(multiplier)){
    multiplier = rep(multiplier, length(nums))
  }else{
    if(length(multiplier)!= length(nums)){
      #Check to make sure the dimensions match
      print('Warning: multiplier length != input array dim')
      print('Calculating unweighted average')
      #Revert to 1 otherwise
      multiplier = rep(1, length(nums))
    }
  }
  nums.sum = t(nums.mat) %*% multiplier
  mean.val = nums.sum[1][1]/length(nums) #Get the average
  #Create an array filled with the mean to get deviation info
  means = matrix(rep(mean.val, length(nums)), length(nums), 1)
  d = nums.mat - means #Deviation scores
  
  sum.squares = sum((d)^2) #Square the deviation scores and add together
  variance = sum.squares/(length(nums)-1)
  std.dev = sqrt(variance)
  stat.list = c('mean'= mean.val, 'variance '= variance, 
                'std.dev' = std.dev, 'sum.squares' = sum.squares,
                'cross.product' = crossprod(nums))
  return(stat.list)
}

#Variance of nums (a vector of numbers)
variance <- function(nums){
  means = rep(sum(nums)/length(nums), length(nums))
  diffs = nums - means
  diffs.sqrd = diffs * diffs
  ssq = sum(diffs.sqrd)
  return(ssq/(length(nums)-1))
}

#Standard deviation of nums (a vector of numbers)
std.dev <- function(nums){
  return(sqrt(variance(nums)))
}

#NOTE to do inverse of normdist.area, simply call qnorm(p, mean, sd)
#Returns area under the curve for a normal distribution in desired direction
normdist.area <- function(val, m, sd, direction = 'above'){
  if(direction=='above'){
    return(1-pnorm(val, m, sd))
  }else{
    return(pnorm(val, m, sd))
  }
}

#Returns area under the curve between two points on a normal distribution
normdist.area.between <- function(min.val, max.val, m, sd){
  area.below.min = normdist.area(min.val, m, sd, direction = 'below')
  area.below.max = normdist.area(max.val, m, sd, direction = 'below')
  return(area.below.max-area.below.min)
}

#Returns cutoff scores for a given area of a normal distribution
#middle.prop is the chunk that you want from the middle
#assumes you're pulling from middle of distribution and 
#distribution is symmetric
inverse.normdist.between <- function(middle.prop, m, sd){
  tail.area <- (1-middle.prop)/2 #calculate area of each tail
  bottom.lim <- qnorm(tail.area, m, sd)
  upper.lim <- qnorm(1-tail.area, m, sd)
  return(c(bottom.lim, upper.lim))
}

#Calculates a z-score for a value given the 
#mean and standard deviation
get.zscore <- function(val, m, sd){
  return((val-m)/sd)
}

# v=c(2,  3, 5, 6, 9) 
# x=matrix(v,5,1) 
# m = mean(x) 
# s_sq = var(x) 
# N = length(x) 
# sem=sqrt(s_sq/N) 
# df=N-1 t=qt(.975,df) 
# LL=m-t*sem 
# UL = m+t*sem LL UL
confint.mean <- function(nums, conf.lev = 0.975){
  x = nums[!is.na(nums)] #Exclude NA values
  m = mean(x) 
  s_sq = var(x)
  N = length(x)
  sem = sqrt(s_sq/N) #Estimate of std error of the mean
  df = N - 1
  t = qt(conf.lev, df) #Get t-stat
  LL = m - t * sem
  UL = m + t * sem
  return(c(LL, UL))
  
}

confint.mean.diff <- function(group.1, group.2){
  g1 <- group.1[!is.na(group.1)]
  g2 <- group.2[!is.na(group.2)]
  MSE = (var(g1)+var(g2))/2 #Mean square error--estimate of population variance from samples
  n = length(g1) 
  df = 2*(n-1) 
  varDiff = 2*MSE/n 
  seDiff = sqrt(varDiff) 
  t = qt(.975,df) 
  m1 = mean(g1) 
  m2 = mean(g2) 
  d = m1-m2 
  LL = d-t*seDiff 
  UL = d+t*seDiff
  return(c(LL, UL))
}

stderr.mean <- function(sd, n){
  return(sd/sqrt(n))
}

#Every test score can be thought of as the sum of two independent components, 
#the true score (number of items that respondent knows the answer to) 
#and the error score (number of items that respondent guesses). 
#This can be written as:
test.score <- function(true.score, error.score){
  return(true.score+error.score)
}

test.variance <- function(true.score, error.score, is.variance = FALSE){
  if(is.variance){
    return(true.score+error.score)
  }
  return(var(true.score)+var(error.score))
}

#The reliability of a test, r.test.test, is the ratio of true-score variance to 
#test-score variance. 
#This can be written as:
reliability.test <- function(true.scores, error.scores, is.variance=FALSE){
  if(is.variance){
    return(true.scores/(true.scores+error.scores))
  }
  var.true = var(true.scores)
  var.error = var(error.scores)
  return(var.true/(var.true+var.error))
  
}

test.std.err <- function(true.scores, error.scores){
  rel.test = reliability.test(true.scores, error.scores)
  test.score.sd = sqrt(test.variance(true.scores, error.scores))
  return(test.score.sd*sqrt(1-rel.test))
}

rel.increase <- function(new.item.length, old.item.length, old.rel){
  fac.inc = new.item.length/old.item.length
  return((fac.inc*old.rel)/(1+(fac.inc-1)*old.rel))
}

max.predictive.validity <-function(reliability){
  return(sqrt(reliability))
}

std.err.meandiff <- function(var1, var2, n1, n2){
  return(sqrt((var1/n1)+(var2/n2)))
}

r.to.zprime <- function(r){
  return(0.5*log((1+r)/(1-r)))
}

r.std.err <- function(n){
  return(1/(sqrt(n -3)))
}



