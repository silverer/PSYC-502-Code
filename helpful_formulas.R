library(psych)
library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stats, dplyr, psych, rstatix)

#Nums: vector containing observed values
geom.mean <- function(nums){
  product = prod(nums)
  return(product^(1/length(nums)))
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
#max.n.trials is the maximum trials performed 
#e.g., if you're taking a 12-question test w/performance = 80%
#and want to know how many you'd need to get right to do *sig* better than chance,
#you'd do: 0.5, 12, 0.8
min.better.than.binom <- function(target.percent, max.n.trials, percent.success){
  i = 1
  #Iterate over trials until you get the minimum trials necessary to reach percent.success
  #That is, the min trials required to have a 95% lower CI that is greater than the minimum successes required
  #If testiing # of trials to do better than chance, target.percent == 0.5
  while(i<max.n.trials){
    tmp.r = binom.test(i, max.n.trials, percent.success, 
                       alternative='greater')
    if(tmp.r$conf.int[1]<target.percent){
      i = i + 1
    }else{
      return(i)
    }
  }
}
#Gives you the probability of performing better than a specific % given 
#a set of trial opportunities and likelihood of success on each trial
prob.min.better.than.binom <- function(target.percent, max.n.trials, percent.success){
  test.val = min.better.than.binom(target.percent, max.n.trials,percent.success)
  return(cumul.binom.atleast(max.n.trials, percent.success, test.val))
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

get.area.zscore <- function(val, m, sd, area = 'below'){
  z = get.zscore(val, m, sd)
  if(area == 'below'){
    return(pnorm(z))
  }else if(area == 'above'){
    return(1-pnorm(z))
  }else{
    return(pnorm(z)*2)
  }
}

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

#group.sizes is a vector that specifies how large each group is
#For example, if you're comparing 2 groups, 
#and length(group1) = 32, length(group2) = 35
#Pass this function c(length(group1), length(group2)) or c(32, 35)
harmonic.mean <- function(group.sizes){
  numerator = length(group.sizes)
  denoms = 1/group.sizes
  return(numerator/sum(denoms))
}

std.err.meandiff <- function(var1, var2, n1, n2){
  return(sqrt((var1/n1)+(var2/n2)))
}

confint.mean.diff <- function(group.1, group.2){
  g1 = group.1[!is.na(group.1)]
  g2 = group.2[!is.na(group.2)]
  df = (length(g1) - 1) + (length(g2) - 1)
  t.stat = qt(.975,df)
  if(length(g1)!=length(g2)){
    sse = sum((g1-mean(g1))^2) + sum((g2-mean(g2))^2)
    MSE = sse/df
    harm.mean = harmonic.mean(c(length(g1), length(g2)))
    seDiff = sqrt(((2*MSE)/harm.mean)) #Std err of the mean difference
  }else{
    MSE = (var(g1)+var(g2))/2 #Mean square error--estimate of population variance from samples
    seDiff = sqrt((2*MSE)/length(g1))
  }
  d = mean(g1)-mean(g2) 
  LL = d-t.stat*seDiff 
  UL = d+t.stat*seDiff
  return(c(LL, UL))
}

stderr.mean <- function(sd, n){
  return(sd/sqrt(n))
}

tval.mean.diff <- function(g1, g2){
  mdiff = mean(g1)-mean(g2)
  mse = (var(g1)+var(g2))/2
  #Get the standard error of the mean difference
  seM = sqrt((2*mse)/length(g1))
  df = 2*(length(g1)-1)
  #Get t-value for test statistic
  test.stat = mdiff/seM
  
  
  return(test.stat)
}

pval.mean.diff <- function(g1, g2){
  test.stat = tval.mean.diff(g1,g2)
  df = 2*(length(g1)-1)
  p.val = 2*pt(abs(test.stat),df,
               lower.tail=FALSE)
  return(p.val)
}

#### Correlations and test reliability ####

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


r.to.zprime <- function(r){
  return(0.5*log((1+r)/(1-r)))
}

r.std.err <- function(n){
  return(1/(sqrt(n -3)))
}

confint.rcorr <- function(r, n){
  z.prime = r.to.zprime(r)
  z.se = r.std.err(n)
  ll = z.prime - (1.96*z.se)
  ul = z.prime + (1.96*z.se)
  ll = fisherz2r(ll)
  ul = fisherz2r(ul)
  return(c(ll, ul))
}

#### Power ####

#Gives the critical value on a distribution for a given alpha
#E.g., two-tailed 95% conf level = 1.96
lookup.critical.value <- function(prob, tails = 'two'){
  if(tails == 'two'){
    arg = 1 - prob
    return(qnorm(1-arg/2))
  }else if (tails=='lower'){
    return(qnorm(1-p))
  }else{
    return(qnorm(p))
  }
}

get.rejection.regions <- function(var1, var2, n1, n2, conf.level = 0.95){
  se = std.err.meandiff(var1, var2, n1, n2)
  return(inverse.normdist.between(conf.level, 0, se))
}

get.prob.rejection <- function(var1, var2, n1, n2, h.diff,
                               conf.lev = 0.95, direction = 'below'){
  #Z = (-3.33 - (-3))/1.7 = -0.2, AUC for Z = -0.2 is 0.42
  se = std.err.meandiff(var1, var2, n1, n2)
  rej.regions = get.rejection.regions(var1, var2, n1, n2, conf.level=conf.lev)
  z = (rej.regions - h.diff)/se
  return(get.area.zscore(rej.regions[1], h.diff, se))
}

get.se.diff <- function(desired.power, conf.level,
                        tails = 'two'){
  crit.val = lookup.critical.value(conf.level, tails = tails)
  z.score = qnorm(desired.power)
  return(crit.val+z.score)
}

get.power.sample.size <- function(var1, var2, h.diff,
                                  power.lev = 0.9, conf.lev = 0.95,
                                  tails = 'two'){
  distribution.distance = get.se.diff(power.lev, conf.lev, tails = tails)
  #N = (2.8^2)/(3^2) * (144 + 144) = 250.88
  req.n = (distribution.distance^2)/(h.diff^2) * (var1+var2)
  return(req.n)
}

COEFFS.TREND <- read.csv("data/trend_coeffs.csv")
COEFFS.TREND <- COEFFS.TREND %>% 
  mutate(trend.type = as.factor(trend.type),
         trend.type = recode_factor(trend.type, 
                                    Lin='linear',
                                    Quad = 'quadratic',
                                    Cubic = 'cubic'))
standard_error <- function(x) sd(x) / sqrt(length(x))

#Test for linear, quadratic, or cubic components of trend
#df is the dataframe with within-subj data
#cols is a vector of timepoint columns in string format (e.g., 'Time1','Time2', etc.)
#contrast.type
test.trend <- function(df, cols, contrast.type = 'linear',
                       id.col = 'id', return.contr.means = FALSE,
                       return.L = TRUE){
  temp = df %>% 
    dplyr::select((all_of(cols)))
  num.points = ncol(temp)
  #Choose which elements of coefficients dataframe to keep
  contrast.mat = COEFFS.TREND %>% 
    dplyr::filter(n.means == num.points & trend.type==contrast.type) %>% 
    dplyr::select(-c(trend.type, n.means))
  #Define matrix of data to which contrast will be applied
  X = as.matrix(temp)
  #Define contrast weights
  w = as.matrix(as.numeric(contrast.mat[,1:num.points]))
  #This list stores the result of the contrast
  stat.list = list()
  #If contr.means == TRUE, then compute subject-wise means
  if(return.contr.means){
    multiplier = as.vector(w)
    #Make sure that the id column is present and if not, add it here
    if(id.col %in% colnames(df)==FALSE){
      df['id'] = factor(1:nrow(df))
      id.col = 'id'
    }
    df['L'] = NA
    new_cols = list()
    for(i in 1:length(cols)){
      new_col = paste0(cols[i],'_L')
      df[new_col] = multiplier[[i]] * df[cols[i]]
      new_cols[i] = new_col
    }
    temp_1 = df %>% dplyr::select(all_of(unlist(new_cols)))
    df['L'] = rowSums(temp_1)
    subj.means = as.data.frame(df %>% 
                                 group_by(.data[[id.col]]) %>% 
                                 dplyr::summarise(subject_mean = mean(L)))
    stat.list[['subj.means']] = as.data.frame(subj.means)
  }
  
  
  N = nrow(X)
  L = t(colMeans(X)) %*% w
  s_sq = t(w) %*% cov(X) %*% w
  sem = sqrt(s_sq/N)
  dof = nrow(X)-1
  t.stat = L/sem
  p.val = 2*pt(abs(t.stat), dof, lower.tail = FALSE)
  
  stat.list[['L.mean']] = L
  stat.list[['t.stat']] = t.stat
  stat.list[['dof']] = dof
  stat.list[['p.val']] = p.val
  stat.list[['seL']] = sem
  stat.list[['contr.weights']] = w
  stat.list[['L']] <- df$L
  stat.list
}

