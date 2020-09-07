
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
  for(i in n.successes){
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

