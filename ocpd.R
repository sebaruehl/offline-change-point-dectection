#CUSUM

cusum = function(x){
  res = rep(0, length(x))
  meanx = mean(x)
  for(i in 2:length(x)){
    res[i] = res[i-1] + x[i] - meanx
  }
  res
}

cusum_plus = function(x){
  res = rep(0, length(x))
  meanx = mean(x)
  for(i in 2:length(x)){
    res[i] = max(0, res[i-1] + x[i] - meanx)
  }
  res
}

cusum_minus = function(x){
  res = rep(0, length(x))
  meanx = mean(x)
  for(i in 2:length(x)){
    res[i] = -min(0, -(res[i-1] - x[i] + meanx))
  }
  res
}

s_i_mean = function(x, theta_0, theta_1){
  ((x-theta_0)^2)-((x-theta_1)^2)
}

log_like_rat = function(x){
  res = rep(0, length(x))
  for(i in 1:length(x)){
    res[i] = s_i_mean(x[i], mean(x[1:(i-1)]), mean(x[i:length(x)]))
  }
  res
}

log_like_rat_ln = function(x){
  res = rep(0, length(x))
  for(i in 1:length(x)){
    res[i] = s_i_mean(x[i], lnmean(x[1:(i-1)]), lnmean(x[1:i]))
  }
  res
}

ratio1 = function(x) {
  res = rep(0, length(x))
  for(i in 1:length(x)){
    res[i] = -(sum((x[1:(i-1)]-mean(x[1:(i-1)]))^2) + sum((x[i:length(x)]-mean(x[i:length(x)]))^2))
  }
  res
}

ratio2 = function(x) {
  res = rep(0, length(x))
  for(i in 1:length(x)){
    res[i] = (-(sum((x[1:(i-1)]-mean(x[1:(i-1)]))^2) + sum((x[i:length(x)]-mean(x[i:length(x)]))^2)))/(-sum((x-mean(x))^2))
  }
  res
}

cusum_2 = function(x){
  res = rep(0, length(x))
  s_k = log_like_rat(x)
  for(i in 1:length(x)){
    res[i] = sum(s_k[1:i]) - min(s_k[1:i])
  }
  res
}

cusum_3 = function(x){
  res = rep(0, length(x))
  s_k = log_like_rat(x)
  for(i in 1:length(x)){
    res[i] = sum(s_k[1:i])
  }
  res
}

cusum_ratio = function(x){
  res = rep(0, length(x))
  res2 = rep(0, length(x))
  s_k = log_like_rat(x)
  for(i in 1:length(x)){
    res[i] = sum(s_k[1:i])
    res2[i] = res[i] - min(res)
  }
  res2
}

cusum_4 = function(x){
  res = rep(0, length(x))
  s_k = ratio2(x)
  for(i in 1:length(x)){
    res[i] = sum(s_k[1:i])
  }
  res
}

lnvar = function(x) {
  var(log(x))
}

lnmean = function(x){
  mean(log(x))
}
