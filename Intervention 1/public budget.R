budget <- c(rep(0,46))
budget[1] <- 73

for(i in 2:46){
  budget[i] <- budget[i-1] * 1.06
  rm(i)
}

for(i in 2:46){
  budget[i] <- budget[i-1] / 1.08
  rm(i)
}

discounted_budget <- sum(budget)
