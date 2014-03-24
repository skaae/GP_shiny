calcLinear <- function(X1,X2,sigma_cov=1) {
  n = length(x1)
  
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      Sigma[i,j] <- sigma_cov*abs(X1[i]-X2[j])
    }
  }
  
  
  diag(Sigma) <- jitter(diag(Sigma)) + diag(Sigma)
  #print(Sigma)
  return(Sigma)
}