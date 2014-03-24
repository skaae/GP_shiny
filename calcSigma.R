calcSigma <- function(X1,X2,l=1,sigma_cov=1) {
  # Calculates the covariance matrix sigma using a
  # simplified version of the squared exponential function.
  #
  # Although the nested loops are ugly, I've checked and it's about
  # 30% faster than a solution using expand.grid() and apply()
  #
  # Parameters:
  #  X1, X2 = vectors
  #   l = the scale length parameter
  # Returns:
  #   a covariance matrix
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      Sigma[i,j] <- sigma_cov*exp(-0.5*(abs(X1[i]-X2[j])/l)^2)
    }
  }
  
  return(Sigma)
}