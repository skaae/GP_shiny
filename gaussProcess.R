GaussProcess <- function(calcSigma,n_samples=3,obs_noise=1,x_samples = 50){
  
  # Demo of Gaussian process regression with R
  # James Keirstead
  # 5 April 2012
  
  # Chapter 2 of Rasmussen and Williams's book `Gaussian Processes
  # for Machine Learning' provides a detailed explanation of the
  # math for Gaussian process regression.  It doesn't provide
  # much in the way of code though.  This Gist is a brief demo
  # of the basic elements of Gaussian process regression, as
  # described on pages 13 to 16.
  
  
  # Load in the required libraries for data manipulation
  # and multivariate normal distribution
  require(MASS)
  require(plyr)
  require(reshape2)
  require(ggplot2)
  
  # Set a seed for repeatable plots
  set.seed(12345)
  
  
  x_star <- seq(-5,5,length.out=x_samples)
  
  # 2. Now let's assume that we have some known data points;
  # this is the case of Figure 2.2(b). In the book, the notation 'f'
  # is used for f$y below.  I've done this to make the ggplot code
  # easier later on.
  f <- data.frame(x=c(-4,-3,-1,0,2),
                  y=c(-2,0,1,2,-1))
  
  # Calculate the covariance matrices
  # using the same x_star values as above
  x <- f$x
  k.xx <- calcSigma(x,x)
  k.xxs <- calcSigma(x,x_star)
  k.xsx <- calcSigma(x_star,x)
  k.xsxs <- calcSigma(x_star,x_star)
  
  # Recalculate the mean and covariance functions
  f.bar.star <- k.xsx%*%solve(k.xx + obs_noise^2*diag(1, ncol(k.xx)))%*%f$y
  cov.f.star <- k.xsxs - k.xsx%*%solve(k.xx + obs_noise^2*diag(1, ncol(k.xx)))%*%k.xxs
  
  # Recalulate the sample functions
  values <- matrix(rep(0,length(x_star)*n_samples), ncol=n_samples)
  for (i in 1:n_samples) {
    values[,i] <- mvrnorm(1, f.bar.star, cov.f.star)
  }
  values <- cbind(x=x_star,as.data.frame(values))
  values <- melt(values,id="x")
  
  
  
  # Plot the result, including error bars on the observed points
  
  
  
  
  ll = list(x=x,values=values,x_star=x_star,f.bar.star=f.bar.star,obs_noise=obs_noise,f=f)
  return(ll)
}