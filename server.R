library(shiny)
source("gaussProcess.R")
source("calcSigma.R")
source("calcLinear.R")
require(MASS)
require(plyr)
require(reshape2)
require(ggplot2)
# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  x_samples = 200
  x_star <- seq(-pi*5,pi*5,length.out=x_samples)
  output$GaussianProcess <- renderPlot({
        
    n_samples=input$n_samples
    obs_noise=input$noise
    n_obs = input$n_obs
    
    mySig <- function(x1,x2) calcSigma(x1,x2,input$length_scale,input$sigma_mag)
    #mySig <- function(x1,x2) calcLinear(x1,x2,input$length_scale)
    
    set.seed(12345)
    
    # 2. Now let's assume that we have some known data points;
    # this is the case of Figure 2.2(b). In the book, the notation 'f'
    # is used for f$y below.  I've done this to make the ggplot code
    # easier later on.
    
   
    s = seq(1:(5*n_obs))
    idx = sample(s,n_obs)
  
    f <- data.frame(x=(pi*seq(-3,3,length.out=5*n_obs))[idx],
                    y=(sin(pi*seq(-3,3,length.out=5*n_obs))+rnorm(n_obs)/5)[idx])
    
    # Calculate the covariance matrices
    # using the same x_star values as above
    x <- f$x
    k.xx <- mySig(x,x)
    k.xxs <- mySig(x,x_star)
    k.xsx <- mySig(x_star,x)
    k.xsxs <- mySig(x_star,x_star)
    
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
    

    df.y = data.frame(x=f$x,ymax=f$y+obs_noise,ymin=f$y-obs_noise)

    
    df = data.frame(x=x_star,y=f.bar.star)
    gg <- ggplot(values, aes(x=x,y=value)) + 
      geom_line(aes(group=variable), colour="grey80") +
      geom_line(data=df,aes(x=x,y=y),colour="red", size=1) + 
    geom_point(data=f,aes(x=x,y=y)) +
    geom_errorbar(data=df.y,aes(x=x,y=NULL,ymin=ymax, ymax=ymin), width=0.2) +
    theme_bw() +
    ylab("output,y") +
    xlab("input, x") +
    opts(title = "GP")
    print(gg)

    #rm(b,x,values,x_star,f.bar.star,obs_noise,f,gg)
  })
  output$covariance <- renderPlot({
    mySig <- function(x1,x2) calcSigma(x1,x2,input$length_scale,input$sigma_mag)
    k.xsxs <- mySig(x_star,x_star)
    image(k.xsxs,main="Covariance fuction",col=gray.colors(128))
  })
})
