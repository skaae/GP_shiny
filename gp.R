

source("gaussProcess.R")
source("calcSigma.R")
#create wrapper around sigma function to accomodate other covariance functions
mySig <- function(x1,x2) calcSigma(x1,x2,0.1,0.1)
b = GaussProcess(mySig,n_samples=3,obs_noise=1,x_samples = 20)
x=b$x
values=b$values
x_star=b$x_star
f.bar.star=b$f.bar.star
obs_noise=b$obs_noise
f=b$f
gg <- ggplot(values, aes(x=x,y=value)) + 
  geom_line(aes(group=variable), colour="grey80") +
  geom_line(data=NULL,aes(x=x_star,y=f.bar.star),colour="red", size=1) + 
  geom_errorbar(data=f,aes(x=x,y=NULL,ymin=y-2*obs_noise, ymax=y+2*obs_noise), width=0.2) +
  geom_point(data=f,aes(x=x,y=y)) +
  theme_bw() +
  ylab("output,y") +
  xlab("input, x")
print(gg)



