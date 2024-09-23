#Title: Test of rdrobust vs rdplot.
#Purpose: rdplot and rdrobust generate different estimates of the treatment
#effect size and model coefficients when passed identical parameters.

#This is a demonstration that the two functions can generate identical results
#when the optimal bandwidth calculated by rdrobust is prespecified in the
#arguments to rdplot. The default implementation of rdplot calculates a 'global'
#polynomial which attempts to fit the entire dataset including those points
#outside of the optimal bandwidth, and thus the estimates for the treatment
#effect and models, both left and right, differ from those of rdrobust unless
#the bandwidth is specified.

#import libraries
library(rdrobust)
library(magrittr)

#set seed for reproducibility
set.seed(2024)

#generate sample data with different slopes on each side of the cutoff (75)
x <- seq(1:120)
c <- 75
y <- case_when(x < c ~ 60 - 0.2*x + runif(c)*10,
               x >= c ~ 10 + 0.33*x + runif(120-c)*15)

#generate linear models in this example, though p can be any integer >= 0
p <- 1

#calculate rdrobust estimate and extract the bandwidth and model coefficients
reg <- rdrobust(x=x,y=y,c=c,p=p,kernel = 'uni')
reg_bandwidth <- reg$bws
reg_coefs <- reg %>% {data.frame(left = .$beta_Y_p_l, right = .$beta_Y_p_r)}
print(reg_coefs)

#calculate rdplot estimates, view plot, and extract model coefficients.
#this step specifies the bandwidth h equals the bandwidth from rdrobust.
#view model coefficients and they should be identical to those from rdrobust
#because the specified bandwidth is the same.
graph <- rdplot(x=x,y=y,c=c,p=p,kernel = 'uni', h = reg_bandwidth[1,1])
print(graph$coef)

#now repeat the rdplot calculation but without specifying the bandwidth.
#this will generate a global polynomial. notice that the regression lines
#extend over the entire dataset and not just those points within the rdrobust
#bandwidth. Hence the calculated treatment effect and model coefficients here
#reflect the global polynomials and not the local polynomials calculated by
#rdrobust and rdplot (with specified bandwidth) in the code above.
graph <- rdplot(x=x,y=y,c=c,p=p,kernel = 'uni')
print(graph$coef)
