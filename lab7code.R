##################################################################################
#LAB 7
#Jake Schneider
##################################################################################
library(tidyverse)
library(ggplot2)
library(e1071)
library(patchwork)

##################################################################################
#task one
##################################################################################
alpha <- 2
beta <- 5
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|> # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),
         # compute the beta PDF
         norm.pdf = dnorm(x,
                          mean = alpha/(alpha+beta),
                          # Gaussian distribution with
                          # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
plot.alpha2.beta5 <- ggplot(data= q1.fig.dat)+
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) + # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color = "Guassian")) + # plot guassian dist
  geom_hline(yintercept=0)+
  theme_bw()+
  xlab("x")+
  ylab("Density")+
  scale_color_manual("", values = c("black", "grey"))+
  theme(legend.position = "bottom")


##################################################################################
alpha <- 5
beta <- 5
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|> # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),
         # compute the beta PDF
         norm.pdf = dnorm(x,
                          mean = alpha/(alpha+beta),
                          # Gaussian distribution with
                          # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

plot.alpha5.beta5 <- ggplot(data= q1.fig.dat)+
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,5)")) + # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color = "Guassian")) + # plot guassian dist
    geom_hline(yintercept=0)+
  theme_bw()+
  xlab("x")+
  ylab("Density")+
  scale_color_manual("", values = c("black", "grey"))+
  theme(legend.position = "bottom")


##################################################################################
alpha <- 5
beta <- 2
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|> # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),
         # compute the beta PDF
         norm.pdf = dnorm(x,
                          mean = alpha/(alpha+beta),
                          # Gaussian distribution with
                          # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
plot.alpha5.beta2 <- ggplot(data= q1.fig.dat)+
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,2)")) + # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color = "Guassian")) + # plot guassian dist
  geom_hline(yintercept=0)+
  theme_bw()+
  xlab("x")+
  ylab("Density")+
  scale_color_manual("", values = c("black", "grey"))+
  theme(legend.position = "bottom")


##################################################################################
alpha <- .5
beta <- .5
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|> # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),
         # compute the beta PDF
         norm.pdf = dnorm(x,
                          mean = alpha/(alpha+beta),
                          # Gaussian distribution with
                          # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

plot.alpha0.5.beta0.5 <- ggplot(data= q1.fig.dat)+
  geom_line(aes(x=x, y=beta.pdf, color="Beta(.5,.5)")) + # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color = "Guassian")) + # plot guassian dist
  geom_hline(yintercept=0)+
  theme_bw()+
  xlab("x")+
  ylab("Density")+
  scale_color_manual("", values = c("black", "grey"))+
  theme(legend.position = "bottom")


#########################################################
#use patchworks to patch the plots together

combine.betas <- (plot.alpha2.beta5 + plot.alpha5.beta5)/
  (plot.alpha5.beta2 + plot.alpha0.5.beta0.5)


#########################################################
#for loop to get all of values in a table

alpha.values <- c(2, 5, 5, .5)
beta.values  <- c(5, 5, 2, .5)

beta.table <- tibble(
  Alpha = numeric(),
  Beta = numeric(),
  Mean = numeric(),
  Variance = numeric(),
  Skewness = numeric(),
  `Excess Kurtosis` = numeric()
)

for(i in 1:4){
alpha <- alpha.values[i]
beta <- beta.values[i]
  
mean.value <- (alpha/(alpha+beta))

variance.value <- ((alpha*beta)/(((alpha+beta)^2)*(alpha+beta+1)))

skewness.value <- (((2*(beta-alpha))*((alpha+beta+1)^.5))/((alpha+beta+2)*((alpha*beta)^.5)))

kurtosis.value <- (6*(((alpha - beta)^2*(alpha+beta+1))-((alpha*beta)*(alpha+beta+2))))/ 
  ((alpha*beta)*(alpha+beta+2)*(alpha+beta+3))

beta.table[i, ] <- tibble(Alpha = alpha, Beta = beta, Mean = mean.value, 
                          Variance = variance.value, Skewness = skewness.value, 
                          `Excess Kurtosis` = kurtosis.value)
}


##############################################################################################
#task two
##########################################################################################
beta.moment <- function(alpha, beta, k, centered = F){
  mean.x <- alpha/(alpha+beta)
  
  centered.moments <- integrate(function(x) (x-mean.x)^k * dbeta(x, alpha, beta),
                                lower = 0, upper =1)$value
  if(centered){
    return(centered.moments)
    
  } else{
    
    uncentered.moments <-  integrate(function(x) x^k * dbeta(x, alpha, beta), 
                                     lower = 0, upper = 1)$value
    return(uncentered.moments)
  }
}

#need to make a function that takes the beta moment
skewness <- beta.moment(2,5,3, T)/(beta.moment(2,5,2, T)^(3/2))

kurtosis <- (beta.moment(2,5,4, T)/(beta.moment(2,5,2, T)^2))-3



##########################################################################################
#Task 3
##########################################################################################
#first case
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 2
beta <- 5
beta.sample <- rbeta(n = sample.size, # sample size
                     shape1 = alpha, # alpha parameter
                     shape2 = beta)


hist.den <- ggplot(data.frame(x = beta.sample), aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 25, fill = "lightblue", color = "black", alpha = 0.7) +
  
  geom_density(aes(color = "Our Estimated Density Function"), size = 1) +  
  
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta),
                aes(color = "Beta Distribution"), linetype = "dashed", size = 1) + 
  
  scale_color_manual(name = "Legend", 
                     values = c("Our Estimated Density Function" = "red", 
                                "Beta Distribution" = "blue")) +   
  
  theme_minimal() +
  ggtitle("Histogram & Density Plot of Beta(2,5) Sample") +
  xlab("x") + ylab("Density") +
  theme(legend.position = "bottom")  


#############################################################
#second case
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 5
beta <- 5
beta.sample <- rbeta(n = sample.size, # sample size
                     shape1 = alpha, # alpha parameter
                     shape2 = beta)


hist.den <- ggplot(data.frame(x = beta.sample), aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 25, fill = "lightblue", color = "black", alpha = 0.7) +
  
  geom_density(aes(color = "Our Estimated Density Function"), size = 1) +  
  
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta),
                aes(color = "Beta Distribution"), linetype = "dashed", size = 1) + 
  
  scale_color_manual(name = "Legend", 
                     values = c("Our Estimated Density Function" = "red", 
                                "Beta Distribution" = "blue")) +   
  
  theme_minimal() +
  ggtitle("Histogram & Density Plot of Beta(2,5) Sample") +
  xlab("x") + ylab("Density") +
  theme(legend.position = "bottom")  

#############################################################
#case 3
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 5
beta <- 2
beta.sample <- rbeta(n = sample.size, # sample size
                     shape1 = alpha, # alpha parameter
                     shape2 = beta)


hist.den <- ggplot(data.frame(x = beta.sample), aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 25, fill = "lightblue", color = "black", alpha = 0.7) +
  
  geom_density(aes(color = "Our Estimated Density Function"), size = 1) +  
  
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta),
                aes(color = "Beta Distribution"), linetype = "dashed", size = 1) + 
  
  scale_color_manual(name = "Legend", 
                     values = c("Our Estimated Density Function" = "red", 
                                "Beta Distribution" = "blue")) +   
  
  theme_minimal() +
  ggtitle("Histogram & Density Plot of Beta(2,5) Sample") +
  xlab("x") + ylab("Density") +
  theme(legend.position = "bottom")  

#############################################################
#case 4
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- .5
beta <- .5
beta.sample <- rbeta(n = sample.size, # sample size
                     shape1 = alpha, # alpha parameter
                     shape2 = beta)


hist.den <- ggplot(data.frame(x = beta.sample), aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 25, fill = "lightblue", color = "black", alpha = 0.7) +
  
  geom_density(aes(color = "Our Estimated Density Function"), size = 1) +  
  
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta),
                aes(color = "Beta Distribution"), linetype = "dashed", size = 1) + 
  
  scale_color_manual(name = "Legend", 
                     values = c("Our Estimated Density Function" = "red", 
                                "Beta Distribution" = "blue")) +   
  
  theme_minimal() +
  ggtitle("Histogram & Density Plot of Beta(2,5) Sample") +
  xlab("x") + ylab("Density") +
  theme(legend.position = "bottom")  


