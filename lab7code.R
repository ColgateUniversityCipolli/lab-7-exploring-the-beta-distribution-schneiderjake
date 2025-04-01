##################################################################################
#LAB 7
#Jake Schneider
##################################################################################
library(tidyverse)
library(ggplot2)
library(e1071)
library(patchwork)
library(xtable)



##################################################################################
##################################################################################
#task one
##################################################################################
##################################################################################

#1st case for population level
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
#2nd case for population level
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
#3rd case for population level
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
#4th case for population level
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

ggsave("varying.beta.distributions.pdf", plot = combine.betas, width = 10, height = 6)

#########################################################
#for loop to get all of population level values in a table
#table for population level beta 

alpha.values <- c(2, 5, 5, .5)
beta.values  <- c(5, 5, 2, .5)

pop.lvl.stats <- tibble(
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

pop.lvl.stats[i, ] <- tibble(Alpha = alpha, Beta = beta, Mean = mean.value, 
                          Variance = variance.value, Skewness = skewness.value, 
                          `Excess Kurtosis` = kurtosis.value)
}

##############################################################################################
##############################################################################################
#task two
##############################################################################################
##############################################################################################

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

beta.moments.table <- tibble(
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
  
  moments.mean <- beta.moment(alpha,beta, 1)  
  
  moments.variance <- beta.moment(alpha,beta,2, T)
  
  moments.skewness <- beta.moment(alpha,beta,3, T)/(beta.moment(alpha,beta,2, T)^(3/2))
  
  moments.kurtosis <- (beta.moment(alpha,beta,4, T)/(beta.moment(alpha,beta,2, T)^2))-3
  
  
  beta.moments.table <- bind_rows(
    beta.moments.table, 
    tibble(Alpha = alpha, Beta = beta, Mean = moments.mean, 
           Variance = moments.variance, Skewness = moments.skewness, 
           `Excess Kurtosis` = moments.kurtosis)
  )
}


##############################################################################################
##############################################################################################
#Task 3
##############################################################################################
##############################################################################################

#1st case n=500 
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 2
beta <- 5
beta.sample <- rbeta(n = sample.size, # sample size
                     shape1 = alpha, # alpha parameter
                     shape2 = beta)


hist.den.1 <- ggplot(data.frame(x = beta.sample), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "lightblue", color = "black", alpha = 0.7) +
  
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


#table for our summary stats when n=500
summary.stats.1 <- tibble(beta.sample) |>
  summarize(
    sample.mean     = mean(beta.sample),
    sample.var      = var(beta.sample),
    sample.skew     = skewness(beta.sample),
    sample.exc.kurt = kurtosis(beta.sample, type = 3)
  )




##############################################################################################
#2nd case n=500 
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 5
beta <- 5
beta.sample <- rbeta(n = sample.size, # sample size
                     shape1 = alpha, # alpha parameter
                     shape2 = beta)


hist.den.2 <- ggplot(data.frame(x = beta.sample), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "lightblue", color = "black", alpha = 0.7) +
  
  geom_density(aes(color = "Our Estimated Density Function"), size = 1) +  
  
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta),
                aes(color = "Beta Distribution"), linetype = "dashed", size = 1) + 
  
  scale_color_manual(name = "Legend", 
                     values = c("Our Estimated Density Function" = "red", 
                                "Beta Distribution" = "blue")) +   
  
  theme_minimal() +
  ggtitle("Histogram & Density Plot of Beta(5,5) Sample") +
  xlab("x") + ylab("Density") +
  theme(legend.position = "bottom")  

summary.stats.2 <- tibble(beta.sample) |>
  summarize(
    sample.mean     = mean(beta.sample),
    sample.var      = var(beta.sample),
    sample.skew     = skewness(beta.sample),
    sample.exc.kurt = kurtosis(beta.sample, type = 3)
  )



##############################################################################################
#3rd case n=500 
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 5
beta <- 2
beta.sample <- rbeta(n = sample.size, # sample size
                     shape1 = alpha, # alpha parameter
                     shape2 = beta)


hist.den.3 <- ggplot(data.frame(x = beta.sample), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "lightblue", color = "black", alpha = 0.7) +
  
  geom_density(aes(color = "Our Estimated Density Function"), size = 1) +  
  
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta),
                aes(color = "Beta Distribution"), linetype = "dashed", size = 1) + 
  
  scale_color_manual(name = "Legend", 
                     values = c("Our Estimated Density Function" = "red", 
                                "Beta Distribution" = "blue")) +   
  
  theme_minimal() +
  ggtitle("Histogram & Density Plot of Beta(5,2) Sample") +
  xlab("x") + ylab("Density") +
  theme(legend.position = "bottom")  


summary.stats.3 <- tibble(beta.sample) |>
  summarize(
    sample.mean     = mean(beta.sample),
    sample.var      = var(beta.sample),
    sample.skew     = skewness(beta.sample),
    sample.exc.kurt = kurtosis(beta.sample, type = 3)
  )



##############################################################################################
#4th case n=500 
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- .5
beta <- .5
beta.sample <- rbeta(n = sample.size, # sample size
                     shape1 = alpha, # alpha parameter
                     shape2 = beta)


hist.den.4 <- ggplot(data.frame(x = beta.sample), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "lightblue", color = "black", alpha = 0.7) +
  
  geom_density(aes(color = "Our Estimated Density Function"), size = 1) +  
  
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta),
                aes(color = "Beta Distribution"), linetype = "dashed", size = 1) + 
  
  scale_color_manual(name = "Legend", 
                     values = c("Our Estimated Density Function" = "red", 
                                "Beta Distribution" = "blue")) +   
  
  theme_minimal() +
  ggtitle("Histogram & Density Plot of Beta(.5,.5) Sample") +
  xlab("x") + ylab("Density") +
  theme(legend.position = "bottom")  

summary.stats.4 <- tibble(beta.sample) |>
  summarize(
    sample.mean     = mean(beta.sample),
    sample.var      = var(beta.sample),
    sample.skew     = skewness(beta.sample),
    sample.exc.kurt = kurtosis(beta.sample, type = 3)
  )

###########################################
#table for n=500 beta distributions

#add identifying columns to each summary
summary.stats.1 <- summary.stats.1 |> mutate(Alpha = 2, Beta = 5)
summary.stats.2 <- summary.stats.2 |> mutate(Alpha = 5, Beta = 5)
summary.stats.3 <- summary.stats.3 |> mutate(Alpha = 5, Beta = 2)
summary.stats.4 <- summary.stats.4 |> mutate(Alpha = 0.5, Beta = 0.5)

#combine them efficiently using bind_rows()
combined.summary.stats <- bind_rows(
  summary.stats.1,
  summary.stats.2,
  summary.stats.3,
  summary.stats.4
) |> 
  select(Alpha, Beta, everything())  #reorder to read easier 


    
  
  
##############################################################################################
##############################################################################################
#Task 3
##############################################################################################
##############################################################################################
library(cumstats)
  
  set.seed(7272) # Set seed so we all get the same results.
  sample.size <- 500 # Specify sample details
  alpha <- 2
  beta <- 5
  beta.sample <- rbeta(n = sample.size, # sample size
                       shape1 = alpha, # alpha parameter
                       shape2 = beta)# beta parameter
  
cumulative.stats <- tibble(beta.sample)|>
  mutate(
    Mean = cummean(beta.sample),
    Skewness = cumskew(beta.sample),
    Variance = cumvar(beta.sample),
    `Excess Kurtosis` = cumkurt(beta.sample) -3
  )|>
  mutate(n = row_number())
  
#################
#Mean
og.cum.mean.plot <- ggplot(cumulative.stats, aes(x = n, y = Mean)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = pop.lvl.stats$Mean[1], linetype = "dashed", color = "red") +
  labs(title = "Cumulative Mean", x = "Sample Size", y = "Mean")

#################
#Skewness
og.cum.skew.plot <- ggplot(cumulative.stats, aes(x = n, y = Skewness)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = pop.lvl.stats$Skewness[1], linetype = "dashed", color = "red") +
  labs(title = "Cumulative Skewness", x = "Sample Size", y = "Skewness")
  
#################
#Variance
og.cum.var.plot <- ggplot(cumulative.stats, aes(x = n, y = Variance)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = pop.lvl.stats$Variance[1], linetype = "dashed", color = "red") +
  labs(title = "Cumulative Variance", x = "Sample Size", y = "Variance")

#################
#Kurtosis
og.cum.kurt.plot <- ggplot(cumulative.stats, aes(x = n, y = `Excess Kurtosis`)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = pop.lvl.stats$`Excess Kurtosis`[1], linetype = "dashed", color = "red") +
  labs(title = "Cumulative Kurtosis", x = "Sample Size", y = "Kurtosis")
  
cum.plots <- (og.cum.mean.plot | og.cum.skew.plot)/
  (og.cum.var.plot | og.cum.kurt.plot)
  

##############################################################################################
##############################################################################################
#Task 4
##############################################################################################
##############################################################################################
#Cum Stat simulation

all.simulated.cum.stats <- tibble()

for(i in 2:50){
  set.seed(7272+i)
  
  new.samples <- tibble(x = rbeta(n = 500, # sample size
                       shape1 = 2, # alpha parameter
                       shape2 = 5))# beta parameter
  
  sim.cum.stats <- new.samples|>
    mutate(
      Mean = cummean(x),
      Skewness = cumskew(x),
      Variance = cumvar(x),
      `Excess Kurtosis` = cumkurt(x) -3,
      n = row_number(),
      Simulation = i
      )
  
  all.simulated.cum.stats <- bind_rows(all.simulated.cum.stats, sim.cum.stats)
  
}
#################
#plotting our simulated samples
sim.og.cum.mean.plot <- og.cum.mean.plot +
  geom_line(data = all.simulated.cum.stats,
            aes(x = n , y = Mean, group = Simulation, color = as.factor(Simulation)), alpha = .5) +
  theme(legend.position = "none")

sim.og.cum.var.plot <- og.cum.var.plot +
  geom_line(data = all.simulated.cum.stats, 
            aes(x = n , y = Variance, group = Simulation, color = as.factor(Simulation)), alpha = .5) +
  theme(legend.position = "none")

sim.og.cum.skew.plot <- og.cum.skew.plot +
  geom_line(data = all.simulated.cum.stats, 
            aes(x = n , y = Skewness, group = Simulation, color = as.factor(Simulation)), alpha = .5) +
  theme(legend.position = "none")

sim.og.cum.kurt.plot <- og.cum.kurt.plot +
  geom_line(data = all.simulated.cum.stats, 
            aes(x = n , y = `Excess Kurtosis`, group = Simulation, color = as.factor(Simulation)), alpha = .5) +
  theme(legend.position = "none")


combined.sim.og.plots <- (sim.og.cum.mean.plot | sim.og.cum.var.plot)/
  (sim.og.cum.skew.plot | sim.og.cum.kurt.plot)

ggsave("convergence.plots.pdf", plot = combined.sim.og.plots, width = 10, height = 65)


##############################################################################################
##############################################################################################
#Task 5
##############################################################################################
##############################################################################################

detach("package:cumstats", unload = T)

sampling.distribution.table <- tibble()

for(i in 1:1000){
  set.seed(7272+i)
  
  beta.data <- rbeta(n = 500, # sample size
                                  shape1 = 2, # alpha parameter
                                  shape2 = 5)# beta parameter
  
  distribution.stats <- tibble(
    Mean              = mean(beta.data),
    Variance          = var(beta.data),
    Skewness          = skewness(beta.data),
    `Excess Kurtosis` = kurtosis(beta.data, type = 3),
      Simulation = i
  )
  sampling.distribution.table <- bind_rows(sampling.distribution.table, distribution.stats)
  
}

#Mean Distribution
mean.variation <- ggplot(sampling.distribution.table, aes(x = Mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Sampling Distribution of Sample Mean", x = "Sample Mean", y = "Density")

#Variance Distribution
variance.variation <- ggplot(sampling.distribution.table, aes(x = Variance)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Sampling Distribution of Sample Variance", x = "Sample Variance", y = "Density")

#Skewness Distribution
skewness.variation <- ggplot(sampling.distribution.table, aes(x = Skewness)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "purple", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Sampling Distribution of Sample Skewness", x = "Sample Skewness", y = "Density")

#Kurtosis Distribution
kurtosis.variation <- ggplot(sampling.distribution.table, aes(x = `Excess Kurtosis`)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "orange", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Sampling Distribution of Sample Kurtosis", x = "Sample Kurtosis", y = "Density")

variation.distributions <- (mean.variation + variance.variation)/
  (skewness.variation + kurtosis.variation)

ggsave("variation.distributions.pdf", plot = variation.distributions, width = 10, height = 5)



