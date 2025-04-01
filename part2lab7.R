library(tidyverse)
library(ggplot2)
library(patchwork)
library(nleqslv)

################################################################################
################################################################################
#task 6
################################################################################
################################################################################

cntry.dth.rts <- read_csv("API_SP.DYN.CDRT.IN_DS2_en_csv_v2_76451.csv")
#select only the infomration that we need from the data 
cntry.dth.rts <- cntry.dth.rts |>
  select("Country Name", "Country Code", "2022")|>
  mutate(`2022` = `2022` / 1000)
  

################################################################################
################################################################################
#task 7
################################################################################
################################################################################

MOM.beta.fn <- function(data, par){
  alpha <- par[1]
  beta  <- par[2]
  
  EX  <- alpha / (alpha + beta)
  EX2 <- ((alpha+1)*alpha)/((alpha+beta+1)*(alpha+beta))
  
  m1 <- mean(data, na.rm = T)
  m2 <- mean(data^2, na.rm = T)
  
  return(c(EX-m1, EX2-m2))
}


nleqslv(x = c(20,20), # guess
        fn = MOM.beta.fn,
        data=cntry.dth.rts$`2022`)




MLE.beta.fn <- function(data, par, neg=FALSE){
  alpha <- par[1]
  beta  <- par[2]
  loglik <- sum(log(dbeta(x=data, shape1 = alpha, shape2 = beta)), na.rm=T)
  
  return(ifelse(neg, -loglik, loglik))
}

optim(par = c(2,2),
      fn = MLE.beta.fn,
      data=cntry.dth.rts$`2022`,
      neg=T)


################################################################################
################################################################################
#task 8
################################################################################
################################################################################
all.estimates <- tibble()

for(i in 1:1000){
  set.seed(7272+i)
  
  new.samples <- rbeta(n = 266, # sample size
                                  shape1 = 8, # alpha parameter
                                  shape2 = 950)# beta parameter
  
  #MOM calculation
  MOM.results <- nleqslv(x = c(8,950), # guess
                         fn = MOM.beta.fn,
                         data= new.samples)
  MOM.alpha <- MOM.results$x[1]
  MOM.beta <- MOM.results$x[2]
  
  #MLE calculation
  MLE.results <- optim(par = c(8,950),
        fn = MLE.beta.fn,
        data=new.samples,
        neg=T)
  MLE.alpha <- MLE.results$par[1]
  MLE.beta <- MLE.results$par[2]
  
  
  all.estimates <- bind_rows(
    all.estimates,
    tibble(
      Simulation = i,
      MOM.alpha = MOM.alpha,
      MOM.beta  = MOM.beta,
      MLE.alpha = MLE.alpha,
      MLE.beta  = MLE.beta
    )
  )
  
}

#convert data into long format with clearer categories
all.estimates.long <- all.estimates |>
  pivot_longer(
    cols = c(MOM.alpha, MOM.beta, MLE.alpha, MLE.beta), 
    names_to = c("Method", "Parameter"),
    names_sep = "\\."
  )



######################################################
#plotting MLE and MOM

#Plot 1: MOM Alpha Estimates
p1 <- ggplot(all.estimates.long |>
               filter(Method == "MOM", Parameter == "alpha"), 
             aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "MOM Alpha Estimates", x = "Estimated Alpha", y = "Density") +
  theme_minimal()

#Plot 2: MOM Beta Estimates
p2 <- ggplot(all.estimates.long |>
               filter(Method == "MOM", Parameter == "beta"), 
             aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "MOM Beta Estimates", x = "Estimated Beta", y = "Density") +
  theme_minimal()

#Plot 3: MLE Alpha Estimates
p3 <- ggplot(all.estimates.long |>
               filter(Method == "MLE", Parameter == "alpha"), 
             aes(x = value)) +
  geom_density(fill = "red", alpha = 0.4) +
  labs(title = "MLE Alpha Estimates", x = "Estimated Alpha", y = "Density") +
  theme_minimal()

#Plot 4: MLE Beta Estimates
p4 <- ggplot(all.estimates.long |>
               filter(Method == "MLE", Parameter == "beta"), 
             aes(x = value)) +
  geom_density(fill = "red", alpha = 0.4) +
  labs(title = "MLE Beta Estimates", x = "Estimated Beta", y = "Density") +
  theme_minimal()


estimator.plot <- (p1 + p2)/
  (p3 + p4)

ggsave("estimator.plot.pdf", plot = estimator.plot, width = 10, height = 6)
#######################################
#values for plots

# True values of alpha and beta
true_alpha <- 8
true_beta  <- 950

# Compute Bias, Precision, and MSE for each estimator
summary.table <- tibble(
  Method = c("MOM", "MOM", "MLE", "MLE"),
  Parameter = c("Alpha", "Beta", "Alpha", "Beta"),
  
  # Bias
  Bias = c(
    mean(all.estimates$MOM.alpha, na.rm = TRUE) - true_alpha,
    mean(all.estimates$MOM.beta, na.rm = TRUE) - true_beta,
    mean(all.estimates$MLE.alpha, na.rm = TRUE) - true_alpha,
    mean(all.estimates$MLE.beta, na.rm = TRUE) - true_beta
  ),
  
  # Precision (inverse of variance)
  Precision = c(
    1 / var(all.estimates$MOM.alpha, na.rm = TRUE),
    1 / var(all.estimates$MOM.beta, na.rm = TRUE),
    1 / var(all.estimates$MLE.alpha, na.rm = TRUE),
    1 / var(all.estimates$MLE.beta, na.rm = TRUE)
  ),
  
  # Mean Squared Error (MSE)
  MSE = c(
    var(all.estimates$MOM.alpha, na.rm = TRUE) + Bias[1]^2,
    var(all.estimates$MOM.beta, na.rm = TRUE) + Bias[2]^2,
    var(all.estimates$MLE.alpha, na.rm = TRUE) + Bias[3]^2,
    var(all.estimates$MLE.beta, na.rm = TRUE) + Bias[4]^2
  )
)




