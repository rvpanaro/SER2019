rm(list=ls(all=TRUE))

##------------------ Simulated data ------------------
setwd("C:\\Users\\adria\\Google Drive\\Estudos_SER\\Segmented")
source('gerabd2.R')

##--------------Hyperparameters ------------

# sigma ~ Gamma( a_sigma, b_sigma )
a_sigma = .01
b_sigma = .01

# beta ~ N( mu_beta, s_beta )
mu_beta = 0
s_beta = 100


# psi1 ~ U( 0 , tau )
# psi2 ~ U( psi1, tau )
##------------------ Stan ------------------
# Clear the R workspace and load rstan. # rm(list=ls(all=TRUE))
library(rstan)

# avoid recompilations
rstan_options(auto_write = TRUE)

# run different chains in parallel.
options(mc.cores = parallel::detectCores())

##------------------- Simulation Study

pars = c("beta", "psi1", "psi2", "sigma")
n <- 100

# Prepare data for Stan
data = list( n = n, 
             a_sigma = a_sigma,  b_sigma = b_sigma,
             mu_beta = mu_beta,  s_beta = s_beta)

l = 100 # number of simulations
sigma  = 50

output1 <- list()
output2 <- list()
output3 <- list()
param1 <- matrix( NA, ncol = 7, nrow = l )
param2 <- matrix( NA, ncol = 7, nrow = l )
param3 <- matrix( NA, ncol = 7, nrow = l )

# simulation: 1 change point
count1 = 0
fail1 = 0
i = 0
while( count1 < l){
  i = i+ 1
  print(c(i, count1))
  # set.seed(1)
  sample0 <- gerabd(n = n, k = 0, xmax = 100, b01 = -20, b1 = 20, b2 = 5, sigma = sigma )
  data$x <- sample0$x;   data$y <- sample0$y;  data$tau = max(data$x)

  stan1 = stan(file = "be_pwlm2.stan", data=data,
                pars=pars, chains = 2, verbose=FALSE)

  if( sum( summary(stan1)$summary[,"Rhat"] < 1.1 ) == 7){
    count1 = count1 + 1
    output1[[count1]] = summary( stan1 )$summary
    param1[count1,] = summary(stan1)$summary[,"mean"]

  }else{
    fail1 = fail1 + 1
  }

}

simulation: 2 change point
count2 = 0
i = fail2 = 0
while( count2 < l){
  i = i+ 1
  print(c(i, count2))
  sample1 <- gerabd(n = n, k = 1, xmax = 100, b01 = -20, b1 = 20, b2 = 5, psi1 = 20, sigma = sigma)
  data$x <- sample1$x;   data$y <- sample1$y;  data$tau = max(data$x)

  stan2 = stan(file = "be_pwlm2.stan", data=data,
               pars=pars, chains = 2, verbose=FALSE)

  if( sum( summary(stan2)$summary[,"Rhat"] < 1.1 ) == 7){
    count2 = count2 + 1
    output2[[count2]] = summary( stan2 )$summary
    param2[count2,] = summary(stan2)$summary[,"mean"]

  }else{
    fail2 = fail2 + 1
  }

}


# simulation: 3 change point
count3 = 0
i = fail3 = 0
while( count3 < l){
  i = i+ 1
  print(c(i, count3))
  sample2 <- gerabd(n = n, k = 2, xmax = 100, b01 = -20, b1 = 20, b2 = 5, psi1 = 20, psi2 = 70,  sigma = sigma )
  data$x <- sample2$x;   data$y <- sample2$y;  data$tau = max(data$x)

  stan3 = stan(file = "be_pwlm2.stan", data=data,
               pars=pars, chains = 2, verbose=FALSE)

  if( sum( summary(stan3)$summary[,"Rhat"] < 1.1 ) == 7){
    count3 = count3 + 1
    output3[[count3]] = summary( stan3 )$summary
    param3[count3,] = summary(stan3)$summary[,"mean"]

  }else{
    fail3 = fail3 + 1
  }

}

write.csv(param1, 'k0_sigma_meio.csv', row.names = F)
write.csv(param2, 'k1_sigma_meio.csv', row.names = F)
write.csv(param3, 'k2_sigma_meio.csv', row.names = F)

theta = c( -20, 20, -15, 20, 70,  (sigma) ) # real parameters 

MAPE = function(parametro){
  erro = apply(parametro, 1, function(x){
    out = (theta - x[-7])/theta
    
    return(abs(out))
  })
  
  final = apply(erro, 1, mean)
  
  return(final)
}

# MAPE(param1)*100 # MAPE: simulation with 0 changepoint
# MAPE(param2)*100 # MAPE: simulation with 1 changepoint
# MAPE(param3)*100 # MAPE: simulation with 2 changepoint
# summary(param3[,1])
# c(sigma, fail1, fail2, fail3) # how many times we'he divergente models
# summary(param1[,4]) # distribution of psi1: simulation with 0 changepoint
# summary(param1[,5]) # distribution of psi2: simulation with 0 changepoint
# summary(param2[,5]) # distribution of psi2: simulation with 1 changepoint
# 
# tab <- rbind(MAPE(param1)*100, MAPE(param2)*100, MAPE(param3)*100)
# colnames(tab) <- c("beta0", "beta1", "beta2", "psi1", "psi2", "sigma")
# rownames(tab) <- c("k=0", "k=1", "k=2")
# tab
# 
# write.csv(tab, "sigma50_sim.csv")