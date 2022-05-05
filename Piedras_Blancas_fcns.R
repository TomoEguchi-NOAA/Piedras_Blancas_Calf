
library(rstan)
library(loo)
# extract MCMC diagnostic statistics, including Rhat, loglikelihood, DIC, and LOOIC
MCMC.diag <- function(jm, MCMC.params){
  n.per.chain <- (MCMC.params$n.samples - MCMC.params$n.burnin)/MCMC.params$n.thin
  
  Rhat <- unlist(lapply(jm$Rhat, FUN = max, na.rm = T))
  
  loglik <- jm$sims.list$loglik
  
  Reff <- relative_eff(exp(loglik), 
                       chain_id = rep(1:MCMC.params$n.chains, 
                                      each = n.per.chain),
                       cores = 1)
  loo.out <- loo(loglik, r_eff = Reff, cores = 1)
  
  return(list(DIC = jm$DIC,
              loglik.obs = loglik,
              Reff = Reff,
              Rhat = Rhat,
              loo.out = loo.out))
  
}
