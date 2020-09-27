# set seed
set.seed(4490, kind = "L'Ecuyer-CMRG")

# load packages
library(SuperLearner)
library(hitandrun)
library(rpart)
library(parallel)
library(SL.ODTR)
library(dplyr)

cores = detectCores()


r = 1000

alphas_to_try = seq(from = 0, to = 1, by = .01)

risk.type = "CV TMLE"
alpha_psi = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = alphas_to_try, cs_to_try = NULL), mc.cores = cores)
alpha0_psi = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = 0, cs_to_try = NULL), mc.cores = cores)

save(alpha_psi, alpha0_psi, file = "alpha_psi.RData")

risk.type = "CV TMLE CI"
alpha_upperCI = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = alphas_to_try, cs_to_try = NULL), mc.cores = cores)
alpha0_upperCI = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = 0, cs_to_try = NULL), mc.cores = cores)

save(alpha_upperCI, alpha0_upperCI, file = "alpha_upperCI.RData")





