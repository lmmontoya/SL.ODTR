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

cs_to_try = seq(from = 0, to = 10, by = .1)

risk.type = "CV TMLE"
c_psi = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, cs_to_try = cs_to_try, alphas_to_try = NULL), mc.cores = cores)
c0_psi = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, cs_to_try = 0, alphas_to_try = NULL), mc.cores = cores)

save(c_psi, c0_psi, file = "c_psi.RData")

risk.type = "CV TMLE CI"
c_upperCI = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, cs_to_try = cs_to_try, alphas_to_try = NULL), mc.cores = cores)
c0_upperCI = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, cs_to_try = 0, alphas_to_try = NULL), mc.cores = cores)

save(c_upperCI, c0_upperCI, file = "c_upperCI.RData")





