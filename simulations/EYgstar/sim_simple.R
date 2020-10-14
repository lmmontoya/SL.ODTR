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
cs_to_try = seq(from = 0, to = 10, by = .1)

QAW.fun = QAW_bin_simple
DGP_fun = DGP_bin_simple

##### psi risk ####

risk.type = "CV TMLE"

QAW.SL.library = "SL.glm"
blip.SL.library = "SL.glm"
alpha_psi_glm = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = alphas_to_try, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
c_psi_glm = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, cs_to_try = cs_to_try, alphas_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
dn_psi_glm = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = 0, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
save(alpha_psi_glm, c_psi_glm, dn_psi_glm, file = "results/psi_glm_complex.RData")

blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.rpart")
alpha_psi_glmSL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = alphas_to_try, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
c_psi_glmSL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, cs_to_try = cs_to_try, alphas_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
dn_psi_glmSL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = 0, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
save(alpha_psi_glmSL, c_psi_glmSL, dn_psi_glmSL, file = "results/psi_glmSL_complex.RData")

QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.rpart")
alpha_psi_SL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = alphas_to_try, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
c_psi_SL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, cs_to_try = cs_to_try, alphas_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
dn_psi_SL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = 0, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
save(alpha_psi_SL, c_psi_SL, dn_psi_SL, file = "results/psi_SL_complex.RData")








##### CI risk ####

risk.type = "CV TMLE CI"

QAW.SL.library = "SL.glm"
blip.SL.library = "SL.glm"
alpha_CI_glm = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = alphas_to_try, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
c_CI_glm = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, cs_to_try = cs_to_try, alphas_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
dn_CI_glm = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = 0, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
save(alpha_CI_glm, c_CI_glm, dn_CI_glm, file = "results/CI_glm_complex.RData")

blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.rpart")
alpha_CI_glmSL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = alphas_to_try, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
c_CI_glmSL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, cs_to_try = cs_to_try, alphas_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
dn_CI_glmSL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = 0, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
save(alpha_CI_glmSL, c_CI_glmSL, dn_CI_glmSL, file = "results/CI_glmSL_complex.RData")

QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.rpart")
alpha_CI_SL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = alphas_to_try, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
c_CI_SL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, cs_to_try = cs_to_try, alphas_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
dn_CI_SL = mclapply(1:r, function(x) performance_EYgstar(x = x, risk.type = risk.type, alphas_to_try = 0, cs_to_try = NULL, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, DGP_fun = DGP_fun, QAW.fun = QAW.fun), mc.cores = cores)
save(alpha_CI_SL, c_CI_SL, dn_CI_SL, file = "results/CI_SL_complex.RData")
