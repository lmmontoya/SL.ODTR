# set seed
set.seed(4490, kind = "L'Ecuyer-CMRG")

# load packages
library(SuperLearner)
library(hitandrun)
library(parallel)
library(SL.ODTR)


r = 1000
n = 1000
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")


#### AL1 0.1 ####
EYgRC_AL1_k0.1 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                             kappa = 0.1,
                                                             n = n,
                                                             DGP_fun = DGP_bin_complex,
                                                             QAW.fun = QAW_bin_complex,
                                                             QAW.SL.library = QAW.SL.library,
                                                             blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_AL1_k0.1 done")

EYgRC_AL1_k0.9 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                             kappa = 0.9,
                                                             n = n,
                                                             DGP_fun = DGP_bin_complex,
                                                             QAW.fun = QAW_bin_complex,
                                                             QAW.SL.library = QAW.SL.library,
                                                             blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_AL1_k0.9 done")



#### AL2 0.1 ####
QAW.SL.library = c("SL.QAW.HTEepi",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.HTEepi",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
EYgRC_AL2_k0.1 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                             kappa = 0.1,
                                                             n = n,
                                                             DGP_fun = DGP_AL_RC,
                                                             QAW.fun = QAW_AL_RC,
                                                             QAW.SL.library = QAW.SL.library,
                                                             blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_AL2_k0.1 done")

EYgRC_AL2_k0.9 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                             kappa = 0.9,
                                                             n = n,
                                                             DGP_fun = DGP_AL_RC,
                                                             QAW.fun = QAW_AL_RC,
                                                             QAW.SL.library = QAW.SL.library,
                                                             blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_AL2_k0.9 done")



save(EYgRC_AL1_k0.1, EYgRC_AL1_k0.9,
     EYgRC_AL2_k0.1, EYgRC_AL2_k0.9, file = "results_EYgRC_incorrect.RData")
