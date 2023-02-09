# set seed
set.seed(4490, kind = "L'Ecuyer-CMRG")

# load packages
library(SuperLearner)
library(hitandrun)
library(parallel)
library(SL.ODTR)


r = 1000
n = 1000
QAW.SL.library = "SL.QAW.HTEepi"
blip.SL.library = "SL.blip.HTEepi"

#### all pos discrete W 0.1 ####
EYgRC_allpos_discreteW_k0.1 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                          kappa = 0.1,
                                                                          n = n,
                                                                          DGP_fun = DGP.rc.discreteW,
                                                                          QAW.fun = QAW.rc.allpos,
                                                                          QAW.SL.library = QAW.SL.library,
                                                                          blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_allpos_discreteW_k0.1 done")



#### all pos discrete W 0.9 ####
EYgRC_allpos_discreteW_k0.9 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                          kappa = 0.9,
                                                                          n = n,
                                                                          DGP_fun = DGP.rc.discreteW,
                                                                          QAW.fun = QAW.rc.allpos,
                                                                          QAW.SL.library = QAW.SL.library,
                                                                          blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_allpos_discreteW_k0.9 done")





#### some pos discrete W 0.1 ####
EYgRC_somepos_discreteW_k0.1 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                          kappa = 0.1,
                                                                          n = n,
                                                                          DGP_fun = DGP.rc.discreteW,
                                                                          QAW.fun = QAW.rc.somepos,
                                                                          QAW.SL.library = QAW.SL.library,
                                                                          blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_somepos_discreteW_k0.1 done")



#### some pos discrete W 0.9 ####
EYgRC_somepos_discreteW_k0.9 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                           kappa = 0.9,
                                                                           n = n,
                                                                           DGP_fun = DGP.rc.discreteW,
                                                                           QAW.fun = QAW.rc.somepos,
                                                                           QAW.SL.library = QAW.SL.library,
                                                                           blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_somepos_discreteW_k0.9 done")




#### no pos discrete W 0.1 ####
EYgRC_nopos_discreteW_k0.1 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                           kappa = 0.1,
                                                                           n = n,
                                                                           DGP_fun = DGP.rc.discreteW,
                                                                           QAW.fun = QAW.rc.nopos,
                                                                           QAW.SL.library = QAW.SL.library,
                                                                           blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_nopos_discreteW_k0.1 done")



#### no pos discrete W 0.9 ####
EYgRC_nopos_discreteW_k0.9 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                           kappa = 0.9,
                                                                           n = n,
                                                                           DGP_fun = DGP.rc.discreteW,
                                                                           QAW.fun = QAW.rc.nopos,
                                                                           QAW.SL.library = QAW.SL.library,
                                                                           blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_nopos_discreteW_k0.9 done")



#### all pos cont W 0.1 ####
EYgRC_allpos_contW_k0.1 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                          kappa = 0.1,
                                                                          n = n,
                                                                          DGP_fun = DGP.rc.contW,
                                                                          QAW.fun = QAW.rc.allpos,
                                                                          QAW.SL.library = QAW.SL.library,
                                                                          blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_allpos_contW_k0.1 done")



#### all pos cont W 0.9 ####
EYgRC_allpos_contW_k0.9 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                          kappa = 0.9,
                                                                          n = n,
                                                                          DGP_fun = DGP.rc.contW,
                                                                          QAW.fun = QAW.rc.allpos,
                                                                          QAW.SL.library = QAW.SL.library,
                                                                          blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_allpos_contW_k0.9 done")





#### some pos cont W 0.1 ####
EYgRC_somepos_contW_k0.1 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                           kappa = 0.1,
                                                                           n = n,
                                                                           DGP_fun = DGP.rc.contW,
                                                                           QAW.fun = QAW.rc.somepos,
                                                                           QAW.SL.library = QAW.SL.library,
                                                                           blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_somepos_contW_k0.1 done")



#### some pos cont W 0.9 ####
EYgRC_somepos_contW_k0.9 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                           kappa = 0.9,
                                                                           n = n,
                                                                           DGP_fun = DGP.rc.contW,
                                                                           QAW.fun = QAW.rc.somepos,
                                                                           QAW.SL.library = QAW.SL.library,
                                                                           blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_somepos_contW_k0.9 done")




#### no pos cont W 0.1 ####
EYgRC_nopos_contW_k0.1 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                         kappa = 0.1,
                                                                         n = n,
                                                                         DGP_fun = DGP.rc.contW,
                                                                         QAW.fun = QAW.rc.nopos,
                                                                         QAW.SL.library = QAW.SL.library,
                                                                         blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_nopos_contW_k0.1 done")



#### no pos cont W 0.9 ####
EYgRC_nopos_contW_k0.9 = mclapply(1:r, function(x) performance_EYgRC(x = x,
                                                                         kappa = 0.9,
                                                                         n = n,
                                                                         DGP_fun = DGP.rc.contW,
                                                                         QAW.fun = QAW.rc.nopos,
                                                                         QAW.SL.library = QAW.SL.library,
                                                                         blip.SL.library = blip.SL.library), mc.cores = detectCores())
print("EYgRC_nopos_contW_k0.9 done")


save(EYgRC_allpos_discreteW_k0.1, EYgRC_allpos_discreteW_k0.9,
     EYgRC_somepos_discreteW_k0.1, EYgRC_somepos_discreteW_k0.9,
     EYgRC_nopos_discreteW_k0.1, EYgRC_nopos_discreteW_k0.1,
     EYgRC_allpos_contW_k0.1, EYgRC_allpos_contW_k0.9,
     EYgRC_somepos_contW_k0.1, EYgRC_somepos_contW_k0.9,
     EYgRC_nopos_contW_k0.1, EYgRC_nopos_contW_k0.1, file = "results_EYgRC.RData")
