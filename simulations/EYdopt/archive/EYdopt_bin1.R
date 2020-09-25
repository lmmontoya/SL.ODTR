# set seed
set.seed(4490, kind = "L'Ecuyer-CMRG")

# load packages
library(SuperLearner)
library(hitandrun)
library(DynTxRegime)
library(rpart)
library(parallel)
library(SL.ODTR)
library(ltmle)

cores = 24


r = 1000
n = 1000
QAW = QAW_bin
DGP_fun = DGP_bin
VFolds = 10 



QAW.SL.library = "SL.QAW.incorrect"
blip.SL.library = "SL.glm"
EYdoptbin_glm = do.call("rbind", mclapply(1:r, function(x) performance_EYdopt(n = n,
                                                                              DGP_fun = DGP_fun,
                                                                              QAW = QAW,
                                                                              QAW.SL.library = QAW.SL.library,
                                                                              blip.SL.library = blip.SL.library,
                                                                              VFolds = VFolds), mc.cores = cores))
save(EYdoptbin_glm,
     file = "results/EYdoptbin_glm.RData")



QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4")
EYdoptbin_glms = do.call("rbind", mclapply(1:r, function(x) performance_EYdopt(n = n,
                                                                               DGP_fun = DGP_fun,
                                                                               QAW = QAW,
                                                                               QAW.SL.library = QAW.SL.library,
                                                                               blip.SL.library = blip.SL.library,
                                                                               VFolds = VFolds), mc.cores = cores))
save(EYdoptbin_glms,
     file = "results/EYdoptbin_glms.RData")






QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
EYdoptbin_MLnotaggglms = do.call("rbind", mclapply(1:r, function(x) performance_EYdopt(n = n,
                                                                                       DGP_fun = DGP_fun,
                                                                                       QAW = QAW,
                                                                                       QAW.SL.library = QAW.SL.library,
                                                                                       blip.SL.library = blip.SL.library,
                                                                                       VFolds = VFolds), mc.cores = cores))
save(EYdoptbin_MLnotaggglms,
     file = "results/EYdoptbin_MLnotaggglms.RData")



