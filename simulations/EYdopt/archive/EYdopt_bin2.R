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
VFolds = 10 # not 100% sure i did it like this...





QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart", "SL.randomForest")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart", "SL.randomForest")
EYdoptbin_MLaggglms = do.call("rbind", mclapply(1:r, function(x) performance_EYdopt(n = n,
                                                                                 DGP_fun = DGP_fun,
                                                                                 QAW = QAW,
                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                 blip.SL.library = blip.SL.library,
                                                                                 VFolds = VFolds), mc.cores = cores))
save(EYdoptbin_MLaggglms,
     file = "results/EYdoptbin_MLaggglms.RData")



