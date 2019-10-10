# set seed
set.seed(4490, kind = "L'Ecuyer-CMRG")

# load packages
library(SuperLearner)
library(hitandrun)
library(DynTxRegime)
library(rpart)
library(parallel)
library(ltmle)

cores = 24

# load functions
source('R/1DGPfunctions.R')
source('R/2helperFunctions.R')
source('R/3SLodtr.R')
source('R/4odtr.R')
source('R/5EYdopt.R')
source('R/6performanceFunctions.R')


r = 1000
n = 1000
QAW = QAW_smooth2
DGP_fun = DGP_smooth2
VFolds = 5


##### QAW and blip glm correct #####
QAW.SL.library = "SL.QAW.correct_smooth"
blip.SL.library = "SL.blip.correct_smooth"
EYdopt_step1_smooth2 = do.call("rbind", mclapply(1:r, function(x) performance_EYdopt(x, n = n, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, VFolds = VFolds), mc.cores = cores))
save(EYdopt_step1_smooth2,
     file = "results/EYdopt_step1_smooth2.RData")

##### QAW and blip glm incorrect #####
QAW.SL.library = "SL.QAW.incorrect"
blip.SL.library = "SL.glm"
EYdopt_step2_smooth2 = do.call("rbind", mclapply(1:r, function(x) performance_EYdopt(x, n = n, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, VFolds = VFolds), mc.cores = cores))
save(EYdopt_step2_smooth2,
     file = "results/EYdopt_step2_smooth2.RData")

##### QAW and blip SL not data adaptive #####
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glmnet")
blip.SL.library = QAW.SL.library
EYdopt_step3_smooth2 = do.call("rbind", mclapply(1:r, function(x) performance_EYdopt(x, n = n, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, VFolds = VFolds), mc.cores = cores))
save(EYdopt_step3_smooth2,
     file = "results/EYdopt_step3_smooth2.RData")

##### QAW and blip SL data adaptive #####
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.glmnet", "SL.earth", "SL.nnet", "SL.rpart")
blip.SL.library = QAW.SL.library
EYdopt_step4_smooth2 = do.call("rbind", mclapply(1:r, function(x) performance_EYdopt(x, n = n, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, VFolds = VFolds), mc.cores = cores))
save(EYdopt_step4_smooth2,
     file = "results/EYdopt_step4_smooth2.RData")

##### QAW and blip SL data adaptive #####
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.glmnet", "SL.earth", "SL.nnet", "SL.randomForest", "SL.rpart")
blip.SL.library = QAW.SL.library
EYdopt_step5_smooth2 = do.call("rbind", mclapply(1:r, function(x) performance_EYdopt(x, n = n, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, VFolds = VFolds), mc.cores = cores))
save(EYdopt_step5_smooth2,
     file = "results/EYdopt_step5_smooth2.RData")

