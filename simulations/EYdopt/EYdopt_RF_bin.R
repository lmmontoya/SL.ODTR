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
QAW = QAW_AL_bin
DGP_fun = DGP_AL_bin
VFolds = 5


##### QAW and blip SL data adaptive #####
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.glmnet", "SL.earth", "SL.nnet", "SL.randomForest", "SL.rpart")
blip.SL.library = QAW.SL.library
EYdopt_step5_bin = do.call("rbind", mclapply(1:r, function(x) performance_EYdopt(x, n = n, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, VFolds = VFolds), mc.cores = cores))
save(EYdopt_step5_bin,
     file = "results/EYdopt_step5_bin.RData")

