# set seed
set.seed(4490, kind = "L'Ecuyer-CMRG")

# load packages
library(SuperLearner)
library(hitandrun)
library(DynTxRegime)
library(rpart)
library(parallel)

cores = 24

# load functions
source('R/1DGPfunctions.R')
source('R/2helperFunctions.R')
source('R/3SLodtr.R')
source('R/4odtr.R')
source('R/6performanceFunctions.R')


r = 1000
n = 1000
QAW = QAW_sin
DGP_fun = DGP_sin
dopt.SL.library = "all"


##### Vote ####
SL.type = "vote"

##### QAW and blip glm incorrect #####
QAW.SL.library = "SL.QAW.incorrect"
blip.SL.library = "SL.glm"
moMain_model = NULL
moCont_model = NULL
risk.type = "CV TMLE"
ODTR_TMLE_step5b_sin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model), mc.cores = cores))
ODTR_TMLE_step5b_sin = do.call("rbind", lapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model)))

#risk.type = "CV IPCWDR"
#ODTR_IPCW_step5b_sin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model), mc.cores = cores))
save(ODTR_TMLE_step5b_sin,
     #ODTR_IPCW_step5b_sin,
     file = "ODTR_step5b_sin.RData")

##### QAW and blip SL #####
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = QAW.SL.library
risk.type = "CV TMLE"
ODTR_TMLE_step1b_sin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model), mc.cores = cores))
#risk.type = "CV IPCWDR"
#ODTR_IPCW_step1b_sin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model), mc.cores = cores))
save(ODTR_TMLE_step1b_sin,
     #ODTR_IPCW_step1b_sin,
     file = "ODTR_step1b_sin.RData")







##### Blip ####
SL.type = "blip"

##### QAW and blip glm incorrect #####
QAW.SL.library = "SL.QAW.incorrect"
blip.SL.library = "SL.glm"
risk.type = "CV TMLE"
ODTR_TMLE_step5a_sin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model), mc.cores = cores))
#risk.type = "CV IPCWDR"
#ODTR_IPCW_step5a_sin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model), mc.cores = cores))
risk.type = "CV MSE"
ODTR_MSE_step5a_sin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model), mc.cores = cores))
save(ODTR_TMLE_step5a_sin,
     #ODTR_IPCW_step5a_sin,
     ODTR_MSE_step5a_sin,
     file = "ODTR_step5a_sin.RData")

##### QAW and blip SL #####
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = QAW.SL.library
risk.type = "CV TMLE"
ODTR_TMLE_step1a_sin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model), mc.cores = cores))
#risk.type = "CV IPCWDR"
#ODTR_IPCW_step1a_sin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model), mc.cores = cores))
risk.type = "CV MSE"
ODTR_MSE_step1a_sin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model), mc.cores = cores))
save(ODTR_TMLE_step1a_sin,
     #ODTR_IPCW_step1a_sin,
     ODTR_MSE_step1a_sin,
     file = "ODTR_step1a_sin.RData")
