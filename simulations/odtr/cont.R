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
QAW = QAW_AL_cont
DGP_fun = DGP_AL_cont
dopt.SL.library = "all"
grid.size = 1000


##### Vote ####
SL.type = "vote"

##### QAW and blip correctly specified #####
QAW.SL.library = "SL.QAW.correct_cont"
blip.SL.library = "SL.QAV.correct_cont"
moMain_model = "W1 + W2"
moCont_model = "W2:W3 + W2:W2 + W3"
risk.type = "CV TMLE"
ODTR_TMLE_step3b_cont = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
save(ODTR_TMLE_step3b_cont,
     file = "results/ODTR_step3b_cont.RData")


##### QAW and blip SL #####
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = QAW.SL.library
moMain_model = NULL
moCont_model = NULL
risk.type = "CV TMLE"
ODTR_TMLE_step1b_cont = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
save(ODTR_TMLE_step1b_cont,
     file = "results/ODTR_step1b_cont.RData")

##### blip library only SL #####
dopt.SL.library = "DonV"
ODTR_TMLE_step1c_cont = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
save(ODTR_TMLE_step1c_cont,
     file = "results/ODTR_step1c_cont.RData")





##### Blip ####
SL.type = "blip"

##### QAW and blip correctly specified #####
QAW.SL.library = "SL.QAW.correct_cont"
blip.SL.library = "SL.QAV.correct_cont"
risk.type = "CV TMLE"
ODTR_TMLE_step3a_cont = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
risk.type = "CV MSE"
ODTR_MSE_step3a_cont = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
save(ODTR_TMLE_step3a_cont,
     ODTR_MSE_step3a_cont,
     file = "results/ODTR_step3a_cont.RData")

##### QAW and blip glm incorrect #####
QAW.SL.library = "SL.QAW.incorrect"
blip.SL.library = "SL.glm"
risk.type = "CV TMLE"
ODTR_TMLE_step5a_cont = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
risk.type = "CV MSE"
ODTR_MSE_step5a_cont = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
save(ODTR_TMLE_step5a_cont,
     ODTR_MSE_step5a_cont,
     file = "results/ODTR_step5a_cont.RData")

##### QAW and blip SL #####
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = QAW.SL.library
risk.type = "CV TMLE"
ODTR_TMLE_step1a_cont = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
risk.type = "CV MSE"
ODTR_MSE_step1a_cont = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, SL.type = SL.type, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
save(ODTR_TMLE_step1a_cont,
     ODTR_MSE_step1a_cont,
     file = "results/ODTR_step1a_cont.RData")
