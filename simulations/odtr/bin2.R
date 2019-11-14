# set seed
set.seed(4490, kind = "L'Ecuyer-CMRG")

# load packages
library(SuperLearner)
library(hitandrun)
library(DynTxRegime)
library(rpart)
library(parallel)
library(SL.ODTR)

cores = 24


r = 1000
n = 1000
QAW = QAW_bin
DGP_fun = DGP_bin
SL.full.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")



ODTR_incorrectglm_NA_NA = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                      risk.type = "CV TMLE",
                                                                                      DGP_fun = DGP_fun,
                                                                                      QAW = QAW,
                                                                                      QAW.SL.library = "SL.QAW.incorrect",
                                                                                      blip.SL.library = "SL.glm",
                                                                                      dopt.SL.library = "DonV",
                                                                                      metalearner = "blip",
                                                                                      moMain_model = NULL, moCont_model = NULL), mc.cores = cores))
ODTR_bliponly_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                      risk.type = "CV MSE",
                                                                                      DGP_fun = DGP_fun,
                                                                                      QAW = QAW,
                                                                                      QAW.SL.library = SL.full.library,
                                                                                      blip.SL.library = SL.full.library,
                                                                                      dopt.SL.library = NULL,
                                                                                      metalearner = "blip",
                                                                                      moMain_model = NULL, moCont_model = NULL,
                                                                                      discrete.SL = T), mc.cores = cores))
ODTR_bliponly_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = "CV TMLE",
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = SL.full.library,
                                                                                           blip.SL.library = SL.full.library,
                                                                                           dopt.SL.library = NULL,
                                                                                           metalearner = "blip",
                                                                                           moMain_model = NULL, moCont_model = NULL,
                                                                                           discrete.SL = T), mc.cores = cores))
ODTR_bliponly_discrete_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                            risk.type = "CV TMLE CI",
                                                                                            DGP_fun = DGP_fun,
                                                                                            QAW = QAW,
                                                                                            QAW.SL.library = SL.full.library,
                                                                                            blip.SL.library = SL.full.library,
                                                                                            dopt.SL.library = NULL,
                                                                                            metalearner = "blip",
                                                                                            moMain_model = NULL, moCont_model = NULL,
                                                                                            discrete.SL = T), mc.cores = cores))


ODTR_bliponly_blipmeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                              risk.type = "CV TMLE CI",
                                                                                              DGP_fun = DGP_fun,
                                                                                              QAW = QAW,
                                                                                              QAW.SL.library = SL.full.library,
                                                                                              blip.SL.library = SL.full.library,
                                                                                              dopt.SL.library = NULL,
                                                                                              metalearner = "blip",
                                                                                              moMain_model = NULL, moCont_model = NULL,
                                                                                              discrete.SL = T), mc.cores = cores))




QAW.SL.library = "SL.QAW.incorrect"
blip.SL.library = "SL.glm"
risk.type = "CV MSE"
discrete.SL = T
ODTR_bliponly_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, metalearner = metalearner, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size, discrete.SL = discrete.SL), mc.cores = cores))




save(ODTR_incorrectglm_NA_NA, file = "results/ODTR_step5a_bin.RData")



risk.type = "CV TMLE"
ODTR_TMLE_step5a_bin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, metalearner = metalearner, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
risk.type = "CV MSE"
ODTR_MSE_step5a_bin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, metalearner = metalearner, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))









##### Vote ####
metalearner = "vote"

#### Vote restricted ####
dopt.SL.library = "DonV"

##### QAW and blip SL #####
moMain_model = NULL
moCont_model = NULL
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = QAW.SL.library
risk.type = "CV TMLE"
ODTR_TMLE_step1c_bin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, metalearner = metalearner, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
save(ODTR_TMLE_step1c_bin,
     file = "results/ODTR_step1c_bin.RData")

#### Vote expanded ####
dopt.SL.library = "all"

##### QAW and blip SL #####
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = QAW.SL.library
risk.type = "CV TMLE"
ODTR_TMLE_step1b_bin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, metalearner = metalearner, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
save(ODTR_TMLE_step1b_bin,
     file = "results/ODTR_step1b_bin.RData")







##### Blip ####
metalearner = "blip"

##### QAW and blip glm incorrect #####
QAW.SL.library = "SL.QAW.incorrect"
blip.SL.library = "SL.glm"
risk.type = "CV TMLE"
ODTR_TMLE_step5a_bin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, metalearner = metalearner, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
risk.type = "CV MSE"
ODTR_MSE_step5a_bin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, metalearner = metalearner, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
save(ODTR_TMLE_step5a_bin,
     ODTR_MSE_step5a_bin,
     file = "results/ODTR_step5a_bin.RData")

##### QAW and blip SL #####
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = QAW.SL.library
risk.type = "CV TMLE"
ODTR_TMLE_step1a_bin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, metalearner = metalearner, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
risk.type = "CV MSE"
ODTR_MSE_step1a_bin = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n, risk.type = risk.type, DGP_fun = DGP_fun, QAW = QAW, QAW.SL.library = QAW.SL.library, blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library, metalearner = metalearner, moMain_model = moMain_model, moCont_model = moCont_model, grid.size = grid.size), mc.cores = cores))
save(ODTR_TMLE_step1a_bin,
     ODTR_MSE_step1a_bin,
     file = "results/ODTR_step1a_bin.RData")
