# set seed
set.seed(4490, kind = "L'Ecuyer-CMRG")

# load packages
library(SuperLearner)
library(hitandrun)
library(DynTxRegime)
library(rpart)
library(parallel)
library(SL.ODTR)
library(MASS)

cores = 24


r = 1000
n = 300
QAW = QAW_bin_dep
DGP_fun = DGP_bin_dep



risk.type = "CV MSE"
QAW.SL.library = "SL.QAW.HTEepi"
blip.SL.library = "SL.blip.HTEepi"
dopt.SL.library = NULL
metalearner = "blip"
ODTRsimple_incorrectglm_NA_NA = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                          risk.type = risk.type,
                                                                                          DGP_fun = DGP_fun,
                                                                                          QAW = QAW,
                                                                                          QAW.SL.library = QAW.SL.library,
                                                                                          blip.SL.library = blip.SL.library,
                                                                                          dopt.SL.library = dopt.SL.library,
                                                                                          metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_incorrectglm_NA_NA,
     file = "results/ODTRsimple_GLM_n300.RData")



risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRsimple_bliponlyparam_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRsimple_bliponlyparam_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_bliponlyparam_discrete_CVMSE,
     ODTRsimple_bliponlyparam_discrete_CVTMLE,
     file = "results/ODTRsimple_bliponlyparam_discrete_n300.RData")








risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4")
dopt.SL.library = NULL
metalearner = "blip"
ODTRsimple_bliponlyparam_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRsimple_bliponlyparam_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_bliponlyparam_blipmeta_CVMSE,
     ODTRsimple_bliponlyparam_blipmeta_CVTMLE,
     file = "results/ODTRsimple_bliponlyparam_blipmeta_n300.RData")








QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4")
dopt.SL.library = "DonV"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRsimple_bliponlyparam_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_bliponlyparam_votemeta_CVTMLE,
     file = "results/ODTRsimple_bliponlyparam_votemeta_n300.RData")









risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRsimple_bliponlyML_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRsimple_bliponlyML_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_bliponlyML_discrete_CVMSE,
     ODTRsimple_bliponlyML_discrete_CVTMLE,
     file = "results/ODTRsimple_bliponlyML_discrete_n300.RData")










risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "blip"
ODTRsimple_bliponlyML_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRsimple_bliponlyML_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_bliponlyML_blipmeta_CVMSE,
     ODTRsimple_bliponlyML_blipmeta_CVTMLE,
     file = "results/ODTRsimple_bliponlyML_blipmeta_n300.RData")










risk.type = "CV TMLE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "DonV"
metalearner = "vote"
ODTRsimple_bliponlyML_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_bliponlyML_votemeta_CVTMLE,
     file = "results/ODTRsimple_bliponlyML_votemeta_n300.RData")











QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "discrete"
risk.type = "CV TMLE"
ODTRsimple_all_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_all_discrete_CVTMLE,
     file = "results/ODTRsimple_all_discrete_n300.RData")














QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRsimple_all_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_all_votemeta_CVTMLE,
     file = "results/ODTRsimple_all_votemeta_n300.RData")




























risk.type = "CV MSE"
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRsimple_bliponlyMLonly_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                risk.type = risk.type,
                                                                                                DGP_fun = DGP_fun,
                                                                                                QAW = QAW,
                                                                                                QAW.SL.library = QAW.SL.library,
                                                                                                blip.SL.library = blip.SL.library,
                                                                                                dopt.SL.library = dopt.SL.library,
                                                                                                metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRsimple_bliponlyMLonly_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_bliponlyMLonly_discrete_CVMSE,
     ODTRsimple_bliponlyMLonly_discrete_CVTMLE,
     file = "results/ODTRsimple_bliponlyMLonly_discrete_n300.RData")










risk.type = "CV MSE"
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "blip"
ODTRsimple_bliponlyMLonly_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                risk.type = risk.type,
                                                                                                DGP_fun = DGP_fun,
                                                                                                QAW = QAW,
                                                                                                QAW.SL.library = QAW.SL.library,
                                                                                                blip.SL.library = blip.SL.library,
                                                                                                dopt.SL.library = dopt.SL.library,
                                                                                                metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRsimple_bliponlyMLonly_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_bliponlyMLonly_blipmeta_CVMSE,
     ODTRsimple_bliponlyMLonly_blipmeta_CVTMLE,
     file = "results/ODTRsimple_bliponlyMLonly_blipmeta_n300.RData")










risk.type = "CV TMLE"
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "DonV"
metalearner = "vote"
ODTRsimple_bliponlyMLonly_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_bliponlyMLonly_votemeta_CVTMLE,
     file = "results/ODTRsimple_bliponlyMLonly_votemeta_n300.RData")












QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "discrete"
risk.type = "CV TMLE"
ODTRsimple_MLonlyEYd_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                          risk.type = risk.type,
                                                                                          DGP_fun = DGP_fun,
                                                                                          QAW = QAW,
                                                                                          QAW.SL.library = QAW.SL.library,
                                                                                          blip.SL.library = blip.SL.library,
                                                                                          dopt.SL.library = dopt.SL.library,
                                                                                          metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_MLonlyEYd_discrete_CVTMLE,
     file = "results/ODTRsimple_MLonlyEYd_discrete_n300.RData")














QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRsimple_MLonlyEYd_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                          risk.type = risk.type,
                                                                                          DGP_fun = DGP_fun,
                                                                                          QAW = QAW,
                                                                                          QAW.SL.library = QAW.SL.library,
                                                                                          blip.SL.library = blip.SL.library,
                                                                                          dopt.SL.library = dopt.SL.library,
                                                                                          metalearner = metalearner), mc.cores = cores))
save(ODTRsimple_MLonlyEYd_votemeta_CVTMLE,
     file = "results/ODTRsimple_MLonlyEYd_votemeta_n300.RData")
