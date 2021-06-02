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
library(earth)

cores = 24


r = 1000
n = 1000
QAW = QAW_bin_dep
DGP_fun = DGP_bin_dep



risk.type = "CV MSE"
QAW.SL.library = "SL.QAW.HTEepi"
blip.SL.library = "SL.blip.HTEepi"
dopt.SL.library = NULL
metalearner = "blip"
ODTRdep_incorrectglm_NA_NA = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x = x, n = n,
                                                                                          risk.type = risk.type,
                                                                                          DGP_fun = DGP_fun,
                                                                                          QAW = QAW,
                                                                                          QAW.SL.library = QAW.SL.library,
                                                                                          blip.SL.library = blip.SL.library,
                                                                                          dopt.SL.library = dopt.SL.library,
                                                                                          metalearner = metalearner), mc.cores = cores))
save(ODTRdep_incorrectglm_NA_NA,
     file = "results/ODTRdep_GLM_n1000.RData")



risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRdep_bliponlyparam_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRdep_bliponlyparam_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
save(ODTRdep_bliponlyparam_discrete_CVMSE,
     ODTRdep_bliponlyparam_discrete_CVTMLE,
     file = "results/ODTRdep_bliponlyparam_discrete_n1000.RData")








risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4")
dopt.SL.library = NULL
metalearner = "blip"
ODTRdep_bliponlyparam_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRdep_bliponlyparam_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
save(ODTRdep_bliponlyparam_blipmeta_CVMSE,
     ODTRdep_bliponlyparam_blipmeta_CVTMLE,
     file = "results/ODTRdep_bliponlyparam_blipmeta_n1000.RData")








QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4")
dopt.SL.library = "DonV"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRdep_bliponlyparam_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
save(ODTRdep_bliponlyparam_votemeta_CVTMLE,
     file = "results/ODTRdep_bliponlyparam_votemeta_n1000.RData")









risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRdep_bliponlyML_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRdep_bliponlyML_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
save(ODTRdep_bliponlyML_discrete_CVMSE,
     ODTRdep_bliponlyML_discrete_CVTMLE,
     file = "results/ODTRdep_bliponlyML_discrete_n1000.RData")










risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "blip"
ODTRdep_bliponlyML_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRdep_bliponlyML_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
save(ODTRdep_bliponlyML_blipmeta_CVMSE,
     ODTRdep_bliponlyML_blipmeta_CVTMLE,
     file = "results/ODTRdep_bliponlyML_blipmeta_n1000.RData")










risk.type = "CV TMLE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "DonV"
metalearner = "vote"
ODTRdep_bliponlyML_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
save(ODTRdep_bliponlyML_votemeta_CVTMLE,
     file = "results/ODTRdep_bliponlyML_votemeta_n1000.RData")











QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "discrete"
risk.type = "CV TMLE"
ODTRdep_all_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
save(ODTRdep_all_discrete_CVTMLE,
     file = "results/ODTRdep_all_discrete_n1000.RData")














QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRdep_all_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
save(ODTRdep_all_votemeta_CVTMLE,
     file = "results/ODTRdep_all_votemeta_n1000.RData")




























risk.type = "CV MSE"
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRdep_bliponlyMLonly_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                risk.type = risk.type,
                                                                                                DGP_fun = DGP_fun,
                                                                                                QAW = QAW,
                                                                                                QAW.SL.library = QAW.SL.library,
                                                                                                blip.SL.library = blip.SL.library,
                                                                                                dopt.SL.library = dopt.SL.library,
                                                                                                metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRdep_bliponlyMLonly_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRdep_bliponlyMLonly_discrete_CVMSE,
     ODTRdep_bliponlyMLonly_discrete_CVTMLE,
     file = "results/ODTRdep_bliponlyMLonly_discrete_n1000.RData")










risk.type = "CV MSE"
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "blip"
ODTRdep_bliponlyMLonly_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                risk.type = risk.type,
                                                                                                DGP_fun = DGP_fun,
                                                                                                QAW = QAW,
                                                                                                QAW.SL.library = QAW.SL.library,
                                                                                                blip.SL.library = blip.SL.library,
                                                                                                dopt.SL.library = dopt.SL.library,
                                                                                                metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRdep_bliponlyMLonly_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRdep_bliponlyMLonly_blipmeta_CVMSE,
     ODTRdep_bliponlyMLonly_blipmeta_CVTMLE,
     file = "results/ODTRdep_bliponlyMLonly_blipmeta_n1000.RData")










risk.type = "CV TMLE"
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "DonV"
metalearner = "vote"
ODTRdep_bliponlyMLonly_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRdep_bliponlyMLonly_votemeta_CVTMLE,
     file = "results/ODTRdep_bliponlyMLonly_votemeta_n1000.RData")












QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "discrete"
risk.type = "CV TMLE"
ODTRdep_MLonlyEYd_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                          risk.type = risk.type,
                                                                                          DGP_fun = DGP_fun,
                                                                                          QAW = QAW,
                                                                                          QAW.SL.library = QAW.SL.library,
                                                                                          blip.SL.library = blip.SL.library,
                                                                                          dopt.SL.library = dopt.SL.library,
                                                                                          metalearner = metalearner), mc.cores = cores))
save(ODTRdep_MLonlyEYd_discrete_CVTMLE,
     file = "results/ODTRdep_MLonlyEYd_discrete_n1000.RData")














QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRdep_MLonlyEYd_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(x=x, n=n,
                                                                                          risk.type = risk.type,
                                                                                          DGP_fun = DGP_fun,
                                                                                          QAW = QAW,
                                                                                          QAW.SL.library = QAW.SL.library,
                                                                                          blip.SL.library = blip.SL.library,
                                                                                          dopt.SL.library = dopt.SL.library,
                                                                                          metalearner = metalearner), mc.cores = cores))
save(ODTRdep_MLonlyEYd_votemeta_CVTMLE,
     file = "results/ODTRdep_MLonlyEYd_votemeta_n1000.RData")
