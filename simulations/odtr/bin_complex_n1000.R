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
QAW = QAW_bin_complex
DGP_fun = DGP_bin_complex



risk.type = "CV MSE"
QAW.SL.library = "SL.QAW.HTEepi"
blip.SL.library = "SL.blip.HTEepi"
dopt.SL.library = NULL
metalearner = "blip"
ODTRcomplex_incorrectglm_NA_NA = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                          risk.type = risk.type,
                                                                                          DGP_fun = DGP_fun,
                                                                                          QAW = QAW,
                                                                                          QAW.SL.library = QAW.SL.library,
                                                                                          blip.SL.library = blip.SL.library,
                                                                                          dopt.SL.library = dopt.SL.library,
                                                                                          metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_incorrectglm_NA_NA,
     file = "results/ODTRcomplex_GLM_n1000.RData")



risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRcomplex_bliponlyparam_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRcomplex_bliponlyparam_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_bliponlyparam_discrete_CVMSE,
     ODTRcomplex_bliponlyparam_discrete_CVTMLE,
     file = "results/ODTRcomplex_bliponlyparam_discrete_n1000.RData")








risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4")
dopt.SL.library = NULL
metalearner = "blip"
ODTRcomplex_bliponlyparam_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRcomplex_bliponlyparam_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_bliponlyparam_blipmeta_CVMSE,
     ODTRcomplex_bliponlyparam_blipmeta_CVTMLE,
     file = "results/ODTRcomplex_bliponlyparam_blipmeta_n1000.RData")








QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4")
dopt.SL.library = "DonV"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRcomplex_bliponlyparam_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_bliponlyparam_votemeta_CVTMLE,
     file = "results/ODTRcomplex_bliponlyparam_votemeta_n1000.RData")









risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRcomplex_bliponlyML_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRcomplex_bliponlyML_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_bliponlyML_discrete_CVMSE,
     ODTRcomplex_bliponlyML_discrete_CVTMLE,
     file = "results/ODTRcomplex_bliponlyML_discrete_n1000.RData")










risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "blip"
ODTRcomplex_bliponlyML_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRcomplex_bliponlyML_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_bliponlyML_blipmeta_CVMSE,
     ODTRcomplex_bliponlyML_blipmeta_CVTMLE,
     file = "results/ODTRcomplex_bliponlyML_blipmeta_n1000.RData")










risk.type = "CV TMLE"
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "DonV"
metalearner = "vote"
ODTRcomplex_bliponlyML_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_bliponlyML_votemeta_CVTMLE,
     file = "results/ODTRcomplex_bliponlyML_votemeta_n1000.RData")











QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "discrete"
risk.type = "CV TMLE"
ODTRcomplex_all_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_all_discrete_CVTMLE,
     file = "results/ODTRcomplex_all_discrete_n1000.RData")














QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRcomplex_all_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_all_votemeta_CVTMLE,
     file = "results/ODTRcomplex_all_votemeta_n1000.RData")




























risk.type = "CV MSE"
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRcomplex_bliponlyMLonly_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                risk.type = risk.type,
                                                                                                DGP_fun = DGP_fun,
                                                                                                QAW = QAW,
                                                                                                QAW.SL.library = QAW.SL.library,
                                                                                                blip.SL.library = blip.SL.library,
                                                                                                dopt.SL.library = dopt.SL.library,
                                                                                                metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRcomplex_bliponlyMLonly_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_bliponlyMLonly_discrete_CVMSE,
     ODTRcomplex_bliponlyMLonly_discrete_CVTMLE,
     file = "results/ODTRcomplex_bliponlyMLonly_discrete_n1000.RData")










risk.type = "CV MSE"
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "blip"
ODTRcomplex_bliponlyMLonly_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                risk.type = risk.type,
                                                                                                DGP_fun = DGP_fun,
                                                                                                QAW = QAW,
                                                                                                QAW.SL.library = QAW.SL.library,
                                                                                                blip.SL.library = blip.SL.library,
                                                                                                dopt.SL.library = dopt.SL.library,
                                                                                                metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRcomplex_bliponlyMLonly_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_bliponlyMLonly_blipmeta_CVMSE,
     ODTRcomplex_bliponlyMLonly_blipmeta_CVTMLE,
     file = "results/ODTRcomplex_bliponlyMLonly_blipmeta_n1000.RData")










risk.type = "CV TMLE"
QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "DonV"
metalearner = "vote"
ODTRcomplex_bliponlyMLonly_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_bliponlyMLonly_votemeta_CVTMLE,
     file = "results/ODTRcomplex_bliponlyMLonly_votemeta_n1000.RData")












QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "discrete"
risk.type = "CV TMLE"
ODTRcomplex_MLonlyEYd_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                          risk.type = risk.type,
                                                                                          DGP_fun = DGP_fun,
                                                                                          QAW = QAW,
                                                                                          QAW.SL.library = QAW.SL.library,
                                                                                          blip.SL.library = blip.SL.library,
                                                                                          dopt.SL.library = dopt.SL.library,
                                                                                          metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_MLonlyEYd_discrete_CVTMLE,
     file = "results/ODTRcomplex_MLonlyEYd_discrete_n1000.RData")














QAW.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRcomplex_MLonlyEYd_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                          risk.type = risk.type,
                                                                                          DGP_fun = DGP_fun,
                                                                                          QAW = QAW,
                                                                                          QAW.SL.library = QAW.SL.library,
                                                                                          blip.SL.library = blip.SL.library,
                                                                                          dopt.SL.library = dopt.SL.library,
                                                                                          metalearner = metalearner), mc.cores = cores))
save(ODTRcomplex_MLonlyEYd_votemeta_CVTMLE,
     file = "results/ODTRcomplex_MLonlyEYd_votemeta_n1000.RData")
