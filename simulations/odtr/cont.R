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
QAW = QAW_cont
DGP_fun = DGP_cont


risk.type = "CV MSE"
QAW.SL.library = "SL.QAW.correct_cont"
blip.SL.library = "SL.blip.correct_cont"
dopt.SL.library = NULL
metalearner = "blip"
ODTRcont_correctglm_NA_NA = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                      risk.type = risk.type,
                                                                                      DGP_fun = DGP_fun,
                                                                                      QAW = QAW,
                                                                                      QAW.SL.library = QAW.SL.library,
                                                                                      blip.SL.library = blip.SL.library,
                                                                                      dopt.SL.library = dopt.SL.library,
                                                                                      metalearner = metalearner), mc.cores = cores))
risk.type = "CV MSE"
QAW.SL.library = "SL.QAW.incorrect"
blip.SL.library = "SL.glm"
dopt.SL.library = NULL
metalearner = "blip"
ODTRcont_incorrectglm_NA_NA = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                      risk.type = risk.type,
                                                                                      DGP_fun = DGP_fun,
                                                                                      QAW = QAW,
                                                                                      QAW.SL.library = QAW.SL.library,
                                                                                      blip.SL.library = blip.SL.library,
                                                                                      dopt.SL.library = dopt.SL.library,
                                                                                      metalearner = metalearner), mc.cores = cores))
save(ODTRcont_incorrectglm_NA_NA,
     ODTRcont_correctglm_NA_NA,
     file = "results/ODTRcont_GLM.RData")



risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRcont_bliponlyparam_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRcont_bliponlyparam_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                risk.type = risk.type,
                                                                                                DGP_fun = DGP_fun,
                                                                                                QAW = QAW,
                                                                                                QAW.SL.library = QAW.SL.library,
                                                                                                blip.SL.library = blip.SL.library,
                                                                                                dopt.SL.library = dopt.SL.library,
                                                                                                metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont_bliponlyparam_discrete_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRcont_bliponlyparam_discrete_CVMSE,
     ODTRcont_bliponlyparam_discrete_CVTMLE,
     ODTRcont_bliponlyparam_discrete_CVTMLECI,
     file = "results/ODTRcont_bliponlyparam_discrete.RData")








risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4")
dopt.SL.library = NULL
metalearner = "blip"
ODTRcont_bliponlyparam_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRcont_bliponlyparam_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont_bliponlyparam_blipmeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                       risk.type = risk.type,
                                                                                                       DGP_fun = DGP_fun,
                                                                                                       QAW = QAW,
                                                                                                       QAW.SL.library = QAW.SL.library,
                                                                                                       blip.SL.library = blip.SL.library,
                                                                                                       dopt.SL.library = dopt.SL.library,
                                                                                                       metalearner = metalearner), mc.cores = cores))
save(ODTRcont_bliponlyparam_blipmeta_CVMSE,
     ODTRcont_bliponlyparam_blipmeta_CVTMLE,
     ODTRcont_bliponlyparam_blipmeta_CVTMLECI,
     file = "results/ODTRcont_bliponlyparam_blipmeta.RData")








QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4")
dopt.SL.library = "DonV"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRcont_bliponlyparam_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont_bliponlyparam_votemeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                       risk.type = risk.type,
                                                                                                       DGP_fun = DGP_fun,
                                                                                                       QAW = QAW,
                                                                                                       QAW.SL.library = QAW.SL.library,
                                                                                                       blip.SL.library = blip.SL.library,
                                                                                                       dopt.SL.library = dopt.SL.library,
                                                                                                       metalearner = metalearner), mc.cores = cores))
save(ODTRcont_bliponlyparam_votemeta_CVTMLE,
     ODTRcont_bliponlyparam_votemeta_CVTMLECI,
     file = "results/ODTRcont_bliponlyparam_votemeta.RData")









risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRcont_bliponlyML_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRcont_bliponlyML_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont_bliponlyML_discrete_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                       risk.type = risk.type,
                                                                                                       DGP_fun = DGP_fun,
                                                                                                       QAW = QAW,
                                                                                                       QAW.SL.library = QAW.SL.library,
                                                                                                       blip.SL.library = blip.SL.library,
                                                                                                       dopt.SL.library = dopt.SL.library,
                                                                                                       metalearner = metalearner), mc.cores = cores))
save(ODTRcont_bliponlyML_discrete_CVMSE,
     ODTRcont_bliponlyML_discrete_CVTMLE,
     ODTRcont_bliponlyML_discrete_CVTMLECI,
     file = "results/ODTRcont_bliponlyML_discrete.RData")










risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "blip"
ODTRcont_bliponlyML_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRcont_bliponlyML_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont_bliponlyML_blipmeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
save(ODTRcont_bliponlyML_blipmeta_CVMSE,
     ODTRcont_bliponlyML_blipmeta_CVTMLE,
     ODTRcont_bliponlyML_blipmeta_CVTMLECI,
     file = "results/ODTRcont_bliponlyML_blipmeta.RData")










risk.type = "CV TMLE"
QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "DonV"
metalearner = "vote"
ODTRcont_bliponlyML_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont_bliponlyML_votemeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
save(ODTRcont_bliponlyML_votemeta_CVTMLE,
     ODTRcont_bliponlyML_votemeta_CVTMLECI,
     file = "results/ODTRcont_bliponlyML_votemeta.RData")











QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "discrete"
risk.type = "CV TMLE"
ODTRcont_all_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont_all_discrete_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
save(ODTRcont_all_discrete_CVTMLECI,
     ODTRcont_all_discrete_CVTMLE,
     file = "results/ODTRcont_all_discrete.RData")














QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRcont_all_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont_all_votemeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                             risk.type = risk.type,
                                                                                             DGP_fun = DGP_fun,
                                                                                             QAW = QAW,
                                                                                             QAW.SL.library = QAW.SL.library,
                                                                                             blip.SL.library = blip.SL.library,
                                                                                             dopt.SL.library = dopt.SL.library,
                                                                                             metalearner = metalearner), mc.cores = cores))
save(ODTRcont_all_votemeta_CVTMLE,
     ODTRcont_all_votemeta_CVTMLECI,
     file = "results/ODTRcont_all_votemeta.RData")


