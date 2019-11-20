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
QAW = QAW_cont2
DGP_fun = DGP_cont2


# risk.type = "CV MSE"
# QAW.SL.library = "SL.QAW.correct_cont2"
# blip.SL.library = "SL.blip.correct_cont2"
# dopt.SL.library = NULL
# metalearner = "blip"
# ODTRcont2_correctglm_NA_NA = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
#                                                                                       risk.type = risk.type,
#                                                                                       DGP_fun = DGP_fun,
#                                                                                       QAW = QAW,
#                                                                                       QAW.SL.library = QAW.SL.library,
#                                                                                       blip.SL.library = blip.SL.library,
#                                                                                       dopt.SL.library = dopt.SL.library,
#                                                                                       metalearner = metalearner), mc.cores = cores))
risk.type = "CV MSE"
QAW.SL.library = "SL.QAW.incorrect"
blip.SL.library = "SL.glm"
dopt.SL.library = NULL
metalearner = "blip"
ODTRcont2_incorrectglm_NA_NA = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                      risk.type = risk.type,
                                                                                      DGP_fun = DGP_fun,
                                                                                      QAW = QAW,
                                                                                      QAW.SL.library = QAW.SL.library,
                                                                                      blip.SL.library = blip.SL.library,
                                                                                      dopt.SL.library = dopt.SL.library,
                                                                                      metalearner = metalearner), mc.cores = cores))
save(ODTRcont2_incorrectglm_NA_NA,
     #ODTRcont2_correctglm_NA_NA,
     file = "results/ODTRcont2_GLM.RData")



risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRcont2_bliponlyparam_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRcont2_bliponlyparam_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                risk.type = risk.type,
                                                                                                DGP_fun = DGP_fun,
                                                                                                QAW = QAW,
                                                                                                QAW.SL.library = QAW.SL.library,
                                                                                                blip.SL.library = blip.SL.library,
                                                                                                dopt.SL.library = dopt.SL.library,
                                                                                                metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont2_bliponlyparam_discrete_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
save(ODTRcont2_bliponlyparam_discrete_CVMSE,
     ODTRcont2_bliponlyparam_discrete_CVTMLE,
     ODTRcont2_bliponlyparam_discrete_CVTMLECI,
     file = "results/ODTRcont2_bliponlyparam_discrete.RData")








risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4")
dopt.SL.library = NULL
metalearner = "blip"
ODTRcont2_bliponlyparam_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRcont2_bliponlyparam_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont2_bliponlyparam_blipmeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                       risk.type = risk.type,
                                                                                                       DGP_fun = DGP_fun,
                                                                                                       QAW = QAW,
                                                                                                       QAW.SL.library = QAW.SL.library,
                                                                                                       blip.SL.library = blip.SL.library,
                                                                                                       dopt.SL.library = dopt.SL.library,
                                                                                                       metalearner = metalearner), mc.cores = cores))
save(ODTRcont2_bliponlyparam_blipmeta_CVMSE,
     ODTRcont2_bliponlyparam_blipmeta_CVTMLE,
     ODTRcont2_bliponlyparam_blipmeta_CVTMLECI,
     file = "results/ODTRcont2_bliponlyparam_blipmeta.RData")








QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4")
dopt.SL.library = "DonV"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRcont2_bliponlyparam_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont2_bliponlyparam_votemeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                       risk.type = risk.type,
                                                                                                       DGP_fun = DGP_fun,
                                                                                                       QAW = QAW,
                                                                                                       QAW.SL.library = QAW.SL.library,
                                                                                                       blip.SL.library = blip.SL.library,
                                                                                                       dopt.SL.library = dopt.SL.library,
                                                                                                       metalearner = metalearner), mc.cores = cores))
save(ODTRcont2_bliponlyparam_votemeta_CVTMLE,
     ODTRcont2_bliponlyparam_votemeta_CVTMLECI,
     file = "results/ODTRcont2_bliponlyparam_votemeta.RData")









risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "discrete"
ODTRcont2_bliponlyML_discrete_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
dopt.SL.library = "DonV"
ODTRcont2_bliponlyML_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                     risk.type = risk.type,
                                                                                                     DGP_fun = DGP_fun,
                                                                                                     QAW = QAW,
                                                                                                     QAW.SL.library = QAW.SL.library,
                                                                                                     blip.SL.library = blip.SL.library,
                                                                                                     dopt.SL.library = dopt.SL.library,
                                                                                                     metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont2_bliponlyML_discrete_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                       risk.type = risk.type,
                                                                                                       DGP_fun = DGP_fun,
                                                                                                       QAW = QAW,
                                                                                                       QAW.SL.library = QAW.SL.library,
                                                                                                       blip.SL.library = blip.SL.library,
                                                                                                       dopt.SL.library = dopt.SL.library,
                                                                                                       metalearner = metalearner), mc.cores = cores))
save(ODTRcont2_bliponlyML_discrete_CVMSE,
     ODTRcont2_bliponlyML_discrete_CVTMLE,
     ODTRcont2_bliponlyML_discrete_CVTMLECI,
     file = "results/ODTRcont2_bliponlyML_discrete.RData")










risk.type = "CV MSE"
QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = NULL
metalearner = "blip"
ODTRcont2_bliponlyML_blipmeta_CVMSE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                 risk.type = risk.type,
                                                                                                 DGP_fun = DGP_fun,
                                                                                                 QAW = QAW,
                                                                                                 QAW.SL.library = QAW.SL.library,
                                                                                                 blip.SL.library = blip.SL.library,
                                                                                                 dopt.SL.library = dopt.SL.library,
                                                                                                 metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE"
ODTRcont2_bliponlyML_blipmeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont2_bliponlyML_blipmeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
save(ODTRcont2_bliponlyML_blipmeta_CVMSE,
     ODTRcont2_bliponlyML_blipmeta_CVTMLE,
     ODTRcont2_bliponlyML_blipmeta_CVTMLECI,
     file = "results/ODTRcont2_bliponlyML_blipmeta.RData")










risk.type = "CV TMLE"
QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "DonV"
metalearner = "vote"
ODTRcont2_bliponlyML_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont2_bliponlyML_votemeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                    risk.type = risk.type,
                                                                                                    DGP_fun = DGP_fun,
                                                                                                    QAW = QAW,
                                                                                                    QAW.SL.library = QAW.SL.library,
                                                                                                    blip.SL.library = blip.SL.library,
                                                                                                    dopt.SL.library = dopt.SL.library,
                                                                                                    metalearner = metalearner), mc.cores = cores))
save(ODTRcont2_bliponlyML_votemeta_CVTMLE,
     ODTRcont2_bliponlyML_votemeta_CVTMLECI,
     file = "results/ODTRcont2_bliponlyML_votemeta.RData")











QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "discrete"
risk.type = "CV TMLE"
ODTRcont2_all_discrete_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                                  risk.type = risk.type,
                                                                                                  DGP_fun = DGP_fun,
                                                                                                  QAW = QAW,
                                                                                                  QAW.SL.library = QAW.SL.library,
                                                                                                  blip.SL.library = blip.SL.library,
                                                                                                  dopt.SL.library = dopt.SL.library,
                                                                                                  metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont2_all_discrete_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
save(ODTRcont2_all_discrete_CVTMLECI,
     ODTRcont2_all_discrete_CVTMLE,
     file = "results/ODTRcont2_all_discrete.RData")














QAW.SL.library = c("SL.QAW.incorrect1", "SL.QAW.incorrect2", "SL.QAW.incorrect3", "SL.QAW.incorrect4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
blip.SL.library = c("SL.blip.incorrect1", "SL.blip.incorrect2", "SL.blip.incorrect3", "SL.blip.incorrect4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart")
dopt.SL.library = "all"
metalearner = "vote"
risk.type = "CV TMLE"
ODTRcont2_all_votemeta_CVTMLE = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                           risk.type = risk.type,
                                                                                           DGP_fun = DGP_fun,
                                                                                           QAW = QAW,
                                                                                           QAW.SL.library = QAW.SL.library,
                                                                                           blip.SL.library = blip.SL.library,
                                                                                           dopt.SL.library = dopt.SL.library,
                                                                                           metalearner = metalearner), mc.cores = cores))
risk.type = "CV TMLE CI"
ODTRcont2_all_votemeta_CVTMLECI = do.call("rbind", mclapply(1:r, function(x) performance_ODTR(n = n,
                                                                                             risk.type = risk.type,
                                                                                             DGP_fun = DGP_fun,
                                                                                             QAW = QAW,
                                                                                             QAW.SL.library = QAW.SL.library,
                                                                                             blip.SL.library = blip.SL.library,
                                                                                             dopt.SL.library = dopt.SL.library,
                                                                                             metalearner = metalearner), mc.cores = cores))
save(ODTRcont2_all_votemeta_CVTMLE,
     ODTRcont2_all_votemeta_CVTMLECI,
     file = "results/ODTRcont2_all_votemeta.RData")


