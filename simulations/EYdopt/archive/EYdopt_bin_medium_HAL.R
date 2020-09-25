# set seed
set.seed(4490, kind = "L'Ecuyer-CMRG")

# load packages
library(SuperLearner)
library(hitandrun)
library(parallel)
library(SL.ODTR)
library(hal9001)

r = 1000
n = 1000
QAW = QAW_bin_complex
DGP_fun = DGP_bin_complex
grid.size = 100
QAW.SL.library = c("SL.QAW.W1", "SL.QAW.W2", "SL.QAW.W3", "SL.QAW.W4",
                   "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart", "SL.hal9001")
blip.SL.library = c("SL.blip.W1", "SL.blip.W2", "SL.blip.W3", "SL.blip.W4",
                    "SL.glm", "SL.mean", "SL.glm.interaction", "SL.earth", "SL.nnet", "SL.svm", "SL.rpart", "SL.hal9001")

EYdopt_bin_medium = mclapply(1:r, function(x) performance_EYdopt(x = x,
                                                                 n = n,
                                                                 DGP_fun = DGP_fun,
                                                                 QAW = QAW,
                                                                 QAW.SL.library = QAW.SL.library,
                                                                 blip.SL.library = blip.SL.library,
                                                                 grid.size = grid.size), mc.cores = detectCores())

save(EYdopt_bin_medium, file = "folder/EYdopt_bin_medium.RData")




