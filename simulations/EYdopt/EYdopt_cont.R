# set seed
set.seed(4490)

# load packages
library(SuperLearner)
library(hitandrun)
library(SL.ODTR)
library(parallel)

r = 1000
n = 1000
DGP_fun = DGP_cont
QAW = QAW_cont
QAW.SL.library = "SL.QAW.correct_cont"
blip.SL.library = "SL.blip.correct_cont"
grid.size = 2
contrast = data.frame(treatnone = rep(0, times = n), treatall = rep(1, times = n))




EYdoptcont_wcontrast = mclapply(1:r, function(x) performance_EYdopt(x = x,
                                                                    n = n,
                                                                    DGP_fun = DGP_fun,
                                                                    QAW = QAW,
                                                                    QAW.SL.library = QAW.SL.library,
                                                                    blip.SL.library = blip.SL.library,
                                                                    grid.size = grid.size,
                                                                    contrast = contrast), mc.cores = detectCores())
save(EYdoptcont_wcontrast,
     file = "../simulations/EYdopt/results/EYdoptcont_wcontrast.RData")


EYdoptcont_nocontrast = mclapply(1:r, function(x) performance_EYdopt(x = x,
                                                                     n = n,
                                                                     DGP_fun = DGP_fun,
                                                                     QAW = QAW,
                                                                     QAW.SL.library = QAW.SL.library,
                                                                     blip.SL.library = blip.SL.library,
                                                                     grid.size = grid.size,
                                                                     contrast = NULL), mc.cores = detectCores())
save(EYdoptcont_nocontrast,
     file = "../simulations/EYdopt/results/EYdoptcont_nocontrast.RData")

