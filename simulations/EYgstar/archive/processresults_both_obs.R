library(SL.ODTR)
#### simple psi ####
set.seed(123)
W = data.frame(W1 = rnorm(10e7))
d0 = as.numeric(QAW_bin_simple(A = 1, W) - QAW_bin_simple(A = 0, W) <= 0)
truth = mean(QAW_bin_simple(A = d0, W))

load("simulations/EYgstar/psi_SL_simple_obs.RData")
alpha = alpha_psi_SL
c = c_psi_SL
dn = dn_psi_SL

param_alpha = unlist(lapply(alpha, function(x) x$SL.info$param))
param_c = unlist(lapply(c, function(x) x$SL.info$param))

bias_EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_TMLE",] - truth)))
bias_EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_TMLE",] - truth)))
bias_EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_TMLE",] - truth)))

bias_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
bias_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
bias_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))

var_EnYgstar_alpha = var(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_TMLE",])))
var_EnYgstar_c = var(unlist(lapply(c, function(x) x$EnYgstar["Psi_TMLE",])))
var_EnYgstar_dn = var(unlist(lapply(dn, function(x) x$EnYgstar["Psi_TMLE",])))

var_CV.EnYgstar_alpha = var(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",])))
var_CV.EnYgstar_c = var(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",])))
var_CV.EnYgstar_dn = var(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",])))

mse_EnYgstar_alpha = bias_EnYgstar_alpha^2 + var_EnYgstar_alpha
mse_EnYgstar_c = bias_EnYgstar_c^2 + var_EnYgstar_c
mse_EnYgstar_dn = bias_EnYgstar_dn^2 + var_EnYgstar_dn

mse_CV.EnYgstar_alpha = bias_CV.EnYgstar_alpha^2 + var_CV.EnYgstar_alpha
mse_CV.EnYgstar_c = bias_CV.EnYgstar_c^2 + var_CV.EnYgstar_c
mse_CV.EnYgstar_dn = bias_CV.EnYgstar_dn^2 + var_CV.EnYgstar_dn

CIwidth_EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_TMLE",])) - unlist(lapply(alpha, function(x) x$EnYgstar["CI_TMLE1",])))
CIwidth_EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_TMLE",])) - unlist(lapply(c, function(x) x$EnYgstar["CI_TMLE1",])))
CIwidth_EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_TMLE",])) - unlist(lapply(dn, function(x) x$EnYgstar["CI_TMLE1",])))

CIwidth_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE1",])))
CIwidth_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE1",])))
CIwidth_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE1",])))

table_simple = data.frame(Estimator = c(rep("TMLE", times = 3), rep("CV-TMLE", times = 3)),
                          SL = rep(c("lambda", "c", "dn"), times = 2),
                          Bias = c(bias_EnYgstar_alpha, bias_EnYgstar_c, bias_EnYgstar_dn,
                                   bias_CV.EnYgstar_alpha, bias_CV.EnYgstar_c, bias_CV.EnYgstar_dn),
                          Variance = c(var_EnYgstar_alpha, var_EnYgstar_c, var_EnYgstar_dn,
                                       var_CV.EnYgstar_alpha, var_CV.EnYgstar_c, var_CV.EnYgstar_dn),
                          MSE = c(mse_EnYgstar_alpha, mse_EnYgstar_c, mse_EnYgstar_dn,
                                  mse_CV.EnYgstar_alpha, mse_CV.EnYgstar_c, mse_CV.EnYgstar_dn),
                          CI_width = c(CIwidth_EnYgstar_alpha, CIwidth_EnYgstar_c, CIwidth_EnYgstar_dn,
                                       CIwidth_CV.EnYgstar_alpha, CIwidth_CV.EnYgstar_c, CIwidth_CV.EnYgstar_dn))
table_simple = data.frame(table_simple[,1:2], sapply(table_simple[,3:6], round, 4))
write.csv(table_simple, file = "simulations/EYgstar/table_simple_obs.csv", )






#### complex psi ####
rm(list = ls())

set.seed(123)
W = data.frame(W1 = rnorm(10e7), W2 = rnorm(10e7), W3 = rnorm(10e7))
d0 = as.numeric(QAW_bin_complex(A = 1, W) - QAW_bin_complex(A = 0, W) <= 0)
truth = mean(QAW_bin_complex(A = d0, W))

load("simulations/EYgstar/psi_SL_complex_obs.RData")
alpha = alpha_psi_SL
c = c_psi_SL
dn = dn_psi_SL

regret_alpha = mean(unlist(lapply(alpha, function(x) x$SL.info$regret)))
regret_c = mean(unlist(lapply(c, function(x) x$SL.info$regret)))
regret_dn = mean(unlist(lapply(dn, function(x) x$SL.info$regret)))

param_alpha = unlist(lapply(alpha, function(x) x$SL.info$param))
param_c = unlist(lapply(c, function(x) x$SL.info$param))

colMeans(do.call('rbind', lapply(alpha, function(x) x$SL.info[grep("coef.", colnames(x$SL.info))])))
colMeans(do.call('rbind', lapply(c, function(x) x$SL.info[grep("coef.", colnames(x$SL.info))])))
colMeans(do.call('rbind', lapply(dn, function(x) x$SL.info[grep("coef.", colnames(x$SL.info))])))

bias_EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_TMLE",] - truth)))
bias_EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_TMLE",] - truth)))
bias_EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_TMLE",] - truth)))

bias_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
bias_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
bias_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))

var_EnYgstar_alpha = var(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_TMLE",])))
var_EnYgstar_c = var(unlist(lapply(c, function(x) x$EnYgstar["Psi_TMLE",])))
var_EnYgstar_dn = var(unlist(lapply(dn, function(x) x$EnYgstar["Psi_TMLE",])))

var_CV.EnYgstar_alpha = var(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",])))
var_CV.EnYgstar_c = var(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",])))
var_CV.EnYgstar_dn = var(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",])))

mse_EnYgstar_alpha = bias_EnYgstar_alpha^2 + var_EnYgstar_alpha
mse_EnYgstar_c = bias_EnYgstar_c^2 + var_EnYgstar_c
mse_EnYgstar_dn = bias_EnYgstar_dn^2 + var_EnYgstar_dn

mse_CV.EnYgstar_alpha = bias_CV.EnYgstar_alpha^2 + var_CV.EnYgstar_alpha
mse_CV.EnYgstar_c = bias_CV.EnYgstar_c^2 + var_CV.EnYgstar_c
mse_CV.EnYgstar_dn = bias_CV.EnYgstar_dn^2 + var_CV.EnYgstar_dn

CIwidth_EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_TMLE",])) - unlist(lapply(alpha, function(x) x$EnYgstar["CI_TMLE1",])))
CIwidth_EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_TMLE",])) - unlist(lapply(c, function(x) x$EnYgstar["CI_TMLE1",])))
CIwidth_EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_TMLE",])) - unlist(lapply(dn, function(x) x$EnYgstar["CI_TMLE1",])))

CIwidth_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE1",])))
CIwidth_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE1",])))
CIwidth_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE1",])))

table_complex = data.frame(Estimator = c(rep("TMLE", times = 3), rep("CV-TMLE", times = 3)),
                           SL = rep(c("lambda", "c", "dn"), times = 2),
                           Bias = c(bias_EnYgstar_alpha, bias_EnYgstar_c, bias_EnYgstar_dn,
                                    bias_CV.EnYgstar_alpha, bias_CV.EnYgstar_c, bias_CV.EnYgstar_dn),
                           Variance = c(var_EnYgstar_alpha, var_EnYgstar_c, var_EnYgstar_dn,
                                        var_CV.EnYgstar_alpha, var_CV.EnYgstar_c, var_CV.EnYgstar_dn),
                           MSE = c(mse_EnYgstar_alpha, mse_EnYgstar_c, mse_EnYgstar_dn,
                                   mse_CV.EnYgstar_alpha, mse_CV.EnYgstar_c, mse_CV.EnYgstar_dn),
                           CI_width = c(CIwidth_EnYgstar_alpha, CIwidth_EnYgstar_c, CIwidth_EnYgstar_dn,
                                        CIwidth_CV.EnYgstar_alpha, CIwidth_CV.EnYgstar_c, CIwidth_CV.EnYgstar_dn))
table_complex = data.frame(table_complex[,1:2], sapply(table_complex[,3:6], round, 4))
write.csv(table_complex, file = "simulations/EYgstar/table_complex_obs.csv", )






#### null psi ####
rm(list = ls())

set.seed(123)
W = data.frame(W1 = rnorm(10e7), W4 = rnorm(10e7))
d0 = as.numeric(QAW_null(A = 1, W) - QAW_null(A = 0, W) <= 0)
truth = mean(QAW_null(A = d0, W))

load("simulations/EYgstar/psi_SL_null_obs.RData")
alpha = alpha_psi_SL
c = c_psi_SL
dn = dn_psi_SL

regret_alpha = mean(unlist(lapply(alpha, function(x) x$SL.info$regret)))
regret_c = mean(unlist(lapply(c, function(x) x$SL.info$regret)))
regret_dn = mean(unlist(lapply(dn, function(x) x$SL.info$regret)))

param_alpha = unlist(lapply(alpha, function(x) x$SL.info$param))
param_c = unlist(lapply(c, function(x) x$SL.info$param))

colMeans(do.call('rbind', lapply(alpha, function(x) x$SL.info[grep("coef.", colnames(x$SL.info))])))
colMeans(do.call('rbind', lapply(c, function(x) x$SL.info[grep("coef.", colnames(x$SL.info))])))
colMeans(do.call('rbind', lapply(dn, function(x) x$SL.info[grep("coef.", colnames(x$SL.info))])))

bias_EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_TMLE",] - truth)))
bias_EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_TMLE",] - truth)))
bias_EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_TMLE",] - truth)))

bias_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
bias_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
bias_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))

var_EnYgstar_alpha = var(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_TMLE",])))
var_EnYgstar_c = var(unlist(lapply(c, function(x) x$EnYgstar["Psi_TMLE",])))
var_EnYgstar_dn = var(unlist(lapply(dn, function(x) x$EnYgstar["Psi_TMLE",])))

var_CV.EnYgstar_alpha = var(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",])))
var_CV.EnYgstar_c = var(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",])))
var_CV.EnYgstar_dn = var(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",])))

mse_EnYgstar_alpha = bias_EnYgstar_alpha^2 + var_EnYgstar_alpha
mse_EnYgstar_c = bias_EnYgstar_c^2 + var_EnYgstar_c
mse_EnYgstar_dn = bias_EnYgstar_dn^2 + var_EnYgstar_dn

mse_CV.EnYgstar_alpha = bias_CV.EnYgstar_alpha^2 + var_CV.EnYgstar_alpha
mse_CV.EnYgstar_c = bias_CV.EnYgstar_c^2 + var_CV.EnYgstar_c
mse_CV.EnYgstar_dn = bias_CV.EnYgstar_dn^2 + var_CV.EnYgstar_dn

CIwidth_EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_TMLE",])) - unlist(lapply(alpha, function(x) x$EnYgstar["CI_TMLE1",])))
CIwidth_EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_TMLE",])) - unlist(lapply(c, function(x) x$EnYgstar["CI_TMLE1",])))
CIwidth_EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_TMLE",])) - unlist(lapply(dn, function(x) x$EnYgstar["CI_TMLE1",])))

CIwidth_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE1",])))
CIwidth_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE1",])))
CIwidth_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE1",])))

table_null = data.frame(Estimator = c(rep("TMLE", times = 3), rep("CV-TMLE", times = 3)),
                        SL = rep(c("lambda", "c", "dn"), times = 2),
                        Bias = c(bias_EnYgstar_alpha, bias_EnYgstar_c, bias_EnYgstar_dn,
                                 bias_CV.EnYgstar_alpha, bias_CV.EnYgstar_c, bias_CV.EnYgstar_dn),
                        Variance = c(var_EnYgstar_alpha, var_EnYgstar_c, var_EnYgstar_dn,
                                     var_CV.EnYgstar_alpha, var_CV.EnYgstar_c, var_CV.EnYgstar_dn),
                        MSE = c(mse_EnYgstar_alpha, mse_EnYgstar_c, mse_EnYgstar_dn,
                                mse_CV.EnYgstar_alpha, mse_CV.EnYgstar_c, mse_CV.EnYgstar_dn),
                        CI_width = c(CIwidth_EnYgstar_alpha, CIwidth_EnYgstar_c, CIwidth_EnYgstar_dn,
                                     CIwidth_CV.EnYgstar_alpha, CIwidth_CV.EnYgstar_c, CIwidth_CV.EnYgstar_dn))
table_null = data.frame(table_null[,1:2], sapply(table_null[,3:6], round, 4))
write.csv(table_null, file = "simulations/EYgstar/table_null_obs.csv", )



