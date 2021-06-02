library(SL.ODTR)
#### null psi ####
rm(list = ls())

set.seed(123)
#W = data.frame(W1 = rnorm(10e7), W4 = rnorm(10e7))
#d0 = as.numeric(QAW_null(A = 1, W) - QAW_null(A = 0, W) <= 0)
truth = 0.5 #mean(QAW_null(A = d0, W))

#alpha = alpha_psi_SL
#c = c_psi_SL
#dn = dn_psi_SL

res_fun = function(alpha, c, dn, risk, study) {

  regret_alpha = mean(unlist(lapply(alpha, function(x) x$SL.info$regret)))
  regret_c = mean(unlist(lapply(c, function(x) x$SL.info$regret)))
  regret_dn = mean(unlist(lapply(dn, function(x) x$SL.info$regret)))

  param_alpha = unlist(lapply(alpha, function(x) x$SL.info$param))
  param_c = unlist(lapply(c, function(x) x$SL.info$param))

  bias_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
  bias_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
  bias_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))

  var_CV.EnYgstar_alpha = var(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",])))
  var_CV.EnYgstar_c = var(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",])))
  var_CV.EnYgstar_dn = var(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",])))

  mse_CV.EnYgstar_alpha = bias_CV.EnYgstar_alpha^2 + var_CV.EnYgstar_alpha
  mse_CV.EnYgstar_c = bias_CV.EnYgstar_c^2 + var_CV.EnYgstar_c
  mse_CV.EnYgstar_dn = bias_CV.EnYgstar_dn^2 + var_CV.EnYgstar_dn

  cov_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE1",] < truth & x$EnYgstar["CI_CV.TMLE2",] > truth)))*100
  cov_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE1",] < truth & x$EnYgstar["CI_CV.TMLE2",] > truth)))*100
  cov_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE1",] < truth & x$EnYgstar["CI_CV.TMLE2",] > truth)))*100

  CIwidth_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE1",])))
  CIwidth_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE1",])))
  CIwidth_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["Psi_CV.TMLE",])) - unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE1",])))

  toreturn = list()
  toreturn$regret =   c(regret_alpha = mean(unlist(lapply(alpha, function(x) x$SL.info$regret))),
                        regret_c = mean(unlist(lapply(c, function(x) x$SL.info$regret))),
                        regret_dn = mean(unlist(lapply(dn, function(x) x$SL.info$regret))))
  toreturn$param_alpha = unlist(lapply(alpha, function(x) x$SL.info$param))
  toreturn$param_c = unlist(lapply(c, function(x) x$SL.info$param))

  toreturn$ret_table = data.frame(Study = study,
                                  Risk = risk,
                                  #Estimator = rep("CV-TMLE", times = 3),
                                  SL = c("lambda", "c", "dn"),
                                  Bias = c(bias_CV.EnYgstar_alpha, bias_CV.EnYgstar_c, bias_CV.EnYgstar_dn),
                                  Variance = c(var_CV.EnYgstar_alpha, var_CV.EnYgstar_c, var_CV.EnYgstar_dn),
                                  MSE = c(mse_CV.EnYgstar_alpha, mse_CV.EnYgstar_c, mse_CV.EnYgstar_dn),
                                  Coverage = c(cov_CV.EnYgstar_alpha, cov_CV.EnYgstar_c, cov_CV.EnYgstar_dn),
                                  CI_width = c(CIwidth_CV.EnYgstar_alpha, CIwidth_CV.EnYgstar_c, CIwidth_CV.EnYgstar_dn))

  return(toreturn)


}

library(latex2exp)
png('simulations/EYgstar/plot1_null.png')
par(mfrow=c(4,2))
load("simulations/EYgstar/psi_SL_null.RData")
res_psi_RCT = res_fun(alpha = alpha_psi_SL, c = c_psi_SL, dn = dn_psi_SL, risk = "Psi", study = "RCT")
tab_psi_RCT = res_psi_RCT$ret_table
res_psi_RCT$regret
hist(res_psi_RCT$param_alpha, ylab = "", main = TeX('Study = RCT; Risk = $R_{\\hat{g}^*,CV,1}$'), xlab = TeX('$\\lambda$'), breaks = 5)
hist(res_psi_RCT$param_c, ylab = "", main = TeX('Study = RCT; Risk = $R_{\\hat{g}^*,CV,1}$'), xlab = TeX('$c$'), breaks = 5)
load("simulations/EYgstar/CI_SL_null.RData")
res_CI_RCT = res_fun(alpha = alpha_CI_SL, c = c_CI_SL, dn = dn_CI_SL, risk = "CI", study = "RCT")
tab_CI_RCT = res_CI_RCT$ret_table
res_CI_RCT$regret
hist(res_CI_RCT$param_alpha, ylab = "", main = TeX('Study = RCT; Risk = $R_{\\hat{g}^*,CV,2}$'), xlab = TeX('$\\lambda$'), breaks = 5)
hist(res_CI_RCT$param_c, ylab = "", main = TeX('Study = RCT; Risk = $R_{\\hat{g}^*,CV,2}$'), xlab = TeX('$c$'), breaks = 5)
load("simulations/EYgstar/psi_SL_null_obs.RData")
res_psi_obs = res_fun(alpha = alpha_psi_SL, c = c_psi_SL, dn = dn_psi_SL, risk = "Psi", study = "obs")
tab_psi_obs = res_psi_obs$ret_table
res_psi_obs$regret
hist(res_psi_obs$param_alpha, ylab = "", main = TeX('Study = Obs.; Risk = $R_{\\hat{g}^*,CV,1}$'), xlab = TeX('$\\lambda$'), breaks = 5)
hist(res_psi_obs$param_c, ylab = "", main = TeX('Study = Obs.; Risk = $R_{\\hat{g}^*,CV,1}$'), xlab = TeX('$c$'), breaks = 5)
load("simulations/EYgstar/CI_SL_null_obs.RData")
res_CI_obs = res_fun(alpha = alpha_CI_SL, c = c_CI_SL, dn = dn_CI_SL, risk = "CI", study = "obs")
tab_CI_obs = res_CI_obs$ret_table
res_CI_obs$regret
hist(res_CI_obs$param_alpha, ylab = "", main = TeX('Study = Obs.; Risk = $R_{\\hat{g}^*,CV,2}$'), xlab = TeX('$\\lambda$'), breaks = 5)
hist(res_CI_obs$param_c, ylab = "", main = TeX('Study = Obs.; Risk = $R_{\\hat{g}^*,CV,2}$'), xlab = TeX('$c$'), breaks = 5)
dev.off()

table = rbind(tab_psi_RCT,
              tab_CI_RCT,
              tab_psi_obs,
              tab_CI_obs)
table = cbind(table[,1:3], apply(table[,4:8], 2, round, 4))
write.csv(table, file = "simulations/EYgstar/null.csv")


