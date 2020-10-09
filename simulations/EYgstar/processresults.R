load("simulations/true values/DGP_null_true_values.RData")
truth = DGP_null_true_values$EYd_star

load("simulations/EYgstar/alpha_psi.RData")

regret_alpha_psi = mean(unlist(lapply(alpha_psi, function(x) x$SL.info$regret)))
param_alpha_psi = unlist(lapply(alpha_psi, function(x) x$SL.info$param))
regret_alpha0_psi = mean(unlist(lapply(alpha0_psi, function(x) x$SL.info$regret)))

colMeans(do.call('rbind', lapply(alpha_psi, function(x) x$SL.info[grep("coef.", colnames(x$SL.info))])))
colMeans(do.call('rbind', lapply(alpha0_psi, function(x) x$SL.info[grep("coef.", colnames(x$SL.info))])))

bias_EnYgstar_alpha_psi = mean(unlist(lapply(alpha_psi, function(x) x$EnYgstar["Psi_TMLE",] - truth)))
bias_CV.EnYgstar_alpha_psi = mean(unlist(lapply(alpha_psi, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
var_EnYgstar_alpha_psi = var(unlist(lapply(alpha_psi, function(x) x$EnYgstar["Psi_TMLE",])))
var_CV.EnYgstar_alpha_psi = var(unlist(lapply(alpha_psi, function(x) x$EnYgstar["Psi_CV.TMLE",])))
cov_EnYgstar_alpha_psi = mean(unlist(lapply(alpha_psi, function(x) x$EnYgstar["CI_TMLE1",])) < truth & unlist(lapply(alpha_psi, function(x) x$EnYgstar["CI_TMLE2",])) > truth)
cov_CV.EnYgstar_alpha_psi = mean(unlist(lapply(alpha_psi, function(x) x$EnYgstar["CI_CV.TMLE1",])) < truth & unlist(lapply(alpha_psi, function(x) x$EnYgstar["CI_CV.TMLE2",])) > truth)

bias_EnYgstar_alpha_psi0 = mean(unlist(lapply(alpha0_psi, function(x) x$EnYgstar["Psi_TMLE",] - truth)))
bias_CV.EnYgstar_alpha_psi0 = mean(unlist(lapply(alpha0_psi, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
var_EnYgstar_alpha_psi0 = var(unlist(lapply(alpha0_psi, function(x) x$EnYgstar["Psi_TMLE",])))
var_CV.EnYgstar_alpha_psi0 = var(unlist(lapply(alpha0_psi, function(x) x$EnYgstar["Psi_CV.TMLE",])))
cov_EnYgstar_alpha_psi0 = mean(unlist(lapply(alpha0_psi, function(x) x$EnYgstar["CI_TMLE1",])) < truth & unlist(lapply(alpha_psi, function(x) x$EnYgstar["CI_TMLE2",])) > truth)
cov_CV.EnYgstar_alpha_psi0 = mean(unlist(lapply(alpha0_psi, function(x) x$EnYgstar["CI_CV.TMLE1",])) < truth & unlist(lapply(alpha_psi, function(x) x$EnYgstar["CI_CV.TMLE2",])) > truth)

load("simulations/EYgstar/c_psi.RData")

regret_c_psi = mean(unlist(lapply(c_psi, function(x) x$SL.info$regret)))
param_c_psi = unlist(lapply(c_psi, function(x) x$SL.info$param))
regret_c0_psi = mean(unlist(lapply(c0_psi, function(x) x$SL.info$regret)))

bias_EnYgstar_c_psi = mean(unlist(lapply(c_psi, function(x) x$EnYgstar["Psi_TMLE",] - truth)))
bias_CV.EnYgstar_c_psi = mean(unlist(lapply(c_psi, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
var_EnYgstar_c_psi = var(unlist(lapply(c_psi, function(x) x$EnYgstar["Psi_TMLE",])))
var_CV.EnYgstar_c_psi = var(unlist(lapply(c_psi, function(x) x$EnYgstar["Psi_CV.TMLE",])))
cov_EnYgstar_c_psi = mean(unlist(lapply(c_psi, function(x) x$EnYgstar["CI_TMLE1",])) < truth & unlist(lapply(c_psi, function(x) x$EnYgstar["CI_TMLE2",])) > truth)
cov_CV.EnYgstar_c_psi = mean(unlist(lapply(c_psi, function(x) x$EnYgstar["CI_CV.TMLE1",])) < truth & unlist(lapply(c_psi, function(x) x$EnYgstar["CI_CV.TMLE2",])) > truth)

bias_EnYgstar_c_psi0 = mean(unlist(lapply(c0_psi, function(x) x$EnYgstar["Psi_TMLE",] - truth)))
bias_CV.EnYgstar_c_psi0 = mean(unlist(lapply(c0_psi, function(x) x$EnYgstar["Psi_CV.TMLE",] - truth)))
var_EnYgstar_c_psi0 = var(unlist(lapply(c0_psi, function(x) x$EnYgstar["Psi_TMLE",])))
var_CV.EnYgstar_c_psi0 = var(unlist(lapply(c0_psi, function(x) x$EnYgstar["Psi_CV.TMLE",])))
cov_EnYgstar_c_psi0 = mean(unlist(lapply(c0_psi, function(x) x$EnYgstar["CI_TMLE1",])) < truth & unlist(lapply(c_psi, function(x) x$EnYgstar["CI_TMLE2",])) > truth)
cov_CV.EnYgstar_c_psi0 = mean(unlist(lapply(c0_psi, function(x) x$EnYgstar["CI_CV.TMLE1",])) < truth & unlist(lapply(c_psi, function(x) x$EnYgstar["CI_CV.TMLE2",])) > truth)

