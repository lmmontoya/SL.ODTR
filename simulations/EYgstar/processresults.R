load("simulations/true values/DGP_null_true_values.RData")
truth = DGP_null_true_values$EYd_star

load("simulations/EYgstar/psi_glm.RData")
alpha = alpha_psi_glm
c = c_psi_glm
dn = dn_psi_glm

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

cov_EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["CI_TMLE1",])) < truth & unlist(lapply(alpha, function(x) x$EnYgstar["CI_TMLE2",])) > truth)
cov_EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["CI_TMLE1",])) < truth & unlist(lapply(c, function(x) x$EnYgstar["CI_TMLE2",])) > truth)
cov_EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["CI_TMLE1",])) < truth & unlist(lapply(dn, function(x) x$EnYgstar["CI_TMLE2",])) > truth)

cov_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE1",])) < truth & unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE2",])) > truth)
cov_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE1",])) < truth & unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE2",])) > truth)
cov_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE1",])) < truth & unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE2",])) > truth)

cov_da_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE1",] < x$E0Ygstar["E0Ygstar.CVTMLE",]))  & unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE2",] > x$E0Ygstar["E0Ygstar.CVTMLE",])))
cov_da_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE1",] < x$E0Ygstar["E0Ygstar.CVTMLE",]))  & unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE2",] > x$E0Ygstar["E0Ygstar.CVTMLE",])))
cov_da_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE1",] < x$E0Ygstar["E0Ygstar.CVTMLE",]))  & unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE2",] > x$E0Ygstar["E0Ygstar.CVTMLE",])))







load("simulations/EYgstar/psi_SL.RData")
alpha = alpha_psi_glm
c = c_psi_glm
dn = dn_psi_glm

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

cov_EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["CI_TMLE1",])) < truth & unlist(lapply(alpha, function(x) x$EnYgstar["CI_TMLE2",])) > truth)
cov_EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["CI_TMLE1",])) < truth & unlist(lapply(c, function(x) x$EnYgstar["CI_TMLE2",])) > truth)
cov_EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["CI_TMLE1",])) < truth & unlist(lapply(dn, function(x) x$EnYgstar["CI_TMLE2",])) > truth)

cov_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE1",])) < truth & unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE2",])) > truth)
cov_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE1",])) < truth & unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE2",])) > truth)
cov_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE1",])) < truth & unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE2",])) > truth)

cov_da_CV.EnYgstar_alpha = mean(unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE1",] < x$E0Ygstar["E0Ygstar.CVTMLE",]))  & unlist(lapply(alpha, function(x) x$EnYgstar["CI_CV.TMLE2",] > x$E0Ygstar["E0Ygstar.CVTMLE",])))
cov_da_CV.EnYgstar_c = mean(unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE1",] < x$E0Ygstar["E0Ygstar.CVTMLE",]))  & unlist(lapply(c, function(x) x$EnYgstar["CI_CV.TMLE2",] > x$E0Ygstar["E0Ygstar.CVTMLE",])))
cov_da_CV.EnYgstar_dn = mean(unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE1",] < x$E0Ygstar["E0Ygstar.CVTMLE",]))  & unlist(lapply(dn, function(x) x$EnYgstar["CI_CV.TMLE2",] > x$E0Ygstar["E0Ygstar.CVTMLE",])))
