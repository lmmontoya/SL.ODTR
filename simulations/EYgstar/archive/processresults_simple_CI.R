set.seed(123)
W = data.frame(W1 = rnorm(10e7))
d0 = as.numeric(QAW_bin_simple(A = 1, W) - QAW_bin_simple(A = 0, W) <= 0)
truth = mean(QAW_bin_simple(A = d0, W))

load("simulations/EYgstar/CI_glm_simple.RData")
alpha = alpha_CI_glm
c = c_CI_glm
dn = dn_CI_glm

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


summary(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)
summary(unlist(lapply(c, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)
summary(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)

abs(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) <= abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)
abs(unlist(lapply(c, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) <= abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)

rm(list = ls())

load("simulations/true values/DGP_bin_simple_true_values.RData")
truth = DGP_bin_simple_true_values$EYd_star

load("simulations/EYgstar/psi_glmSL_simple.RData")
alpha = alpha_psi_glmSL
c = c_psi_glmSL
dn = dn_psi_glmSL

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

summary(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)
summary(unlist(lapply(c, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)
summary(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)

class = rep(NA, times = 1000)
class[abs(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) == abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)] = "eq"
class[abs(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) > abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)] = "greater - bad"
class[abs(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) < abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)] = "less than - good"
table(class)

abs(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) <= abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)
abs(unlist(lapply(c, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) <= abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)









#### SL ####
rm(list = ls())

load("simulations/true values/DGP_bin_simple_true_values.RData")
truth = DGP_bin_simple_true_values$EYd_star

load("simulations/EYgstar/psi_SL_simple.RData")
alpha = alpha_psi_SL[!(1:1000 %in% seq(from = 15, to = 1000, by = 24))]
c = c_psi_SL[!(1:1000 %in% seq(from = 15, to = 1000, by = 24))]
dn = dn_psi_SL[!(1:1000 %in% seq(from = 15, to = 1000, by = 24))]

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

summary(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)
summary(unlist(lapply(c, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)
summary(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)

class = rep(NA, times = 1000)
class[abs(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) == abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)] = "eq"
class[abs(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) > abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)] = "greater - bad"
class[abs(unlist(lapply(alpha, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) < abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)] = "less than - good"
table(class)
abs(unlist(lapply(c, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth) <= abs(unlist(lapply(dn, function(x) x$E0Ygstar["E0Ygstar.nonCVTMLE",])) - truth)


