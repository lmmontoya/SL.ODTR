set.seed(12312018)
set.seed(1)
m = 10000
n = 10000

get_true_vals = function(DGP_fun) {

  # Expected value of Y
  EY = mean(do.call("rbind", lapply(1:m, function(x) mean(DGP_fun(n = n)$Y_star))))
  print("EY done")

  # Expected value of Y setting A to 1
  EY1 = mean(do.call("rbind", lapply(1:m, function(x) mean(DGP_fun(n = n, a = 1)$Y_star))))
  print("EY1 done")

  # Expected value of Y setting A to 0
  EY0 = mean(do.call("rbind", lapply(1:m, function(x) mean(DGP_fun(n = n, a = 0)$Y_star))))
  print("EY0 done")

  # Expected value of Y under simple dynamic rule
  EYd = mean(do.call("rbind", lapply(1:m, function(x) mean(DGP_fun(n = n, dA = "simple dynamic")$Y_star))))
  print("EYd done")

  # Expected value of Y under ODTR
  EYd_star = mean(do.call("rbind", lapply(1:m, function(x) mean(DGP_fun(n = n, dA = "ODTR")$Y_star))))
  print("EYd_star done")

  # Expected value of Y under ODTR-RC when kappa = 0.9
#  EYd_tilde0.9 = mean(do.call("rbind", lapply(1:m, function(x) mean(DGP_fun(n = n, dA = "ODTR-RC", kappa = 0.9)$Y_star))))
#  print("EYd_tile0.9 done")

  # Expected value of Y under ODTR-RC when kappa = 0.4
#  EYd_tilde0.4 = mean(do.call("rbind", lapply(1:m, function(x) mean(DGP_fun(n = n, dA = "ODTR-RC", kappa = 0.4)$Y_star))))
#  print("EYd_tile0.4 done")

  # Mean proportion of patients treated kappa = 0.4 - ODTR-RC
#  mean_dstar_0.4 = mean(do.call("rbind", lapply(1:m, function(x) mean(DGP_fun(n = n, dA = "ODTR-RC", kappa = 0.4)$A_star))))
#  print("mean_dstar done")

  # Mean proportion of patients treated kappa = .9 - ODTR
#  mean_dstar_0.9 = mean(do.call("rbind", lapply(1:m, function(x) mean(DGP_fun(n = n, dA = "ODTR-RC", kappa = 0.9)$A_star))))
#  print("mean_dstar done")

  # Mean proportion of patients treated - ODTR
  mean_dstar = mean(do.call("rbind", lapply(1:m, function(x) mean(DGP_fun(n = n, dA = "ODTR")$A_star))))
  print("mean_dstar done")

  truevals = data.frame(EY = EY,
                        EY1 = EY1,
                        EY0 = EY0,
                        EYd = EYd,
                        EYd_star = EYd_star,
                        #EYd_tilde0.9 = EYd_tilde0.9,
                        #EYd_tilde0.4 = EYd_tilde0.4,
                        mean_dstar = mean_dstar)

}


source('R/1DGPfunctions.R')

###### DGP 4 ######
DGP4_true_values = get_true_vals(DGP_fun = DGP4)
save(DGP4_true_values, file = "simulations/true values/DGP4_true_values.RData")

###### DGP 5 ######
DGP5_true_values = get_true_vals(DGP_fun = DGP5)
save(DGP5_true_values, file = "simulations/true values/DGP5_true_values.RData")

###### DGP 6 ######
DGP6_true_values = get_true_vals(DGP_fun = DGP6)
save(DGP6_true_values, file = "simulations/true values/DGP6_true_values.RData")


##### DGP AL bin ######
DGP_AL_bin_true_values = get_true_vals(DGP_fun = DGP_AL_bin)
save(DGP_AL_bin_true_values, file = "simulations/true values/DGP_AL_bin_true_values.RData")

##### DGP AL cont ######
DGP_AL_cont_true_values = get_true_vals(DGP_fun = DGP_AL_cont)
save(DGP_AL_cont_true_values, file = "simulations/true values/DGP_AL_cont_true_values.RData")


##### DGP smooth ######
DGP_smooth_true_values = get_true_vals(DGP_fun = DGP_smooth)
save(DGP_smooth_true_values, file = "simulations/true values/DGP_smooth_true_values.RData")

##### DGP jump ######
DGP_jump_true_values = get_true_vals(DGP_fun = DGP_jump)
save(DGP_jump_true_values, file = "simulations/true values/DGP_jump_true_values.RData")

##### DGP sin ######
DGP_sin_true_values = get_true_vals(DGP_fun = DGP_sin)
save(DGP_sin_true_values, file = "simulations/true values/DGP_sin_true_values.RData")


##### DGP smooth2 ######
DGP_smooth2_true_values = get_true_vals(DGP_fun = DGP_smooth2)
save(DGP_smooth2_true_values, file = "simulations/true values/DGP_smooth2_true_values.RData")

