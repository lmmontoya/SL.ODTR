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




##### DGP AL bin ######
DGP_bin_complex_true_values = get_true_vals(DGP_fun = DGP_bin_complex)
save(DGP_bin_complex_true_values, file = "/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/true values/DGP_bin_complex_true_values.RData")

##### DGP bin simple ######
DGP_bin_simple_true_values = get_true_vals(DGP_fun = DGP_bin_simple)
save(DGP_bin_simple_true_values, file = "/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/true values/DGP_bin_simple_true_values.RData")

##### DGP bin6 ######
DGP_bin6_true_values = get_true_vals(DGP_fun = DGP_bin6)
save(DGP_bin6_true_values, file = "/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/true values/DGP_bin6_true_values.RData")

##### DGP AL bin min######
DGP_bin_complex_min_true_values = get_true_vals(DGP_fun = DGP_bin_complex_min)
save(DGP_bin_complex_min_true_values, file = "/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/true values/DGP_bin_complex_min_true_values.RData")

##### DGP cont######
DGP_cont_true_values = get_true_vals(DGP_fun = DGP_cont)
save(DGP_cont_true_values, file = "/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/true values/DGP_cont_true_values.RData")

##### DGP null######
DGP_null_true_values = get_true_vals(DGP_fun = DGP_null)
save(DGP_null_true_values, file = "/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/true values/DGP_null_true_values.RData")
