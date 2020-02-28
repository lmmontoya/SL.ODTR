

load_object = function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  toreturn = lapply(1:length(ls(tmp)), function(x) tmp[[ls(tmp)[x]]])
  names(toreturn) = ls(tmp)
  return(toreturn)
}


makeplotdf_ODTR = function(x, ODTR, truevalues) {

  estimates = ODTR[[x]]
  est = estimates[,grep(colnames(estimates), pattern = "EYdn")]
  truth = as.numeric(truevalues["EYd_star"])
  Bias = mean(est - truth)
  Variance = var(est)
  MSE = mean((est - truth)^2)

  match_dopt = mean(estimates[,grep(colnames(estimates), pattern = "match")])
  mean_dopt = colMeans(estimates[,grep(colnames(estimates), pattern = "mean_dopt")])
  df = data.frame(Library = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][1]+1, stop = gregexpr(pattern ='_', names(ODTR[x]))[[1]][2]-1),
                  Metalearner = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][2]+1, stop = gregexpr(pattern ='_', names(ODTR[x]))[[1]][3]-1),
                  Risk = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][3]+1, stop = nchar(names(ODTR[x]))),
                  Estimates = mean(est),
                  minQ = quantile(est, probs = 0.025),
                  maxQ = quantile(est, probs = 0.975),
                  Bias = Bias,
                  Variance = Variance,
                  MSE = MSE)
  df = cbind(df, match_dopt, t(mean_dopt))

  return(df)

}


maketable_ODTR = function(ODTR, truevalues, caption, scalebox = .7){

  makedftable_ODTR = function(x, ODTR, truevalues){

    estimates = ODTR[x]
    est = estimates[[1]][,grep(colnames(estimates[[1]]), pattern = "EYdn_")]
    #colnames(est) = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][1]+1, stop = gregexpr(pattern ='_', names(ODTR[x]))[[1]][2]-1)
    truth = as.numeric(truevalues["EYd_star"])
    Bias = mean(est - truth)
    Variance = var(est)
    MSE = mean((est - truth)^2)
    propcorrect = mean(estimates[[1]][,grep(colnames(estimates[[1]]), pattern = "match")])
    coefs = data.frame(t(colMeans(data.frame(estimates[[1]][,grep(colnames(estimates[[1]]), pattern = "coef")]))))
    colnames(coefs) = substr(colnames(coefs), start = unlist(gregexpr(pattern ='SL', colnames(coefs))), stop = nchar(colnames(coefs)))
    if (length(grep("_All", colnames(coefs)))>0 ) {
      colnames(coefs)[grep("_All", colnames(coefs))] = substr(colnames(coefs)[grep("_All", colnames(coefs))], start = 1, stop = nchar(colnames(coefs)[grep("_All", colnames(coefs))])-4)
    }
    if (length(grep("coef.", colnames(coefs)))>0){
      colnames(coefs)[grep("coef.", colnames(coefs))] = substr(colnames(coefs)[grep("coef.", colnames(coefs))], start = 6, stop = nchar(colnames(coefs)[grep("coef.", colnames(coefs))]))
    }
    toreturn = cbind(Bias, Variance, MSE, propcorrect, coefs)
    rownames(toreturn) = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][1]+1, stop = nchar(names(ODTR[x])))

    return(toreturn)
  }

  df = do.call("rbind", lapply(1:length(ODTR), makedftable_ODTR, ODTR = ODTR, truevalues = truevalues))
  return(df)

}





# for real data
plot_ODTR_results = function(results, risk, EY, EY1, CI_EY1, EY0, CI_EY0, title) {

  estimates = results$EYdopt_estimates
  estimates = estimates[-grep("LTMLE", names(estimates))]
  est = estimates[grep(names(estimates), pattern = "Psi_")]
  odtr = data.frame(Risk = risk,
                  EYdn = factor(names(est), levels = names(est)),
                  Estimates = est,
                  lowerCI = c(estimates["CI_unadj1"], EY , estimates["CI_IPTW1"], estimates["CI_IPTW_DR1"], estimates["CI_TMLE1"], estimates["CI_CV.TMLE1"]), #estimates["CI_LTMLE1"],
                  upperCI = c(estimates["CI_unadj2"], EY , estimates["CI_IPTW2"], estimates["CI_IPTW_DR2"], estimates["CI_TMLE2"], estimates["CI_CV.TMLE2"]), #estimates["CI_LTMLE2"],
                  prop_dopt = mean(results$SL.odtr$dopt))

  pd <- position_dodge(width = 0.7)

  odtr %>%
    ggplot(aes(x = Risk, y = Estimates, group = EYdn)) +
    geom_point(position = pd, size = 3, shape = 15) +
    geom_point(aes(colour = EYdn), shape = 15, position = pd, size = 2.5) +
    geom_errorbar(aes(ymin = odtr$lowerCI, ymax = odtr$upperCI), width = .4, position = pd) +
    geom_hline(yintercept = EY, colour = "blue") +
    geom_hline(yintercept = EY1, colour = "red") +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = min(CI_EY1), ymax = max(CI_EY1)), fill="red", alpha = 0.01) +
    geom_hline(yintercept = EY0, colour = "green") +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = min(CI_EY0), ymax = max(CI_EY0)), fill="green", alpha = 0.01) +
    scale_x_discrete(name ="Rule estimator (risk type)") +
    ylab("Blue = E[Y]; Red = E[Y1]; Green = E[Y0] ") +
    ggtitle(title) +
    annotate("text", x = 0.5, y = max(odtr$upperCI, EY, na.rm = T)+.032, label = paste0(round(unique(odtr$prop_dopt)*100, 2),"%"), colour = "black") +
    annotate("text", x = 0, y = max(odtr$upperCI, EY, na.rm = T)+.032, xmin = -0.5, label = "~underline('% treated under ODR:')", colour = "black", parse = TRUE) +
    ylim(min(c(odtr$lowerCI, CI_EY1, CI_EY0), na.rm = T)-.005, max(c(odtr$upperCI, EY, CI_EY1, CI_EY0), na.rm = T)+.035) +
    theme_bw() +
    theme(panel.border = element_blank())



}



