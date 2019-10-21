

load_object = function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  toreturn = lapply(1:length(ls(tmp)), function(x) tmp[[ls(tmp)[x]]])
  names(toreturn) = ls(tmp)
  return(toreturn)
}



makeplot_ODTR = function(ODTR, truevalues, title){

  makeplotdf_ODTR = function(x, ODTR) {

    estimates = ODTR[[x]]
    est = estimates[,grep(colnames(estimates), pattern = "EYdn")]
    match_dopt = mean(estimates[,grep(colnames(estimates), pattern = "match")])
    mean_dopt = colMeans(estimates[,grep(colnames(estimates), pattern = "mean_dopt")])
    df = data.frame(Risk = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][1]+1, stop = gregexpr(pattern ='_', names(ODTR[x]))[[1]][2]-1),
                    Estimates = mean(est),
                    minQ = quantile(est, probs = 0.025),
                    maxQ = quantile(est, probs = 0.975))
    df = cbind(df, match_dopt, t(mean_dopt))


    #estimates = ODTR[[x]] #ODTR[x]
    #est = estimates[,"EYdn"]
    #match_dopt = mean(estimates[,"match_dopt"])
    #df = data.frame(Risk = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][1]+1, stop = gregexpr(pattern ='_', names(ODTR[x]))[[1]][2]-1),
    #                Estimates = mean(est),
    #                minQ = quantile(est, probs = 0.025),
    #                maxQ = quantile(est, probs = 0.975),
    #                match_dopt = match_dopt)

    return(df)

  }

  odtr = do.call("rbind", lapply(1:length(ODTR), makeplotdf_ODTR, ODTR = ODTR))

  pd <- position_dodge(width = 0.7)

  odtr %>%
    ggplot(aes(x = Risk, y = Estimates)) +
    geom_point(position = pd, size = 3, shape = 15) +
   # geom_point(aes(colour = EYdn), shape = 15, position = pd, size = 2.5) +
    geom_errorbar(aes(ymin = minQ, ymax = maxQ), width = .4, position = pd) +
    geom_hline(yintercept = (truevalues$EYd_star), colour = "blue") +
    scale_x_discrete(name ="Rule estimator (risk type)") +
    ylab(expression(paste(E,"[",Y[d^{"*"}],"]"))) +
    ggtitle(title) +
    annotate("text", x = 1, y = max(odtr$maxQ, truevalues$EYd_star)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[1]])*100, 1),"%"), colour = "red") +
    annotate("text", x = 2, y = max(odtr$maxQ, truevalues$EYd_star)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[2]])*100, 1),"%"), colour = "red") +
    annotate("text", x = 3, y = max(odtr$maxQ, truevalues$EYd_star)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[3]])*100, 1),"%"), colour = "red") +

     annotate("text", x = 0, y = max(odtr$maxQ, truevalues$EYd_star)+.032, xmin = -0.5, label = "~underline('% match true dopt:')", colour = "red", parse = TRUE) +

    annotate("text", x = 1, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .05, label = paste0(round(unique(odtr$mean_dopt[odtr$Risk == levels(odtr$Risk)[1]])*100, 1),"%"), colour = "black") +
    annotate("text", x = 2, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .05, label = paste0(round(unique(odtr$mean_dopt[odtr$Risk == levels(odtr$Risk)[2]])*100, 1),"%"), colour = "black") +
    annotate("text", x = 3, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .05, label = paste0(round(unique(odtr$mean_dopt[odtr$Risk == levels(odtr$Risk)[3]])*100, 1),"%"), colour = "black") +

    annotate("text", x = 0, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .05, xmin = -0.5, label = "~underline('% treated:')", colour = "black", parse = TRUE) +

    ylim(min(odtr$minQ)-.005, max(odtr$maxQ, truevalues$EYd_star)+.035) +
    theme_bw() +
    theme(panel.border = element_blank())

  #
  # odtr %>%
  #   ggplot(aes(x = Risk, y = Estimates)) +
  #   geom_point(position = pd, size = 3, shape = 15) +
  #   geom_point(aes(colour = Risk), shape = 15, position = pd, size = 2.5) +
  #   geom_errorbar(aes(ymin = minQ, ymax = maxQ), width = .4, position = pd) +
  #   geom_hline(yintercept = (truevalues$EYd_star), colour = "blue") +
  #   scale_x_discrete(name ="Rule estimator (risk type)") +
  #   ylab(expression(paste(E,"[",Y[d^{"*"}],"]"))) +
  #   ggtitle(paste("Simulation Results - Optimal DTR -", title)) +
  #   annotate("text", x = 1, y = max(odtr$maxQ)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[1]])*100, 1),"%"), colour = "red") +
  #   annotate("text", x = 2, y = max(odtr$maxQ)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[2]])*100, 1),"%"), colour = "red") +
  #   annotate("text", x = 3, y = max(odtr$maxQ)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[3]])*100, 1),"%"), colour = "red") +
  #   annotate("text", x = 4, y = max(odtr$maxQ)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[4]])*100, 1),"%"), colour = "red") +
  #   annotate("text", x = 5, y = max(odtr$maxQ)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[5]])*100, 1),"%"), colour = "red") +
  #   annotate("text", x = 6, y = max(odtr$maxQ)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[6]])*100, 1),"%"), colour = "red") +
  #   annotate("text", x = 0, y = max(odtr$maxQ)+.032, xmin = -0.5, label = "~underline('% match dopt:')", colour = "red", parse = TRUE) +
  #   ylim(min(odtr$minQ)-.005, max(odtr$maxQ)+.035) +
  #   theme_bw() +
  #   theme(panel.border = element_blank())

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
    coefs = colMeans(data.frame(estimates[[1]][,grep(colnames(estimates[[1]]), pattern = "coef")]))
    toreturn = data.frame(Bias, Variance, MSE, t(coefs))
    rownames(toreturn) = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][1]+1, stop = gregexpr(pattern ='_', names(ODTR[x]))[[1]][2]-1)

    return(toreturn)
  }

  df = do.call("rbind", lapply(1:length(ODTR), makedftable_ODTR, ODTR = ODTR, truevalues = truevalues))
  tbl = xtable(t(df), caption = caption, digits = 5)
#  hlines <- c(-1, 0, seq(from = 6, to = nrow(tbl), by = 6))
#  align(tbl) <- c(rep("r", ncol(df)-4), rep("|r", 5))
  print(tbl, include.rownames = T, scalebox = scalebox)#, hline.after = hlines, scalebox = scalebox)
}






make_table_EYdopt = function(EYdopt, truevalues) {

  truth = truevalues$EYd_star
  Psi = data.frame(EYdopt[,grep("Psi_", colnames(EYdopt))])
  Psi$Psi_LTMLE = Psi$Psi_LTMLE.tmle
  Psi$Psi_LTMLE.tmle = NULL
  colnames(Psi) = substr(colnames(Psi), start = 5, stop = nchar(colnames(Psi)))
  CI = EYdopt[,grep(colnames(EYdopt), pattern = "^CI")]
  Bias = colMeans(Psi - truth)
  Variance = diag(var(Psi))
  MSE = colMeans((Psi - truth)^2)
  results = data.frame(Bias, Variance, MSE)
  Coverage = colMeans(truth > CI[,grep(colnames(CI), pattern = "1$")] & truth < CI[,grep(colnames(CI), pattern = "2$")])
  names(Coverage) = substr(names(Coverage), start = 4, stop = nchar(names(Coverage))-1)
  results$Coverage = NA
  results$Coverage = Coverage[rownames(results)]
  results[is.na(results)] = "-"

  return(results)


}



make_plot_EYdopt = function(EYdopt, truevalues) {

  Psi = data.frame(EYdopt[,grep("Psi_", colnames(EYdopt))])
  Psi$Psi_LTMLE = Psi$Psi_LTMLE.tmle
  Psi$Psi_LTMLE.tmle = NULL
  colnames(Psi) = substr(colnames(Psi), start = 5, stop = nchar(colnames(Psi)))
  df = data.frame(Estimator = factor(colnames(Psi), levels = colnames(Psi)),
                  Estimates = colMeans(Psi),
                  minQ = apply(Psi, 2, quantile, probs = 0.025),
                  maxQ = apply(Psi, 2, quantile, probs = 0.975))
  pd <- position_dodge(width = 0.7)
  df %>%
    ggplot(aes(x = Estimator, y = Estimates)) +
    geom_point(position = pd, size = 3, shape = 15) +
    geom_point(aes(colour = Estimator), shape = 15, position = pd, size = 2.5) +
    geom_errorbar(aes(ymin = minQ, ymax = maxQ), width = .4, position = pd) +
    geom_hline(yintercept = (truevalues$EYd_star), colour = "blue") +
    scale_x_discrete(name ="Estimator") +
    ylab(expression(paste(E,"[",Y[d^{"*"}],"]"))) +
    ggtitle("Simulation Results - EYdopt with true dopt") +
    theme_bw() +
    theme(panel.border = element_blank())

}













makeplot_ODTR_RC = function(ODTR, truevalues, title){

  makeplotdf_ODTR_RC = function(x, ODTR) {

    estimates = ODTR[[x]]
    est = estimates[,grep(colnames(estimates), pattern = "EYdn")]
    #colnames(est) = substr(colnames(est), stop = sapply(gregexpr(pattern ='_O', colnames(est)), "[[", 1)-1, start = 1)
    #c("Unadj.", "g-comp", "IPTW", "IPTW-DR", "TMLE") #PUT IN"CV.TMLE_ODTR"
    match_dopt = colMeans(estimates[,grep(colnames(estimates), pattern = "match")])
    mean_dopt = colMeans(estimates[,grep(colnames(estimates), pattern = "mean")])
    df = data.frame(Risk = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][1]+1, stop = gregexpr(pattern ='_', names(ODTR[x]))[[1]][2]-1),
                    EYdn = factor(colnames(est), levels = colnames(est)),
                    Estimates = colMeans(est),
                    minQ = apply(est, 2, quantile, probs = 0.025),
                    maxQ = apply(est, 2, quantile, probs = 0.975))
    df = cbind(df, t(match_dopt), t(mean_dopt))


    #estimates = ODTR[[x]] #ODTR[x]
    #est = estimates[,"EYdn"]
    #match_dopt = mean(estimates[,"match_dopt"])
    #df = data.frame(Risk = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][1]+1, stop = gregexpr(pattern ='_', names(ODTR[x]))[[1]][2]-1),
    #                Estimates = mean(est),
    #                minQ = quantile(est, probs = 0.025),
    #                maxQ = quantile(est, probs = 0.975),
    #                match_dopt = match_dopt)

    return(df)

  }

  odtr = do.call("rbind", lapply(1:length(ODTR), makeplotdf_ODTR_RC, ODTR = ODTR))

  pd <- position_dodge(width = 0.7)

  odtr %>%
    ggplot(aes(x = Risk, y = Estimates, group = EYdn)) +
    geom_point(position = pd, size = 3, shape = 15) +
    geom_point(aes(colour = EYdn), shape = 15, position = pd, size = 2.5) +
    geom_errorbar(aes(ymin = minQ, ymax = maxQ), width = .4, position = pd) +
    geom_hline(yintercept = (truevalues$EYd_star), colour = "blue") +
    scale_x_discrete(name ="Rule estimator (risk type)") +
    ylab(expression(paste(E,"[",Y[d^{"*"}],"]"))) +
    ggtitle("Simulation Results - Optimal DTR") +
    annotate("text", x = 1-.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032, label = paste0(round(unique(odtr$match_dopt_Qbar0[odtr$Risk == levels(odtr$Risk)[1]])*100, 1),"%"), colour = "red") +
    annotate("text", x = 1+.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032, label = paste0(round(unique(odtr$match_dopt_QbarHat[odtr$Risk == levels(odtr$Risk)[1]])*100, 1),"%"), colour = "blue") +
    annotate("text", x = 2-.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032, label = paste0(round(unique(odtr$match_dopt_Qbar0[odtr$Risk == levels(odtr$Risk)[2]])*100, 1),"%"), colour = "red") +
    annotate("text", x = 2+.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032, label = paste0(round(unique(odtr$match_dopt_QbarHat[odtr$Risk == levels(odtr$Risk)[2]])*100, 1),"%"), colour = "blue") +
    annotate("text", x = 3-.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032, label = paste0(round(unique(odtr$match_dopt_Qbar0[odtr$Risk == levels(odtr$Risk)[3]])*100, 1),"%"), colour = "red") +
    annotate("text", x = 3+.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032, label = paste0(round(unique(odtr$match_dopt_QbarHat[odtr$Risk == levels(odtr$Risk)[3]])*100, 1),"%"), colour = "blue") +
    annotate("text", x = 0, y = max(odtr$maxQ, truevalues$EYd_star)+.032, xmin = -0.5, label = "~underline('% match true ODR:')", colour = "purple", parse = TRUE) +

    annotate("text", x = 1-.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .005, label = paste0(round(unique(odtr$mean_dopt_Qbar0[odtr$Risk == levels(odtr$Risk)[1]])*100, 1),"%"), colour = "red") +
    annotate("text", x = 1+.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .005, label = paste0(round(unique(odtr$mean_dopt_QbarHat[odtr$Risk == levels(odtr$Risk)[1]])*100, 1),"%"), colour = "blue") +
    annotate("text", x = 2-.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .005, label = paste0(round(unique(odtr$mean_dopt_Qbar0[odtr$Risk == levels(odtr$Risk)[2]])*100, 1),"%"), colour = "red") +
    annotate("text", x = 2+.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .005, label = paste0(round(unique(odtr$mean_dopt_QbarHat[odtr$Risk == levels(odtr$Risk)[2]])*100, 1),"%"), colour = "blue") +
    annotate("text", x = 3-.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .005, label = paste0(round(unique(odtr$mean_dopt_Qbar0[odtr$Risk == levels(odtr$Risk)[3]])*100, 1),"%"), colour = "red") +
    annotate("text", x = 3+.155, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .005, label = paste0(round(unique(odtr$mean_dopt_QbarHat[odtr$Risk == levels(odtr$Risk)[3]])*100, 1),"%"), colour = "blue") +
    annotate("text", x = 0, y = max(odtr$maxQ, truevalues$EYd_star)+.032 - .005, xmin = -0.5, label = "~underline('% treated:')", colour = "purple", parse = TRUE) +

    ylim(min(odtr$minQ)-.005, max(odtr$maxQ, truevalues$EYd_star)+.035) +
    theme_bw() +
    theme(panel.border = element_blank())



}














makeplot_results = function(results, truevalues, title, rc, dopt0 = FALSE, LTMLE = FALSE){

  makeplotdf_results = function(x, results, truevalues) {

    estimates = results[[x]]
    est = estimates[,grep(colnames(estimates), pattern = "Psi_")]
    #colnames(est) = substr(colnames(est), stop = sapply(gregexpr(pattern ='_O', colnames(est)), "[[", 1)-1, start = 1)
    #c("Unadj.", "g-comp", "IPTW", "IPTW-DR", "TMLE") #PUT IN"CV.TMLE_ODTR"
    match_dopt = mean(estimates[,grep(colnames(estimates), pattern = "match")])
    mean_dopt = mean(estimates[,grep(colnames(estimates), pattern = "mean")])
    df = data.frame(Risk = substr(names(results[x]), start = 21, stop = nchar(names(results)[x])),
                    EYdn = factor(colnames(est), levels = colnames(est)),
                    Estimates = colMeans(est, na.rm = T),
                    minQ = apply(est, 2, quantile, probs = 0.025, na.rm = T),
                    maxQ = apply(est, 2, quantile, probs = 0.975, na.rm = T))
    df = cbind(df, match_dopt, mean_dopt)


    #estimates = ODTR[[x]] #ODTR[x]
    #est = estimates[,"EYdn"]
    #match_dopt = mean(estimates[,"match_dopt"])
    #df = data.frame(Risk = substr(names(ODTR[x]), start = gregexpr(pattern ='_', names(ODTR[x]))[[1]][1]+1, stop = gregexpr(pattern ='_', names(ODTR[x]))[[1]][2]-1),
    #                Estimates = mean(est),
    #                minQ = quantile(est, probs = 0.025),
    #                maxQ = quantile(est, probs = 0.975),
    #                match_dopt = match_dopt)

    return(df)

  }


  odtr = do.call("rbind", lapply(1:length(results), makeplotdf_results, results = results))

  if (!LTMLE) {
    odtr = odtr[-grep("LTMLE", odtr$EYdn),]
  }

  if (!dopt0) {
    odtr = odtr[-grep("0", odtr$EYdn),]
  }

  if (rc == "none") {
    odtr = odtr[-grep("0.", odtr$Risk),]
    odtr$Risk = factor(odtr$Risk)
    truevalues = truevalues[,c("EYd_star", "mean_dstar")]
  } else {
    odtr = odtr[grep(rc, odtr$Risk),]
    truevalues = truevalues[,paste0(c("EYd_tilde", "mean_dstar_"), rc)]
    truevalues$EYd_star = truevalues[,paste0(c("EYd_tilde"), rc)]
    truevalues$mean_dstar = truevalues[,paste0(c("mean_dstar_"), rc)]
    odtr$Risk = factor(odtr$Risk)
  }


  pd <- position_dodge(width = 0.7)

  odtr %>%
    ggplot(aes(x = Risk, y = Estimates, group = EYdn)) +
    geom_point(position = pd, size = 3, shape = 15) +
    geom_point(aes(colour = EYdn), shape = 15, position = pd, size = 2.5) +
    geom_errorbar(aes(ymin = minQ, ymax = maxQ), width = .4, position = pd) +
    geom_hline(yintercept = (truevalues$EYd_star), colour = "blue") +
    scale_x_discrete(name ="Rule estimator (risk type)") +
    ylab(expression(paste(E,"[",Y[d^{"*"}],"]"))) +
    ggtitle(title) +
    annotate("text", x = 1, y = max(odtr$maxQ, truevalues$EYd_star, na.rm = T)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[1]])*100, 1),"%"), colour = "black") +
    annotate("text", x = 2, y = max(odtr$maxQ, truevalues$EYd_star, na.rm = T)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[2]])*100, 1),"%"), colour = "black") +
    annotate("text", x = 3, y = max(odtr$maxQ, truevalues$EYd_star, na.rm = T)+.032, label = paste0(round(unique(odtr$match_dopt[odtr$Risk == levels(odtr$Risk)[3]])*100, 1),"%"), colour = "black") +
    annotate("text", x = 0, y = max(odtr$maxQ, truevalues$EYd_star, na.rm = T)+.032, xmin = -0.5, label = "~underline('% match true ODR:')", colour = "black", parse = TRUE) +

    annotate("text", x = 1, y = max(odtr$maxQ, truevalues$EYd_star, na.rm = T)+.032 - .01, label = paste0(round(unique(odtr$mean_dopt[odtr$Risk == levels(odtr$Risk)[1]])*100, 1),"%"), colour = "black") +
    annotate("text", x = 2, y = max(odtr$maxQ, truevalues$EYd_star, na.rm = T)+.032 - .01, label = paste0(round(unique(odtr$mean_dopt[odtr$Risk == levels(odtr$Risk)[2]])*100, 1),"%"), colour = "black") +
    annotate("text", x = 3, y = max(odtr$maxQ, truevalues$EYd_star, na.rm = T)+.032 - .01, label = paste0(round(unique(odtr$mean_dopt[odtr$Risk == levels(odtr$Risk)[3]])*100, 1),"%"), colour = "black") +
    annotate("text", x = 0, y = max(odtr$maxQ, truevalues$EYd_star, na.rm = T)+.032 - .01, xmin = -0.5, label = "~underline('% treated:')", colour = "black", parse = TRUE) +

    ylim(min(odtr$minQ, na.rm = T)-.005, max(odtr$maxQ, truevalues$EYd_star, na.rm = T)+.035) +
    theme_bw() +
    theme(panel.border = element_blank())



}







maketable_results = function(results, truevalues, caption, scalebox = .7, rc){

  makedftable_results = function(x, results, truevalues, rc){

    if (rc == "none") {
      truevalues = truevalues[,c("EYd_star", "mean_dstar")]
    } else {
      truevalues = truevalues[,paste0(c("EYd_tilde", "mean_dstar_"), rc)]
      truevalues$EYd_star = truevalues[,paste0(c("EYd_tilde"), rc)]
      truevalues$mean_dstar = truevalues[,paste0(c("mean_dstar_"), rc)]
    }

    estimates = results[x]
    est = data.frame(estimates[[1]][,grep(colnames(estimates[[1]]), pattern = "Psi_")])
    colnames(est) = substr(colnames(est), start = 5, stop = nchar(colnames(est)))
    est$LTMLE = est$LTMLE.tmle
    est$LTMLE_dopt0 = est$LTMLE.tmle_dopt0
    est$LTMLE.tmle = est$LTMLE.tmle_dopt0 = NULL
    CI = estimates[[1]][,grep(colnames(estimates[[1]]), pattern = "^CI")]
    truth_E0Yd0 = as.numeric(truevalues["EYd_star"])
    truth_E0Ydn = estimates[[1]][,"EY0dn"]
    truth_E0Ydn_CVTMLE = estimates[[1]][,"EY0dn_CVTMLE"]
    Bias_EY0d0 = colMeans(est - truth_E0Yd0)
    Bias_EY0dn = colMeans(est - truth_E0Ydn)
    Bias_EY0dn_CVTMLE = colMeans(est - truth_E0Ydn_CVTMLE)
    Variance = diag(var(est))
    MSE_EY0d0 = colMeans((est - truth_E0Yd0)^2)
    MSE_EY0dn = colMeans((est - truth_E0Ydn)^2)
    MSE_EY0dn_CVTMLE = colMeans((est - truth_E0Ydn_CVTMLE)^2)
    toreturn = data.frame(Bias_EY0d0, Bias_EY0dn, Bias_EY0dn_CVTMLE,
                          Variance,
                          MSE_EY0d0, MSE_EY0dn, MSE_EY0dn_CVTMLE)

    Coverage_EY0d0 = colMeans(truth_E0Yd0 > CI[,grep(colnames(CI), pattern = "1")] & truth_E0Yd0 < CI[,grep(colnames(CI), pattern = "2")])
    names(Coverage_EY0d0) = substr(names(Coverage_EY0d0), start = 4, stop = regexpr(pattern = "[1]", text = names(Coverage_EY0d0))-1)
    names(Coverage_EY0d0) = paste0(names(Coverage_EY0d0), rep(c("", "_dopt0"), each = 6))
    toreturn$Coverage_EY0d0 = NA
    toreturn$Coverage_EY0d0 = Coverage_EY0d0[rownames(toreturn)]
    toreturn[is.na(results)] = "-"

    Coverage_EY0dn = colMeans(truth_E0Ydn > CI[,grep(colnames(CI), pattern = "1")] & truth_E0Ydn < CI[,grep(colnames(CI), pattern = "2")])
    names(Coverage_EY0dn) = substr(names(Coverage_EY0dn), start = 4, stop = regexpr(pattern = "[1]", text = names(Coverage_EY0dn))-1)
    names(Coverage_EY0dn) = paste0(names(Coverage_EY0dn), rep(c("", "_dopt0"), each = 6))
    toreturn$Coverage_EY0dn = NA
    toreturn$Coverage_EY0dn = Coverage_EY0dn[rownames(toreturn)]
    toreturn[is.na(results)] = "-"

    Coverage_EY0dn_CVTMLE = colMeans(truth_E0Ydn_CVTMLE > CI[,c("CI_CV.TMLE1", "CI_CV.TMLE1_dopt0")] & truth_E0Ydn_CVTMLE < CI[,c("CI_CV.TMLE2", "CI_CV.TMLE2_dopt0")])
    names(Coverage_EY0dn_CVTMLE) = substr(names(Coverage_EY0dn_CVTMLE), start = 4, stop = regexpr(pattern = "[1]", text = names(Coverage_EY0dn_CVTMLE))-1)
    names(Coverage_EY0dn_CVTMLE) = paste0(names(Coverage_EY0dn_CVTMLE), rep(c("", "_dopt0"), each = 1))
    toreturn$Coverage_EY0dn_CVTMLE = NA
    toreturn$Coverage_EY0dn_CVTMLE = Coverage_EY0dn_CVTMLE[rownames(toreturn)]
    toreturn[is.na(results)] = "-"

    toreturn = data.frame(LF = substr(names(estimates), start = gregexpr(pattern ='_', names(estimates))[[1]][3]+1, stop = nchar(names(estimates))),
                          Prop_treated = mean(estimates[[1]][,"mean_dopt"]),
                          Prop_dopt_match = mean(estimates[[1]][,"match_dopt"]),
                          # Estimator = rownames(toreturn),
                          toreturn)


    return(toreturn)
  }

  df = do.call("rbind", lapply(1:length(results), makedftable_results, results = results, truevalues = truevalues, rc = rc))
  if (rc == "none") {
    df = df[-grep(df$LF, pattern = "0."),]
  } else {
    df = df[grep(df$LF, pattern = rc),]
  }

  tbl = xtable(df, caption = caption, digits = 5)
 # hlines <- c(-1, 0, seq(from = 14, to = nrow(tbl), by = 14))
  align(tbl) <- c(rep("r", ncol(df)-4), rep("|r", 5))
  print(tbl, include.rownames = T, scalebox = scalebox) #hline.after = hlines, )
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
    ylim(min(odtr$lowerCI, na.rm = T)-.005, max(odtr$upperCI, EY, na.rm = T)+.035) +
    theme_bw() +
    theme(panel.border = element_blank())



}



