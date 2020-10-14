# function that computes EYdopt - unadj, TMLE, IPTW, gcomp, CV-TMLE

#' @name EYdopt
#' @aliases EYdopt
#' @title Estimation of E[Ydopt]
#' @description Given a W, A, Y dataset, this function will compute the estimated ODTR using SuperLearner. If a Qbar function is provided that computes the true E[Y|A,W] (e.g., if simulating), the function will also return the true treatment under the optimal rule and other metrics of evaluating the estimated optimal rule's performance. Then, it will estimate E[Ydopt] using g-computation, IPTW, IPTW-DR, TMLE, and CV-TMLE. Follows the framework of Luedtke and van der laan, 2015 and 2016.
#'
#' @param W Data frame of observed baseline covariates
#' @param V Data frame of observed baseline covariates (subset of W) used to design the ODTR
#' @param A Vector of treatment
#' @param Y Vector of outcome (continuous or binary)
#' @param metalearner Discrete ("discrete"), blip-based ("blip"), vote-based SuperLearner ("vote"). Note that if metalearner is "vote" then cannot put in kappa.
#' @param g.SL.library SuperLearner library for estimating txt mechanism
#' @param QAW.SL.library SuperLearner library for estimating outcome regression
#' @param blip.SL.library SuperLearner library for estimating the blip
#' @param risk.type Risk type in order to pick optimal combination of coefficients to combine the candidate algorithms. For (1) MSE risk use "CV MSE"; for (2) -E[Ydopt] risk use "CV IPCWDR" (for -E[Ydopt] estimated using double-robust IPTW) or "CV TMLE" (for -E[Ydopt] estimates using TMLE); (3) For the upper bound of the CI of -E[Ydopt] use "CV TMLE CI"
#' @param dopt.SL.library SuperLearner library for estimating dopt directly. Default is \code{NULL}. Could be "DonV", "Qlearn", "OWL", "EARL", "optclass", "RWL", "treatall", "treatnone". Could also be "all" for all algorithms.
#' @param QAW.fun True outcome regression E[Y|A,W]. Useful for simulations. Default is \code{NULL}.
#' @param VFolds Number of folds to use in cross-validation. Default is 10.
#' @param grid.size Grid size for \code{\link[hitandrun:simplex.sample]{simplex.sample()}} function to create possible combinations of coefficients
#' @param kappa For ODTR with resource constriants, kappa is the proportion of people in the population who are allowed to receive treatment. Default is \code{NULL}.
#' @param family either "gaussian" or "binomial". Default is null, if outcome is between 0 and 1 it will change to binomial, otherwise gaussian
#' @param contrast An integer to contrast Psi = E[Ydopt]-E[Ycontrast] for CV-TMLE. For example, 0 will contrast Psi = E[Ydopt]-E[Y0]. Default is \code{NULL}.
#' @param odtr.obj An object from the odtr function that estimates the odtr.
#'
#' @importFrom stats predict var qnorm
#' @import SuperLearner
#'
#' @return If the true Qbar function is specified, the output will be a vector of point estimates of E[Ydopt] and their respective confidence intervals. This will be for both the estimated optimal rule and the true optimal rule. Performance results on the optimal rule will also be output: proportion of people treated under ODTR, proportion of times the estimated rule matches the optimal rule, the mean outcome under the estimated optimal rule under the true mean outcome function, and the mean outcome under the estimated optimal rule under the sample-specific true mean outcome.
#'
#' If the true Qbar is not specified, return:
#' \describe{
#'   \item{EYdopt_estimates}{Point estimates and confidence intervals for E[Ydopt], using the unadjusted mean outcome for the people who received the optimal rule, g-computation, IPTW, IPTW-DR, TMLE}
#'   \item{SL.odtr}{SuperLearner list. See \code{SL.blip} or \code{SL.vote} documentation.}
#' }
#'
#' @references
#' van der Laan, Mark J., and Alexander R. Luedtke. "Targeted learning of the mean outcome under an optimal dynamic treatment rule." \emph{Journal of causal inference} 3.1 (2015): 61-95.
#'
#' Luedtke, Alexander R., and Mark J. van der Laan. "Super-learning of an optimal dynamic treatment rule." \emph{The international journal of biostatistics} 12.1 (2016): 305-332.
#'
#' Luedtke, Alexander R., and Mark J. van der Laan. "Optimal individualized treatments in resource-limited settings." \emph{The international journal of biostatistics} 12.1 (2016): 283-303.
#'
#' Coyle, J.R. (2017). Jeremy Coyle, “Computational Considerations for Targeted Learning” PhD diss., University of California, Berkeley 2017 \url{https://escholarship.org/uc/item/9kh0b9vm}.
#'
#' @export
#'
#' @examples
#' ## Example
#' library(SuperLearner)
#' library(hitandrun)
#' ObsData = subset(DGP_bin_simple(1000), select = -c(A_star, Y_star))
#' W = subset(ObsData, select = -c(A,Y))
#' V = W
#' A = ObsData$A
#' Y = ObsData$Y
#'
#' # E[Ydopt] using blip-based estimate of ODTR with risk function CV-TMLE
#' EYdopt(W = W, A = A, Y = Y, V = W, blip.SL.library = "SL.blip.HTEepi", g.SL.library = "SL.mean", QAW.SL.library = "SL.QAW.HTEepi", risk.type = "CV TMLE", metalearner = 'blip')



EYdopt = function(W, V, A, Y, g.SL.library, QAW.SL.library, blip.SL.library, dopt.SL.library = NULL,
                  metalearner, risk.type, rule.output,
                  grid.size = 100, VFolds = 10, kappa = NULL, QAW.fun = NULL,
                  family = NULL, contrast = NULL){

  n = length(Y)
  if (is.null(family)) { family = ifelse(max(Y) <= 1 & min(Y) >= 0, "binomial", "gaussian") }
  ab = range(Y)

  #### All things non-CV ####
  SL.odtr = odtr(V=V, W=W, A=A, Y=Y, ab = ab, g.SL.library = g.SL.library, QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library,
                   dopt.SL.library = dopt.SL.library, metalearner = metalearner,
                   risk.type=risk.type, grid.size=grid.size, VFolds=VFolds, QAW.fun = NULL, newV = NULL,
                   kappa = kappa, family = family, rule.output = "d")
  QAW.reg = SL.odtr$QAW.reg
  g.reg = SL.odtr$g.reg
  dopt = SL.odtr$dopt
  #EnYdn, non-CVTMLE
  EnYdn.nonCVTMLE = estimatorsEYd_nonCVTMLE(W = W, A = A, Y = Y, d = dopt, QAW.reg = QAW.reg, g.reg = g.reg, ab = ab, contrast = contrast)
  if (!is.null(QAW.fun)) {
    dopt0 = dopt.fun(blip = QAW.fun(A = 1, W = W) - QAW.fun(A = 0, W = W), kappa = kappa)
    #EnYd0, non-CVTMLE
    EnYd0.nonCVTMLE = estimatorsEYd_nonCVTMLE(W = W, A = A, Y = Y, d = dopt0, QAW.reg = QAW.reg, g.reg = g.reg, ab = ab, contrast = contrast)
    #E0Ydn, non-CVTMLE
    E0Ydn.nonCVTMLE = mean(QAW.fun(A = dopt, W = W))
    if (!is.null(contrast)) {
      contrastE0Ydn_fun = function(contrast_i) {
        contrast_i = contrast_i
        E0Ydn.nonCVTMLE_i = E0Ydn.nonCVTMLE - mean(QAW.fun(A = contrast_i, W = W))
        return(E0Ydn.nonCVTMLE_i)
      }
      contrastE0Ydn_df = apply(contrast, 2, contrastE0Ydn_fun)
      E0Ydn.nonCVTMLE = c(EYd = E0Ydn.nonCVTMLE, contrastE0Ydn_df)
    }
  }

  #### All things CV ####
  folds = sample(1:VFolds, size = n, replace = T)
  CV.TMLE_fun = function(i){

    SL.odtr.train = odtr(V = V[folds!=i,], W = W[folds!=i,], A = A[folds!=i], Y = Y[folds!=i], newV = V[folds==i,],
                         g.SL.library = g.SL.library, QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library, dopt.SL.library = dopt.SL.library,
                         metalearner = metalearner, risk.type=risk.type, grid.size=grid.size, VFolds=VFolds, QAW.fun = NULL,
                         kappa = kappa, family = family, ab = ab, rule.output = "d")
    g.reg.train = SL.odtr.train$g.reg
    QAW.reg.train = SL.odtr.train$QAW.reg
    dopt.test = SL.odtr.train$dopt
    g1W.test = predict(g.reg.train, newdata = W[folds == i,], type = "response")$pred
    gAW.test = ifelse(A[folds == i] == 1, g1W.test, 1 - g1W.test)
    Qdopt.test = predict(QAW.reg.train, newdata = data.frame(W[folds == i,], A = dopt.test), type = "response")$pred
    tmle_objects.EnYdn.test = tmle.d.fun(A = A[folds == i], Y = Y[folds==i], d = dopt.test, Qd = Qdopt.test, gAW = gAW.test, ab = ab)
    Psi_EnYdn.test = tmle_objects.EnYdn.test$psi
    varIC_EnYdn.test = var(tmle_objects.EnYdn.test$IC)
    toreturn = list(Psi_EnYdn.test = c(EYd = Psi_EnYdn.test), varIC_EnYdn.test = c(EYd = varIC_EnYdn.test))

    if (!is.null(QAW.fun)) {
      E0Ydn.test = mean(QAW.fun(A = dopt.test, W = W[folds == i,]))
      Qdopt0.test = predict(QAW.reg.train, newdata = data.frame(W[folds == i,], A = dopt0[folds == i]), type = "response")$pred
      tmle_objects.EnYd0.test = tmle.d.fun(A = A[folds == i], Y = Y[folds==i], d = dopt0[folds == i], Qd = Qdopt0.test, gAW = gAW.test, ab = ab)
      Psi_EnYd0.test = tmle_objects.EnYd0.test$psi
      varIC_EnYd0.test = var(tmle_objects.EnYd0.test$IC)
      toreturn = list(Psi_EnYdn.test = c(EYd = Psi_EnYdn.test), varIC_EnYdn.test = c(EYd = varIC_EnYdn.test),
                      Psi_EnYd0.test = c(EYd = Psi_EnYd0.test), varIC_EnYd0.test = c(EYd = varIC_EnYd0.test),
                      E0Ydn.test = c(EYd = E0Ydn.test))
    }

    if (!is.null(contrast)) {
      contrast_fun = function(contrast_i) {
        contrast.test_i = contrast_i[folds == i]
        Qcontrast.test_i = predict(QAW.reg.train, newdata = data.frame(W[folds == i,], A = contrast.test_i), type = "response")$pred
        tmle_objects.contrast.test_i = tmle.d.fun(A = A[folds == i], Y = Y[folds==i], d = contrast.test_i, Qd = Qcontrast.test_i, gAW = gAW.test, ab = ab)
        Psi_EnYdn.test_i = tmle_objects.EnYdn.test$psi - tmle_objects.contrast.test_i$psi
        varIC_EnYdn.test_i = var(tmle_objects.EnYdn.test$IC - tmle_objects.contrast.test_i$IC)
        toreturn_contrast = c(Psi_EnYdn.test_i = Psi_EnYdn.test_i, varIC_EnYdn.test_i = varIC_EnYdn.test_i)
        if (!is.null(QAW.fun)) {
          E0Ydn.test_i = mean(QAW.fun(A = dopt.test, W = W[folds == i,]) - QAW.fun(A = contrast.test_i, W = W[folds == i,]))
          Psi_EnYd0.test_i = tmle_objects.EnYd0.test$psi - tmle_objects.contrast.test_i$psi
          varIC_EnYd0.test_i = var(tmle_objects.EnYd0.test$IC - tmle_objects.contrast.test_i$IC)
          toreturn_contrast = c(Psi_EnYdn.test = Psi_EnYdn.test_i, varIC_EnYdn.test = varIC_EnYdn.test_i,
                                Psi_EnYd0.test = Psi_EnYd0.test_i, varIC_EnYd0.test = varIC_EnYd0.test_i,
                                E0Ydn.test = E0Ydn.test_i)
        }
        return(toreturn_contrast)
      }
      contrast_df = apply(contrast, 2, contrast_fun)
      toreturn_contrast = lapply(1:length(toreturn), function(x) c(toreturn[[x]], contrast_df[x,]))
      names(toreturn_contrast) = names(toreturn)
      toreturn = toreturn_contrast
    }

    return(toreturn)

  }

  CV.TMLE.est = lapply(1:VFolds, CV.TMLE_fun)

  #EnYdn, CVTMLE
  Psi_CV.TMLE = colMeans(do.call('rbind', lapply(1:VFolds, function(i) CV.TMLE.est[[i]]$Psi_EnYdn.test)))
  var_CV.TMLE = colMeans(do.call('rbind', lapply(1:VFolds, function(i) CV.TMLE.est[[i]]$varIC_EnYdn.test)))/n
  CI_CV.TMLE = sapply(1:length(Psi_CV.TMLE), function(i) Psi_CV.TMLE[i] + c(-1,1)*qnorm(0.975)*sqrt(var_CV.TMLE[i]))
  rownames(CI_CV.TMLE) = c("CI_CV.TMLE1", "CI_CV.TMLE2")
  colnames(CI_CV.TMLE) = names(Psi_CV.TMLE)
  EnYdn.CVTMLE = rbind(Psi_CV.TMLE = Psi_CV.TMLE, CI_CV.TMLE = CI_CV.TMLE)

  if (!is.null(QAW.fun)) {
    #EnYd0, CVTMLE
    Psi_CV.TMLE0 = colMeans(do.call('rbind', lapply(1:VFolds, function(i) CV.TMLE.est[[i]]$Psi_EnYd0.test)))
    var_CV.TMLE0 = colMeans(do.call('rbind', lapply(1:VFolds, function(i) CV.TMLE.est[[i]]$varIC_EnYd0.test)))/n
    CI_CV.TMLE0 = sapply(1:length(Psi_CV.TMLE0), function(i) Psi_CV.TMLE0[i] + c(-1,1)*qnorm(0.975)*sqrt(var_CV.TMLE0[i]))
    rownames(CI_CV.TMLE0) = c("CI_CV.TMLE1", "CI_CV.TMLE2")
    colnames(CI_CV.TMLE0) = names(Psi_CV.TMLE0)
    EnYd0.CVTMLE = rbind(Psi_CV.TMLE = Psi_CV.TMLE0, CI_CV.TMLE = CI_CV.TMLE0)
    #E0Ydn, CVTMLE
    E0Ydn.CVTMLE = colMeans(do.call('rbind', lapply(1:VFolds, function(i) CV.TMLE.est[[i]]$E0Ydn.test)))

    toreturn = list(EnYdn = rbind(EnYdn.nonCVTMLE, EnYdn.CVTMLE),
                    EnYd0 = rbind(EnYd0.nonCVTMLE, EnYd0.CVTMLE),
                    E0Ydn = rbind(E0Ydn.nonCVTMLE, E0Ydn.CVTMLE))

  } else {
    toreturn = list(EYdopt_estimates = rbind(EnYdn.nonCVTMLE, EnYdn.CVTMLE),
                    SL.odtr = SL.odtr)
  }

  return(toreturn)

}


















# function that computes EYgstar - unadj, TMLE, IPTW, gcomp, CV-TMLE

#' @name EYgstar
#' @aliases EYgstar
#' @title Estimation of E[Ygstar]
#' @description Given a W, A, Y dataset, this function will compute the estimated ODTR using SuperLearner. If a Qbar function is provided that computes the true E[Y|A,W] (e.g., if simulating), the function will also return the true treatment under the optimal rule and other metrics of evaluating the estimated optimal rule's performance. Then, it will estimate E[Ygstar] using g-computation, IPTW, IPTW-DR, TMLE, and CV-TMLE. Follows the framework of Luedtke and van der laan, 2015 and 2016.
#'
#' @param W Data frame of observed baseline covariates
#' @param V Data frame of observed baseline covariates (subset of W) used to design the ODTR
#' @param A Vector of treatment
#' @param Y Vector of outcome (continuous or binary)
#' @param metalearner Discrete ("discrete"), blip-based ("blip").
#' @param g.SL.library SuperLearner library for estimating txt mechanism
#' @param QAW.SL.library SuperLearner library for estimating outcome regression
#' @param blip.SL.library SuperLearner library for estimating the blip
#' @param risk.type Risk type in order to pick optimal combination of coefficients to combine the candidate algorithms. For (1) MSE risk use "CV MSE"; for (2) -E[Ygstar] risk use "CV IPCWDR" (for -E[Ygstar] estimated using double-robust IPTW) or "CV TMLE" (for -E[Ygstar] estimates using TMLE); (3) For the upper bound of the CI of -E[Ygstar] use "CV TMLE CI"
#' @param QAW.fun True outcome regression E[Y|A,W]. Useful for simulations. Default is \code{NULL}.
#' @param VFolds Number of folds to use in cross-validation. Default is 10.
#' @param grid.size Grid size for \code{\link[hitandrun:simplex.sample]{simplex.sample()}} function to create possible combinations of coefficients
#' @param kappa For ODTR with resource constriants, kappa is the proportion of people in the population who are allowed to receive treatment. Default is \code{NULL}.
#' @param family either "gaussian" or "binomial". Default is null, if outcome is between 0 and 1 it will change to binomial, otherwise gaussian
#' @param contrast An integer to contrast Psi = E[Ygstar]-E[Ycontrast] for CV-TMLE. For example, 0 will contrast Psi = E[Ygstar]-E[Y0]. Default is \code{NULL}.
#' @param odtr.obj An object from the odtr function that estimates the odtr.
#' @param cs_to_try Constants for SL.blip.c
#' @param alphas_to_try Convex combination alphas for SL.blip.alpha
#'
#' @importFrom stats predict var qnorm
#' @import SuperLearner
#'
#' @return If the true Qbar function is specified, the output will be a vector of point estimates of E[Ygstar] and their respective confidence intervals. This will be for both the estimated optimal rule and the true optimal rule. Performance results on the optimal rule will also be output: proportion of people treated under ODTR, proportion of times the estimated rule matches the optimal rule, the mean outcome under the estimated optimal rule under the true mean outcome function, and the mean outcome under the estimated optimal rule under the sample-specific true mean outcome.
#'
#' If the true Qbar is not specified, return:
#' \describe{
#'   \item{EYgstar_estimates}{Point estimates and confidence intervals for E[Ygstar], using the unadjusted mean outcome for the people who received the optimal rule, g-computation, IPTW, IPTW-DR, TMLE}
#'   \item{SL.odtr}{SuperLearner list. See \code{SL.blip} or \code{SL.vote} documentation.}
#' }
#'
#' @export
#'



EYgstar = function(W, V, A, Y, g.SL.library, QAW.SL.library, blip.SL.library,
                   metalearner, risk.type,
                   grid.size = 100, VFolds = 10, kappa = NULL, QAW.fun = NULL,
                   family = NULL, contrast = NULL, cs_to_try = NULL, alphas_to_try = NULL){

  n = length(Y)
  if (is.null(family)) { family = ifelse(max(Y) <= 1 & min(Y) >= 0, "binomial", "gaussian") }
  ab = range(Y)

  #### All things non-CV ####
  SL.odtr = odtr(V=V, W=W, A=A, Y=Y, ab = ab, g.SL.library = g.SL.library, QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library,
                 dopt.SL.library = NULL, metalearner = metalearner,
                 risk.type=risk.type, grid.size=grid.size, VFolds=VFolds, QAW.fun = NULL, newV = NULL,
                 kappa = NULL, family = family, rule.output = "g", cs_to_try, alphas_to_try)
  QAW.reg = SL.odtr$QAW.reg
  g.reg = SL.odtr$g.reg
  gstar1W = SL.odtr$gstar1W
  gstar0W = SL.odtr$gstar0W
  #EnYgstar, non-CVTMLE
  EnYgstar.nonCVTMLE = estimatorsEYgstar_nonCVTMLE(W = W, A = A, Y = Y, gstar1W = gstar1W, gstar0W = gstar0W, QAW.reg = QAW.reg, g.reg = g.reg, ab = ab, contrast = contrast)
  if (!is.null(QAW.fun)) {
    #E0Ydn, non-CVTMLE
    E0Ygstar.nonCVTMLE = mean(QAW.fun(A = 1, W = W)*gstar1W + QAW.fun(A = 0, W = W)*gstar0W)
    if (!is.null(contrast)) {
      contrastE0Ygstar_fun = function(contrast_i) {
        contrast_i = contrast_i
        E0Ygstar.nonCVTMLE_i = E0Ygstar.nonCVTMLE - mean(QAW.fun(A = contrast_i, W = W))
        return(E0Ygstar.nonCVTMLE_i)
      }
      contrastE0Ygstar_df = apply(contrast, 2, contrastE0Ygstar_fun)
      E0Ygstar.nonCVTMLE = c(EYgstar = E0Ygstar.nonCVTMLE, contrastE0Ygstar_df)
    }
  }

  #### All things CV ####
  folds = sample(1:VFolds, size = n, replace = T)
  CV.TMLE_fun = function(i){

    SL.odtr.train = odtr(V = V[folds!=i,], W = W[folds!=i,], A = A[folds!=i], Y = Y[folds!=i], newV = V[folds==i,],
                         g.SL.library = g.SL.library, QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library, dopt.SL.library = NULL,
                         metalearner = metalearner, risk.type=risk.type, grid.size=grid.size, VFolds=VFolds, QAW.fun = NULL,
                         kappa = NULL, family = family, ab = ab, rule.output = "g", cs_to_try = cs_to_try, alphas_to_try = alphas_to_try)
    g.reg.train = SL.odtr.train$g.reg
    QAW.reg.train = SL.odtr.train$QAW.reg
    gstar1W.test = SL.odtr.train$gstar1W
    gstar0W.test = SL.odtr.train$gstar0W
    gstarAW.test = ifelse(A[folds == i] == 1, gstar1W.test, gstar0W.test)
    g1W.test = predict(g.reg.train, newdata = W[folds == i,], type = "response")$pred
    gAW.test = ifelse(A[folds == i] == 1, g1W.test, 1 - g1W.test)
    Q1W.test = predict(QAW.reg.train, newdata = data.frame(W[folds == i,], A = 1), type = "response")$pred
    Q0W.test = predict(QAW.reg.train, newdata = data.frame(W[folds == i,], A = 0), type = "response")$pred
    QAW.test = predict(QAW.reg.train, newdata = data.frame(W[folds == i,], A = A[folds == i]), type = "response")$pred
    tmle_objects.EnYgstar.test = tmle.g.fun(A = A[folds == i], Y = Y[folds==i], gstarAW = gstarAW.test, gstar1W = gstar1W.test, gstar0W = gstar0W.test, QAW = QAW.test, Q1W = Q1W.test, Q0W = Q0W.test, gAW = gAW.test, ab = ab)
    Psi_EnYgstar.test = tmle_objects.EnYgstar.test$psi
    varIC_EnYgstar.test = var(tmle_objects.EnYgstar.test$IC)
    toreturn = list(Psi_EnYgstar.test = c(EYgstar = Psi_EnYgstar.test), varIC_EnYgstar.test = c(EYgstar = varIC_EnYgstar.test))

    if (!is.null(QAW.fun)) {
      E0Ygstar.test = mean(QAW.fun(A = 1, W = W[folds == i,])*gstar1W.test + QAW.fun(A = 0, W = W[folds == i,])*gstar0W.test)
      toreturn = list(Psi_EnYgstar.test = c(EYgstar = Psi_EnYgstar.test), varIC_EnYgstar.test = c(EYgstar = varIC_EnYgstar.test),
                      E0Ygstar.test = c(EYgstar = E0Ygstar.test))
    }

    if (!is.null(contrast)) {
      contrast_fun = function(contrast_i) {
        contrast.test_i = contrast_i[folds == i]
        Qcontrast.test_i = predict(QAW.reg.train, newdata = data.frame(W[folds == i,], A = contrast.test_i), type = "response")$pred
        tmle_objects.contrast.test_i = tmle.d.fun(A = A[folds == i], Y = Y[folds==i], d = contrast.test_i, Qd = Qcontrast.test_i, gAW = gAW.test, ab = ab)
        Psi_EnYgstar.test_i = tmle_objects.EnYgstar.test$psi - tmle_objects.contrast.test_i$psi
        varIC_EnYgstar.test_i = var(tmle_objects.EnYgstar.test$IC - tmle_objects.contrast.test_i$IC)
        toreturn_contrast = c(Psi_EnYgstar.test_i = Psi_EnYgstar.test_i, varIC_EnYgstar.test_i = varIC_EnYgstar.test_i)
        if (!is.null(QAW.fun)) {
          E0Ygstar.test_i = mean(QAW.fun(A = 1, W = W[folds == i,])*gstar1W.test + QAW.fun(A = 0, W = W[folds == i,])*gstar0W.test) - mean(QAW.fun(A = contrast.test_i, W = W[folds == i,]))
          toreturn_contrast = c(Psi_EnYgstar.test = Psi_EnYgstar.test_i, varIC_EnYgstar.test = varIC_EnYgstar.test_i,
                                E0Ygstar.test = E0Ygstar.test_i)
        }
        return(toreturn_contrast)
      }
      contrast_df = apply(contrast, 2, contrast_fun)
      toreturn_contrast = lapply(1:length(toreturn), function(x) c(toreturn[[x]], contrast_df[x,]))
      names(toreturn_contrast) = names(toreturn)
      toreturn = toreturn_contrast
    }

    return(toreturn)

  }

  CV.TMLE.est = lapply(1:VFolds, CV.TMLE_fun)

  #EnYgstar, CVTMLE
  Psi_CV.TMLE = colMeans(do.call('rbind', lapply(1:VFolds, function(i) CV.TMLE.est[[i]]$Psi_EnYgstar.test)))
  var_CV.TMLE = colMeans(do.call('rbind', lapply(1:VFolds, function(i) CV.TMLE.est[[i]]$varIC_EnYgstar.test)))/n
  CI_CV.TMLE = sapply(1:length(Psi_CV.TMLE), function(i) Psi_CV.TMLE[i] + c(-1,1)*qnorm(0.975)*sqrt(var_CV.TMLE[i]))
  rownames(CI_CV.TMLE) = c("CI_CV.TMLE1", "CI_CV.TMLE2")
  colnames(CI_CV.TMLE) = names(Psi_CV.TMLE)
  EnYgstar.CVTMLE = rbind(Psi_CV.TMLE = Psi_CV.TMLE, CI_CV.TMLE = CI_CV.TMLE)

  if (!is.null(QAW.fun)) {
    #E0Ygstar, CVTMLE
    E0Ygstar.CVTMLE = colMeans(do.call('rbind', lapply(1:VFolds, function(i) CV.TMLE.est[[i]]$E0Ygstar.test)))
    # regret
    d0 = as.numeric(QAW.fun(1,W) - QAW.fun(0,W) <= 0)
    regret = mean(QAW.fun(A = 1, W)*SL.odtr$gstar1W + QAW.fun(A = 0, W)*SL.odtr$gstar0W) - mean(QAW.fun(d0,W))

    toreturn = list(EnYgstar = rbind(EnYgstar.nonCVTMLE, EnYgstar.CVTMLE),
                    E0Ygstar = rbind(E0Ygstar.nonCVTMLE, E0Ygstar.CVTMLE),
                    SL.info = data.frame(regret = regret, param.type = SL.odtr$param.type, param = SL.odtr$param, coef = t(SL.odtr$SL.fit$coef)))

  } else {
    toreturn = list(EYdopt_estimates = rbind(EnYgstar.nonCVTMLE, EnYgstar.CVTMLE),
                    SL.odtr = SL.odtr)
  }

  return(toreturn)

}



