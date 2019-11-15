# function that computes EYdopt - unadj, TMLE, IPTW, gcomp, CV-TMLE

#' @name EYdopt
#' @aliases EYdopt
#' @title Estimation of E[Ydopt]
#' @description Given a W, A, Y dataset, this function will compute the estimated ODTR using SuperLearner. If a Qbar function is provided that computes the true E[Y|A,W] (e.g., if simulating), the function will also return the true treatment under the optimal rule and other metrics of evaluating the estimated optimal rule's performance. Then, it will estimate E[Ydopt] using g-computation, IPTW, IPTW-DR, TMLE, and CV-TMLE. Follows the framework of Luedtke and van der laan, 2015 and 2016.
#'
#' @param W Data frame of observed baseline covariates
#' @param W_for_g Data frame of observed baseline covariates used for predicting g1W (probability of treatment) via a GLM, e.g., if there's stratified randomization.
#' @param V Data frame of observed baseline covariates (subset of W) used to design the ODTR
#' @param A Vector of treatment
#' @param Y Vector of outcome (continuous or binary)
#' @param metalearner Discrete ("discrete"), blip-based ("blip"), vote-based SuperLearner ("vote"). Note that if metalearner is "vote" then cannot put in kappa.
#' @param QAW.SL.library SuperLearner library for estimating outcome regression
#' @param blip.SL.library SuperLearner library for estimating the blip
#' @param dopt.SL.library SuperLearner library for estimating dopt directly. Default is \code{NULL}.
#' @param QAW True outcome regression E[Y|A,W]. Useful for simulations. Default is \code{NULL}.
#' @param risk.type Risk type in order to pick optimal combination of coefficients to combine the candidate algorithms. For (1) MSE risk use "CV MSE"; for (2) -E[Ydopt] risk use "CV IPCWDR" (for -E[Ydopt] estimated using double-robust IPTW) or "CV TMLE" (for -E[Ydopt] estimates using TMLE); (3) For the upper bound of the CI of -E[Ydopt] use "CV TMLE CI"
#' @param moMain_model for DynTxRegime outcome regression
#' @param moCont_model for DynTxRegime contrast function
#' @param VFolds Number of folds to use in cross-validation. Default is 10.
#' @param grid.size Grid size for \code{\link[hitandrun:simplex.sample]{simplex.sample()}} function to create possible combinations of coefficients
#' @param kappa For ODTR with resource constriants, kappa is the proportion of people in the population who are allowed to receive treatment. Default is \code{NULL}.
#' @param g1W user-supplied vector of g1W
#' @param QAW True outcome regression E[Y|A,W]. Useful for simulations. Default is \code{NULL}.
#' @param family either "gaussian" or "binomial". Default is null, if outcome is between 0 and 1 it will change to binomial, otherwise gaussian
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
#' ObsData = subset(DGP_smooth2(1000), select = -c(A_star, Y_star))
#' W = subset(ObsData, select = -c(A,Y))
#' V = W
#' A = ObsData$A
#' Y = ObsData$Y
#'
#' # E[Ydopt] using blip-based estimate of ODTR with risk function CV-TMLE
#' EYdopt(W = W, A = A, Y = Y, V = W, blip.SL.library = "SL.blip.correct_smooth", QAW.SL.library = "SL.QAW.correct_smooth", risk.type = "CV TMLE", metalearner = 'blip')



EYdopt = function(W, W_for_g = 1, V, A, Y, metalearner,
                  QAW.SL.library, blip.SL.library, dopt.SL.library = NULL, risk.type,
                  moMain_model = NULL, moCont_model = NULL,
                  grid.size = 100, VFolds = 10, kappa = NULL, QAW = NULL, g1W = NULL,
                  family = NULL){

  n = length(Y)
  if (is.null(family)) { family = ifelse(max(Y) <= 1 & min(Y) >= 0, "binomial", "gaussian") }
  ab = range(Y)

  SL.odtr = odtr(V=V, W=W, A=A, Y=Y, ab = ab, QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library,
                 dopt.SL.library = dopt.SL.library, metalearner = metalearner,
                 risk.type=risk.type, grid.size=grid.size, VFolds=VFolds, QAW = NULL, newV = NULL,
                 moMain_model = moMain_model, moCont_model = moCont_model, W_for_g = W_for_g,
                 kappa = kappa, g1W = g1W, family = family)

  QAW.reg = SL.odtr$QAW.reg

  if (is.null(g1W)) {
    g.reg = SL.odtr$g.reg
    g1W = predict(g.reg, type = "response")
  }

  gAW = ifelse(A == 1, g1W, 1-g1W)

  dopt = SL.odtr$dopt

  toreturn_dopt = estimatorsEYdopt_nonCVTMLE(W = W, A = A, Y = Y, dopt = dopt,
                                             QAW.reg = QAW.reg, gAW = gAW,
                                             QAW.SL.library = QAW.SL.library, ab = ab)
  ### CV-TMLE ###
  folds = sample(1:VFolds, size = n, replace = T)
  CV.TMLE_fun = function(i){
    SL.odtr.train = odtr(V = V[folds!=i,], W = W[folds!=i,], A = A[folds!=i], Y = Y[folds!=i], newV = V[folds==i,],
                         QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library, dopt.SL.library = dopt.SL.library,
                         metalearner = metalearner, risk.type=risk.type, grid.size=grid.size, VFolds=VFolds, QAW = NULL,
                         moMain_model = moMain_model, moCont_model = moCont_model,
                         W_for_g = W_for_g, kappa = kappa, g1W = g1W[folds!=i], family = family, ab = ab)
    QAW.reg.train = SL.odtr.train$QAW.reg
    dopt.test = SL.odtr.train$SL.fit$SL.predict
    Qdopt.test = predict(QAW.reg.train, newdata = data.frame(W[folds == i,], A = dopt.test), type = "response")$pred
    gAW.test = gAW[folds==i]
    tmle_objects.dopt.test = tmle.fun(A = A[folds == i], Y = Y[folds==i],
                                      d = dopt.test, Qd = Qdopt.test,
                                      gAW = gAW.test, ab = ab)
    Psi_TMLE.test = tmle_objects.dopt.test$psi
    var_IC.test = var(tmle_objects.dopt.test$IC)
    if (is.null(QAW)) {
      toreturn = list(Psi_TMLE.test = Psi_TMLE.test, var_IC.test = var_IC.test)
    } else {
      E0Ydn_test = mean(QAW(A = dopt.test, W = W[folds == i,]))
      toreturn = list(Psi_TMLE.test = Psi_TMLE.test, var_IC.test = var_IC.test, E0Ydn_test = E0Ydn_test)
    }
    return(toreturn)
  }
  CV.TMLE.est = lapply(1:VFolds, CV.TMLE_fun)
  Psi_CV.TMLE = mean(sapply(1:VFolds, function(i) CV.TMLE.est[[i]]$Psi_TMLE.test))
  var_CV.TMLE = mean(sapply(1:VFolds, function(i) CV.TMLE.est[[i]]$var_IC.test))/n
  CI_CV.TMLE = Psi_CV.TMLE + c(-1,1)*qnorm(0.975)*sqrt(var_CV.TMLE)

  toreturn_dopt = c(toreturn_dopt, Psi_CV.TMLE = Psi_CV.TMLE, CI_CV.TMLE = CI_CV.TMLE)

  if (!is.null(QAW)) {

    dopt0 = dopt.fun(blip = QAW(A = 1, W = W) - QAW(A = 0, W = W), kappa = kappa)
    toreturn_dopt0 = estimatorsEYdopt_nonCVTMLE(W = W, A = A, Y = Y, dopt = dopt0,
                                                QAW.reg = QAW.reg, gAW = gAW,
                                                QAW.SL.library = QAW.SL.library, ab = ab)
    ### CV-TMLE ###
    folds = sample(1:VFolds, size = n, replace = T)
    CV.TMLE_fun = function(i){
      QAW.reg.train = SuperLearner(Y = Y[folds!=i],
                                   X = data.frame(A = A[folds!=i], W[folds!=i,]),
                                   SL.library = QAW.SL.library, family = family)
      dopt.test = as.numeric((QAW(A = 1, W = W[folds==i,]) - QAW(A = 0, W = W[folds==i,])) > 0)
      Qdopt.test = predict(QAW.reg.train, newdata = data.frame(W[folds == i,], A = dopt.test), type = "response")$pred
      g1W.test = suppressWarnings(predict(g.reg, newdata = data.frame(A,W)[folds==i,], type = "response"))
      gAW.test = ifelse(A[folds==i] == 1, g1W.test, 1-g1W.test)
      tmle_objects.dopt.test = tmle.fun(A = A[folds == i], Y = Y[folds==i],
                                        d = dopt.test, Qd = Qdopt.test,
                                        gAW = gAW.test, ab = ab)
      Psi_TMLE.test = tmle_objects.dopt.test$psi
      var_IC.test = var(tmle_objects.dopt.test$IC)
      return(list(Psi_TMLE.test = Psi_TMLE.test, var_IC.test = var_IC.test))
    }
    CV.TMLE.est0 = lapply(1:VFolds, CV.TMLE_fun)
    Psi_CV.TMLE0 = mean(sapply(1:VFolds, function(i) CV.TMLE.est0[[i]]$Psi_TMLE.test))
    var_CV.TMLE0 = mean(sapply(1:VFolds, function(i) CV.TMLE.est0[[i]]$var_IC.test))/n
    CI_CV.TMLE0 = Psi_CV.TMLE0 + c(-1,1)*qnorm(0.975)*sqrt(var_CV.TMLE0)

    toreturn_dopt0 = c(toreturn_dopt0, Psi_CV.TMLE = Psi_CV.TMLE0, CI_CV.TMLE = CI_CV.TMLE0)
    names(toreturn_dopt0) = paste0(names(toreturn_dopt0), "_dopt0")

    EY0dn = mean(QAW(A = dopt, W = W))
    toreturn = c(toreturn_dopt,
                 toreturn_dopt0,
                 #mean_dopt = mean(dopt),
                 #match_dopt = mean(dopt == dopt0),
                 EY0dn = EY0dn,
                 EY0dn_CVTMLE = mean(sapply(1:VFolds, function(i) CV.TMLE.est[[i]]$E0Ydn_test)))

  } else {

    Qdopt = predict(QAW.reg, newdata = data.frame(W, A = dopt), type = "response")$pred
    Qdopt.star = tmle.fun(A = A, d = dopt, Y = Y, Qd = Qdopt, gAW = gAW, ab = ab)$Qd.star

    toreturn = list(EYdopt_estimates = toreturn_dopt,
                    Qdopt.star = Qdopt.star,
                    SL.odtr = SL.odtr)

  }

  return(toreturn)

}

