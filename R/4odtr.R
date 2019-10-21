# function to compute ODTR
#' @name odtr
#' @aliases odtr
#' @title Function that computes ODTR
#' @description Given a W, A, Y dataset, this function will compute the estimated ODTR using SuperLearner. If a QAW function is provided that computes the true E[Y|A,W] (e.g., if simulating), the function will also return the true treatment under the optimal rule and other metrics of evaluating the estimated optimal rule's performance.
#'
#' @param W Data frame of observed baseline covariates
#' @param W_for_g Data frame of observed baseline covariates used for predicting g1W (probability of treatment). Could also be 1 if using mean of A as estimate of g1W.
#' @param A Vector of treatment
#' @param Y Vector of treatment (continuous or binary)
#' @param V Data frame of observed baseline covariates (subset of W) used to design the ODTR
#' @param blip.SL.library SuperLearner library for estimating blip
#' @param QAW.SL.library SuperLearner library for estimating the outcome regression
#' @param risk.type Risk type in order to pick optimal combination of coefficients to combine the candidate algorithms. For (1) MSE risk use "CV MSE" and for (2) E[Ydopt] risk use "CV IPCWDR" (for E[Ydopt] estimated using double-robust IPTW) or "CV TMLE" (for E[Ydopt] estimates using TMLE)
#' @param grid.size Grid size for \code{\link[hitandrun:simplex.sample]{simplex.sample()}} function to create possible combinations of coefficients
#' @param SL.type Blip-based ("blip") or vote-based SuperLearner ("vote"). Note that if SL.type is "vote" then cannot put in kappa.
#' @param kappa For ODTR with resource constriants, kappa is the proportion of people in the population who are allowed to receive treatment. Default is \code{NULL}.
#' @param QAW True outcome regression E[Y|A,W]. Useful for simulations. Default is \code{NULL}.
#' @param VFolds Number of folds to use in cross-validation. Default is 10.
#' @param moMain_model for DynTxRegime modeling outcome regression
#' @param moCont_model for DynTxRegime modeling contrast
#' @param g1W user-supplied vector of g1W
#' @param family either "gaussian" or "binomial". Default is null, if outcome is between 0 and 1 it will change to binomial, otherwise gaussian
#'
#' @return
#'
#' @export
#'
#' @references
#' Luedtke, Alexander R., and Mark J. van der Laan. "Super-learning of an optimal dynamic treatment rule." \emph{The international journal of biostatistics} 12.1 (2016): 305-332.
#' Coyle, J.R. (2017). Jeremy Coyle, “Computational Considerations for Targeted Learning” PhD diss., University of California, Berkeley 2017 \url{https://escholarship.org/uc/item/9kh0b9vm}.
#' Eric Polley, Erin LeDell, Chris Kennedy and Mark van der Laan (2018). SuperLearner: Super Learner Prediction. R package version 2.0-24. \url{https://CRAN.R-project.org/package=SuperLearner}.
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
#' # blip-based estimate of ODTR with risk function CV-TMLE
#' odtr(W = W, W_for_g = subset(W, select = c(W1, W2)), A = A, Y = Y, V = W, blip.SL.library = "SL.blip.correct_smooth", QAW.SL.library = "SL.QAW.correct_smooth", risk.type = "CV TMLE", SL.type = 'blip')
#'
#'
odtr = function(W, W_for_g = 1, A, Y, ab = NULL, V, newV = NULL, blip.SL.library, dopt.SL.library = NULL,
                QAW.SL.library, risk.type, grid.size = 100,
                SL.type, kappa = NULL, QAW = NULL, VFolds = 10,
                moMain_model = NULL, moCont_model = NULL, g1W = NULL, family = NULL){

  n = length(A)
  if (is.null(family)) { family = ifelse(max(Y) <= 1 & min(Y) >= 0, "binomial", "gaussian") }
  if (is.null(ab)) { ab = range(Y) }

  # E[Y|A,W]
  QAW.reg = SuperLearner(Y = Y, X = data.frame(A, W), SL.library = QAW.SL.library, family = family)

  # estimate pred. prob. observed exposure, P(A|W)=g(A|W)
  if (is.null(g1W)) {
    g.reg = glm(A ~ ., data = data.frame(A,W_for_g), family = "binomial")
    g1W = predict(g.reg, type = "response")
  } else {
    g.reg = NULL
  }
  gAW = ifelse(A == 1, g1W, 1-g1W)

  if (SL.type == "vote") {
    # get estimate of txt under rule based on risk type (IPCWDR, TMLE, CV TMLE, CV IPCWDR)
    SL.fit = SL.vote(V = V, W = W, W_for_g, A = A, Y = Y, ab = ab, QAW.reg = QAW.reg,
                     blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library,
                     gAW = gAW, risk.type = risk.type, grid.size = grid.size,
                     VFolds = VFolds, moMain_model = moMain_model, moCont_model = moCont_model, newV = newV, family = family)
    # get estimate of txt under optimal rule
    dopt = SL.fit$SL.predict
  } else if (SL.type == "blip") {
    # get estimate of blip based on risk type (IPCWDR, TMLE, CV TMLE, CV IPCWDR, MSE)
    SL.fit = SL.blip(V = V, W = W, A = A, Y = Y, ab = ab, QAW.reg = QAW.reg,
                     blip.SL.library = blip.SL.library,
                     gAW = gAW, risk.type = risk.type,
                     grid.size = grid.size, VFolds = VFolds, newV = newV, family = family)
    # get estimate of optimal rule based on blip estimate
    blip = SL.fit$SL.predict
    # get dopt
    dopt = dopt.fun(blip = blip, kappa = kappa)
  }

  if (!is.null(QAW)) {

    # True mean under estimated optimal rule using true QAW
    EYdn_QAWHat = mean(QAW(A = dopt, W = W))

    # True optimal rule
    true_dopt = dopt.fun(blip = QAW(A = 1, W = W) - QAW(A = 0, W = W), kappa = kappa)

    # Does estimated optimal rule using estimated QAW match true optimal rule?
    match_dopt_QAWHat = mean(true_dopt == dopt)

    toreturn = c(EYdn_QAWHat = EYdn_QAWHat,
                 match_dopt_QAWHat = match_dopt_QAWHat,
                mean_dopt = mean(dopt),
                mean_dopt0 = mean(true_dopt),
                coef = SL.fit$coef)
  } else {
    toreturn = list(dopt = dopt,
                    QAW.reg = QAW.reg,
                    g.reg = g.reg,
                    g1W = g1W,
                    SL.fit = SL.fit)
  }
  return(toreturn)
}









# function to predict ODTR
#' @name predict_odtr
#' @aliases predict_odtr
#' @title Predict ODTR
#' @description Function that, given an odtr object, predicts the treatment under the ODTR for a new V.
#'
#' @param odtr_obj ODTR object from odtr function
#' @param newV new covariates to predict from
#'
#' @return
#'
#' @export
#'
#' @references
#' Luedtke, Alexander R., and Mark J. van der Laan. "Super-learning of an optimal dynamic treatment rule." \emph{The international journal of biostatistics} 12.1 (2016): 305-332.
#' Coyle, J.R. (2017). Jeremy Coyle, “Computational Considerations for Targeted Learning” PhD diss., University of California, Berkeley 2017 \url{https://escholarship.org/uc/item/9kh0b9vm}.
#' Eric Polley, Erin LeDell, Chris Kennedy and Mark van der Laan (2018). SuperLearner: Super Learner Prediction. R package version 2.0-24. \url{https://CRAN.R-project.org/package=SuperLearner}.
#'
#'
predict_odtr = function(odtr_obj, newV) {

  # predict on new data
  pred_blip = matrix(NA, nrow = nrow(newV), ncol = length(odtr_obj$SL.fit$libraryNames))
  colnames(pred_blip) = c(odtr_obj$SL.fit$libraryNames)
  SL.fit = odtr_obj$SL.fit

  for (mm in seq(length(odtr_obj$SL.fit$libraryNames))) {
    pred_blip[, mm] = do.call('predict', list(object = SL.fit$fitBlipLibrary[[mm]],
                                              newdata = newV,
                                              family = SL.fit$blipFamily))
  }

  SL.out = list()
  SL.out$libraryBlipPredict = pred_blip
  SL.out$SL.predict = SL.out$libraryBlipPredict%*%odtr_obj$SL.fit$coef
  SL.out$SL.dopt.predict = as.numeric(SL.out$SL.predict > 0)

  return(SL.out)

}
