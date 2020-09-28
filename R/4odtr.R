# function to compute ODTR
#' @name odtr
#' @aliases odtr
#' @title Function that computes ODTR
#' @description Given a W, A, Y dataset, this function will compute the estimated ODTR using SuperLearner. If a QAW function is provided that computes the true E[Y|A,W] (e.g., if simulating), the function will also return the true treatment under the optimal rule and other metrics of evaluating the estimated optimal rule's performance.
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#' @param Y Vector of treatment (continuous or binary)
#' @param V Data frame of observed baseline covariates (subset of W) used to design the ODTR
#' @param rule.output Either "d" for deterministic ODTR or "g" for stochastic ODTR. Default is "d."
#' @param g.SL.library Character vector for logistic regression modeling the treatment mechanism. Default is 1 (i.e., using mean of A as estimate of g1W).
#' @param QAW.SL.library SuperLearner library for estimating the outcome regression
#' @param blip.SL.library SuperLearner library for estimating blip
#' @param dopt.SL.library SuperLearner library for estimating dopt directly. Default is \code{NULL}. Could be "DonV", "Qlearn", "OWL", "EARL", "optclass", "RWL", "treatall", "treatnone". Could also be "all" for all algorithms.
#' @param risk.type Risk type in order to pick optimal combination of coefficients to combine the candidate algorithms. For (1) MSE risk use "CV MSE"; for (2) -E[Ydopt] risk use "CV IPCWDR" (for -E[Ydopt] estimated using double-robust IPTW) or "CV TMLE" (for -E[Ydopt] estimates using TMLE); (3) For the upper bound of the CI of -E[Ydopt] use "CV TMLE CI"
#' @param grid.size Grid size for \code{\link[hitandrun:simplex.sample]{simplex.sample()}} function to create possible combinations of coefficients
#' @param metalearner Discrete ("discrete"), blip-based ("blip"), vote-based SuperLearner ("vote"). Note that if metalearner is "vote" then cannot put in kappa.
#' @param kappa For ODTR with resource constriants, kappa is the proportion of people in the population who are allowed to receive treatment. Default is \code{NULL}.
#' @param QAW.fun True outcome regression E[Y|A,W]. Useful for simulations. Default is \code{NULL}.
#' @param VFolds Number of folds to use in cross-validation. Default is 10.
#' @param family Either "gaussian" or "binomial". Default is null, if outcome is between 0 and 1 it will change to binomial, otherwise gaussian
#' @param ab Range of Y
#' @param newV New V for prediction
#' @param cs_to_try Constants for SL.blip.c
#' @param alphas_to_try Convex combination alphas for SL.blip.alpha
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
#' ObsData = subset(DGP_bin_simple(1000), select = -c(A_star, Y_star))
#' W = subset(ObsData, select = -c(A,Y))
#' V = W
#' A = ObsData$A
#' Y = ObsData$Y
#'
#' # blip-based estimate of ODTR with risk function CV-TMLE
#' odtr(W = W, gform = "W1 + W2", A = A, Y = Y, V = W, blip.SL.library = "SL.blip.HTEepi", QAW.SL.library = "SL.QAW.HTEepi", risk.type = "CV TMLE", metalearner = 'blip')
#'
#'
odtr = function(W, A, Y, V, rule.output = "d", g.SL.library, QAW.SL.library, blip.SL.library, dopt.SL.library = NULL, risk.type, metalearner,
                kappa = NULL, newV = NULL, QAW.fun = NULL, VFolds = 10, grid.size = 100, family = NULL, ab = NULL,
                cs_to_try = NULL, alphas_to_try = NULL){

  n = length(A)
  if (is.null(family)) { family = ifelse(max(Y) <= 1 & min(Y) >= 0, "binomial", "gaussian") }
  if (is.null(ab)) { ab = range(Y) }
  if (is.null(newV)) { newV = W }

  # E[Y|A,W]
  QAW.reg = SuperLearner(Y = Y, X = data.frame(A, W), SL.library = QAW.SL.library, family = family)
  # P(A=1|W)=g(1|W)
  g.reg = SuperLearner(Y = A, X = W, SL.library = g.SL.library, family = family)

  if (metalearner == "discrete") {
    discrete.SL = T
  } else {
    discrete.SL = F
  }

  if (rule.output == "d") {
    if (discrete.SL) {
      SL.type = "blip"
    } else {
      SL.type = metalearner
    }

    if (SL.type == "vote") {
      # get estimate of txt under rule based on risk type (CV TMLE, CV TMLE CI)
      SL.fit = SL.vote(V = V, W = W, A = A, Y = Y, ab = ab, QAW.reg = QAW.reg, g.reg = g.reg,
                       blip.SL.library = blip.SL.library, dopt.SL.library = dopt.SL.library,
                       risk.type = risk.type, grid.size = grid.size, VFolds = VFolds,
                       newV = newV, family = family, discrete.SL = discrete.SL)
      # get estimate of txt under optimal rule
      dopt = SL.fit$SL.predict
    } else if (SL.type == "blip") {
      # get estimate of blip based on risk type (CV TMLE, MSE, CV TMLE CI)
      SL.fit = SL.blip(V = V, W = W, A = A, Y = Y, ab = ab, QAW.reg = QAW.reg, g.reg = g.reg,
                       blip.SL.library = blip.SL.library, risk.type = risk.type,
                       grid.size = grid.size, VFolds = VFolds, newV = newV, family = family,
                       discrete.SL = discrete.SL)
      # get estimate of optimal rule based on blip estimate
      blip = SL.fit$SL.predict
      # get dopt
      dopt = dopt.fun(blip = blip, kappa = kappa)
    }


    if (!is.null(QAW.fun)) {

      # True mean under estimated optimal rule using true QAW
      EYdn_QAWHat = mean(QAW.fun(A = dopt, W = W))

      # True optimal rule
      true_dopt = dopt.fun(blip = QAW.fun(A = 1, W = W) - QAW.fun(A = 0, W = W), kappa = kappa)

      # Does estimated optimal rule using estimated QAW match true optimal rule?
      match_dopt_QAWHat = mean(true_dopt == dopt)

      if(risk.type %in% c("CV TMLE", "CV TMLE CI")) {
        CV.risk_minCI = as.numeric(SL.fit$CV.risk_min$CI)
      } else {
        CV.risk_minCI = NA
      }

      toreturn = c(EYdn_QAWHat = EYdn_QAWHat,
                   match_dopt_QAWHat = match_dopt_QAWHat,
                   mean_dopt = mean(dopt),
                   mean_dopt0 = mean(true_dopt),
                   coef = SL.fit$coef,
                   CV.risk_min = SL.fit$CV.risk_min$est,
                   CV.risk_minCI = CV.risk_minCI)
    } else {
      toreturn = list(dopt = dopt,
                      QAW.reg = QAW.reg,
                      g.reg = g.reg,
                      SL.fit = SL.fit,
                      rule.output = "d")
    }





  } else if (rule.output == "g") {





    if (rule.output == "g" & metalearner == "vote") { stop("Can't have both vote metalearner and stochastic rule")}
    if (!is.null(cs_to_try) & is.null(alphas_to_try)) {
      SL.type = "c"
    } else if (is.null(cs_to_try) & !is.null(alphas_to_try)) {
      SL.type = "alpha"
    } else {
      stop("Can only have c or alpha for rule.output = g. Right now you have specified both or neither.")
    }

    if (SL.type == "c") {
      SL.fit = SL.blip.c(V = V, W = W, A = A, Y = Y, ab = ab, QAW.reg = QAW.reg, g.reg = g.reg,
                         blip.SL.library = blip.SL.library, risk.type = risk.type,
                         grid.size = grid.size, VFolds = VFolds, newV = newV, family = family,
                         discrete.SL = discrete.SL, cs_to_try = cs_to_try)
      # get estimate of optimal rule based on blip estimate
      blip = SL.fit$SL.predict
      c = param = SL.fit$c
      gstar1W = 1-plogis(blip/c)
      gstar0W = plogis(blip/c)

    } else if (SL.type == "alpha") {
      SL.fit = SL.blip.alpha(V = V, W = W, A = A, Y = Y, ab = ab, QAW.reg = QAW.reg, g.reg = g.reg,
                             blip.SL.library = blip.SL.library, risk.type = risk.type,
                             grid.size = grid.size, VFolds = VFolds, newV = newV, family = family,
                             discrete.SL = discrete.SL, alphas_to_try = alphas_to_try)
      # get estimate of optimal rule based on blip estimate
      blip = SL.fit$SL.predict
      dopt = as.numeric(blip <= 0)
      alpha = param = SL.fit$alpha
      g1W.pred = predict(g.reg, data.frame(newV), type = "response")$pred
      g0W.pred = 1 - g1W.pred
      Q1W.pred = predict(QAW.reg, newdata = data.frame(newV, A = 1), type = "response")$pred
      Q0W.pred = predict(QAW.reg, newdata = data.frame(newV, A = 0), type = "response")$pred
      var1W.pred = Q1W.pred*(1-Q1W.pred)
      var0W.pred = Q0W.pred*(1-Q0W.pred)
      num1 = g1W.pred/var1W.pred
      num0 = g0W.pred/var0W.pred
      denom = g1W.pred/var1W.pred + g0W.pred/var0W.pred
      gstarv1W = num1/denom
      gstarv0W = num0/denom
      gdn_1W = dopt
      gdn_0W = 1-gdn_1W
      gstar1W = (1-alpha)*gdn_1W + alpha*as.numeric(gstarv1W)
      gstar0W = (1-alpha)*gdn_0W + alpha*as.numeric(gstarv0W)
    }



    if (!is.null(QAW.fun)) {

      # True mean under estimated optimal rule using true QAW
      EYdn_QAWHat = mean(QAW.fun(1,W)*gstar1W + QAW.fun(0,W)*gstar0W)

      # True optimal rule
      true_dopt = as.numeric((blip = QAW.fun(A = 1, W = W) - QAW.fun(A = 0, W = W)<= 0))

      toreturn = c(EYdn_QAWHat = EYdn_QAWHat,
                   coef = SL.fit$coef,
                   param = ifelse(!is.null(alphas_to_try), alpha, c))
    } else {
      toreturn = list(gstar1W = as.numeric(gstar1W),
                      gstar0W = as.numeric(gstar0W),
                      QAW.reg = QAW.reg,
                      g.reg = g.reg,
                      SL.fit = SL.fit,
                      rule.output = "g",
                      param.type = SL.type,
                      param = param)
    }


  } else {
    stop("Must specify if deterministic or stochastic rule")
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
  if (odtr_obj$rule.output == "d") {
    SL.out$SL.dopt.predict = as.numeric(SL.out$SL.predict > 0)
  } else {
    SL.out$param.type = odtr_obj$param.type
    SL.out$param = odtr_obj$param
    if (odtr_obj$param.type == "alpha") {
      dopt = as.numeric(SL.out$SL.predict <= 0)
      alpha = odtr_obj$param
      g1W.pred = predict(odtr_obj$g.reg, data.frame(newV), type = "response")$pred
      g0W.pred = 1 - g1W.pred
      Q1W.pred = predict(odtr_obj$QAW.reg, newdata = data.frame(newV, A = 1), type = "response")$pred
      Q0W.pred = predict(odtr_obj$QAW.reg, newdata = data.frame(newV, A = 0), type = "response")$pred
      var1W.pred = Q1W.pred*(1-Q1W.pred)
      var0W.pred = Q0W.pred*(1-Q0W.pred)
      num1 = g1W.pred/var1W.pred
      num0 = g0W.pred/var0W.pred
      denom = g1W.pred/var1W.pred + g0W.pred/var0W.pred
      gstarv1W = num1/denom
      gstarv0W = num0/denom
      gdn_1W = dopt
      gdn_0W = 1-gdn_1W
      SL.out$SL.gstar1W.predict = (1-alpha)*gdn_1W + alpha*as.numeric(gstarv1W)
      SL.out$SL.gstar0W.predict = (1-alpha)*gdn_0W + alpha*as.numeric(gstarv0W)
    } else if (odtr_obj$param.type == "c") {
      blip = SL.out$SL.predict
      c = SL.out$param
      SL.out$SL.gstar1W.predict = 1-plogis(blip/c)
      SL.out$SL.gstar0W.predict = plogis(blip/c)
    }
  }


  return(SL.out)

}
