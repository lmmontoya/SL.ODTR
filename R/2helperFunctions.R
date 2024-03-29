#' @name getpreds.blip.fun
#' @aliases getpreds.blip.fun
#' @title Predictions
#' @description Get library predictions (what would be library.predict) for blip. Code mostly from SuperLearner.
#'
#' @param Y outcome
#' @param X predictors
#' @param newX new X
#' @param family family
#' @param SL.library SL library
#' @param id for unique ids
#' @param verbose verbose
#' @param control control
#' @param obsWeights obsWeights
#' @param env env
#'
#' @return predicted blips
#'
#' @export
#'
# Helper Functions

# get what would've been library.predict
# code mostly from SuperLearner

getpreds.blip.fun <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                         id = NULL, verbose = FALSE, control = list(),
                         obsWeights = NULL, env = parent.frame()) {

  # # get defaults for controls and make sure in correct format
  control <- do.call('SuperLearner.control', control)

  .createLibrary <- function(SL.library) {
    if (is.character(SL.library)) {
      k <- length(SL.library)
      whichScreen <- matrix(1, nrow = 1, ncol = k)
      screenAlgorithm <- "All"
      library <- data.frame(predAlgorithm = SL.library, rowScreen = 1, stringsAsFactors=FALSE)
    } else if (is.list(SL.library)) {
      predNames <- sapply(SL.library, FUN = "[", 1)
      NumberScreen <- (sapply(SL.library, FUN = length) - 1)
      if (sum(NumberScreen == 0) > 0) {
        for(ii in which(NumberScreen == 0)) {
          SL.library[[ii]] <- c(SL.library[[ii]], "All")
          NumberScreen[ii] <- 1
        }
      }
      screenAlgorithmFull <- unlist(lapply(SL.library, FUN="[", -1))
      screenAlgorithm <- unique(screenAlgorithmFull)

      library <- data.frame(predAlgorithm = rep(predNames, times=NumberScreen), rowScreen = match(screenAlgorithmFull, screenAlgorithm), stringsAsFactors = FALSE)
    } else {
      stop('format for SL.library is not recognized')
    }

    out <- list(library = library, screenAlgorithm = screenAlgorithm)
    return(out)
  }

  # put together the library
  # should this be in a new environment?
  library <- .createLibrary(SL.library)
  #.check.SL.library(library = c(unique(library$library$predAlgorithm), library$screenAlgorithm))

  call <- match.call(expand.dots = TRUE)
  # should we be checking X and newX for data.frame?

  varNames <- colnames(X)
  N <- dim(X)[1L]
  p <- dim(X)[2L]
  k <- nrow(library$library)
  kScreen <- length(library$screenAlgorithm)
  libraryNames <- paste(library$library$predAlgorithm, library$screenAlgorithm[library$library$rowScreen], sep="_")

  # put fitLibrary in it's own environment to locate later
  fitLibEnv <- new.env()
  assign('fitLibrary', vector('list', length = k), envir = fitLibEnv)
  assign('libraryNames', libraryNames, envir = fitLibEnv)
  evalq(names(fitLibrary) <- libraryNames, envir = fitLibEnv)

  # if newX is missing, use X
  if(is.null(newX)) {
    newX <- X
  }
  # Are these checks still required?
  if(!identical(colnames(X), colnames(newX))) {
    stop("The variable names and order in newX must be identical to the variable names and order in X")
  }
  if (sum(is.na(X)) > 0 | sum(is.na(newX)) > 0 | sum(is.na(Y)) > 0) {
    stop("missing data is currently not supported. Check Y, X, and newX for missing values")
  }
  if (!is.numeric(Y)) {
    stop("the outcome Y must be a numeric vector")
  }
  # family can be either character or function, so these lines put everything together (code from glm())
  if(is.character(family))
    family <- get(family, mode="function", envir=parent.frame())
  if(is.function(family))
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }

  # test id
  if(is.null(id)) {
    id <- seq(N)
  }
  if(!identical(length(id), N)) {
    stop("id vector must have the same dimension as Y")
  }
  # test observation weights
  if(is.null(obsWeights)) {
    obsWeights <- rep(1, N)
  }
  if(!identical(length(obsWeights), N)) {
    stop("obsWeights vector must have the same dimension as Y")
  }


  # now fit all algorithms in library on entire learning data set and predict on newX
  m <- dim(newX)[1L]
  predY <- matrix(NA, nrow = m, ncol = k)
  # whichScreen <- matrix(NA, nrow = kScreen, ncol = p)

  .screenFun <- function(fun, list) {
    screen_fn = get(fun, envir = env)
    testScreen <- try(do.call(screen_fn, list))
    if (inherits(testScreen, "try-error")) {
      warning(paste("replacing failed screening algorithm,", fun, ", with All() in full data", "\n "))
      out <- rep(TRUE, ncol(list$X))
    } else {
      out <- testScreen
    }
    return(out)
  }

  whichScreen <- sapply(library$screenAlgorithm, FUN = .screenFun, list = list(Y = Y, X = X, family = family, id = id, obsWeights = obsWeights), simplify = FALSE)
  whichScreen <- do.call(rbind, whichScreen)

  .predFun <- function(index, lib, Y, dataX, newX, whichScreen, family, id, obsWeights, verbose, control, libraryNames) {
    pred_fn = get(lib$predAlgorithm[index], envir = env)
    testAlg <- try(do.call(pred_fn, list(Y = Y,
                                         X = subset(dataX,
                                                    select = whichScreen[lib$rowScreen[index], ], drop=FALSE),
                                         newX = subset(newX, select = whichScreen[lib$rowScreen[index], ], drop=FALSE),
                                         family = family, id = id, obsWeights = obsWeights)))
    # testAlg <- try(do.call(lib$predAlgorithm[index], list(Y = Y, X = dataX[, whichScreen[lib$rowScreen[index], drop = FALSE]], newX = newX[, whichScreen[lib$rowScreen[index], drop = FALSE]], family = family, id = id, obsWeights = obsWeights)))
    if (inherits(testAlg, "try-error")) {
      warning(paste("Error in algorithm", lib$predAlgorithm[index], " on full data", "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n" ))
      out <- rep.int(NA, times = nrow(newX))
    } else {
      out <- testAlg$pred
      if (control$saveFitLibrary) {
        eval(bquote(fitLibrary[[.(index)]] <- .(testAlg$fit)), envir = fitLibEnv)
      }
    }
    if (verbose) {
      message(paste("full", libraryNames[index]))
    }
    invisible(out)
  }


  predY <- do.call('cbind', lapply(seq(k), FUN = .predFun,
                                   lib = library$library, Y = Y, dataX = X,
                                   newX = newX, whichScreen = whichScreen,
                                   family = family, id = id,
                                   obsWeights = obsWeights, verbose = verbose,
                                   control = control,
                                   libraryNames = libraryNames))

  # Add names of algorithms to the predictions.
  colnames(predY) <- libraryNames


  # Put everything together in a list.
  out <- list(
    call = call,
    libraryNames = libraryNames,
    SL.library = library,
    library.predict = predY,
    family = family,
    fitLibrary = get('fitLibrary', envir = fitLibEnv),
    varNames = varNames,
    whichScreen = whichScreen,
    control = control,
    env = env
  )
  class(out) <- c("SuperLearner")
  return(out)
}

#' @name tmle.g.fun
#' @aliases tmle.g.fun
#' @title TMLE function
#' @description Compute E[Yg] using TMLE
#'
#' @param A txt
#' @param Y outcome
#' @param gstarAW stochastic rule under A = A
#' @param gstar1W stochastic rule for A = 1
#' @param gstar0W stochastic rule for A = 0
#' @param gAW probability observed txt predictions
#' @param QAW outcome regression A = A
#' @param Q1W outcome regression A = 1
#' @param Q0W outcome regression A = 0
#' @param ab range of Y
#'
#' @return psi and IC of tmle g
#'
#' @export
#'
tmle.g.fun = function(A, Y, gstarAW, gstar1W, gstar0W, gAW, QAW, Q1W, Q0W, ab){

  HdW = gstarAW/gAW

  Y01 = (Y-min(ab))/diff(ab)
  QAW01 = (QAW-min(ab))/diff(ab)
  QAW01[QAW01>0.999] <- 0.999
  QAW01[QAW01<0.001] <- 0.001

  Q1W01 = (Q1W-min(ab))/diff(ab)
  Q1W01[Q1W01>0.999] <- 0.999
  Q1W01[Q1W01<0.001] <- 0.001

  Q0W01 = (Q0W-min(ab))/diff(ab)
  Q0W01[Q0W01>0.999] <- 0.999
  Q0W01[Q0W01<0.001] <- 0.001

  qlogisQAW01 = qlogis(QAW01)
  qlogisQ1W01 = qlogis(Q1W01)
  qlogisQ0W01 = qlogis(Q0W01)

  logitUpdateHAW <- glm(Y01 ~ offset(qlogisQAW01), weights = HdW, family='quasibinomial')
  epsilon = logitUpdateHAW$coef # get intercept coefficient

  QAW.star<- plogis(qlogisQAW01+ epsilon)*diff(ab) + min(ab)
  Q1W.star<- plogis(qlogisQ1W01+ epsilon)*diff(ab) + min(ab)
  Q0W.star<- plogis(qlogisQ0W01+ epsilon)*diff(ab) + min(ab)

  psi = mean(Q1W.star*gstar1W + Q0W.star*gstar0W)
  IC = HdW*(Y-QAW.star) + (Q1W.star*gstar1W + Q0W.star*gstar0W) - psi

  return(list(psi = psi, IC = IC))

}



#' @name tmle.rc.fun
#' @aliases tmle.rc.fun
#' @title TMLE function
#' @description Compute E[YgRC] using TMLE
#'
#' @param A txt
#' @param Y outcome
#' @param gstarAW Prd.is.A
#' @param gstar1W Prd.is.1
#' @param gstar0W Prd.is.0
#' @param gAW probability observed txt predictions
#' @param QAW outcome regression A = A
#' @param Q1W outcome regression A = 1
#' @param Q0W outcome regression A = 0
#' @param ab range of Y
#' @param tauP tauP
#' @param kappa kappa
#'
#' @return psi and IC of tmle rc
#'
#' @export
#'
tmle.rc.fun = function(A, Y, gstarAW, gstar1W, gstar0W, gAW, QAW, Q1W, Q0W, ab, tauP, kappa){

  HdW = gstarAW/gAW

  Y01 = (Y-min(ab))/diff(ab)
  QAW01 = (QAW-min(ab))/diff(ab)
  QAW01[QAW01>0.999] <- 0.999
  QAW01[QAW01<0.001] <- 0.001

  Q1W01 = (Q1W-min(ab))/diff(ab)
  Q1W01[Q1W01>0.999] <- 0.999
  Q1W01[Q1W01<0.001] <- 0.001

  Q0W01 = (Q0W-min(ab))/diff(ab)
  Q0W01[Q0W01>0.999] <- 0.999
  Q0W01[Q0W01<0.001] <- 0.001

  qlogisQAW01 = qlogis(QAW01)
  qlogisQ1W01 = qlogis(Q1W01)
  qlogisQ0W01 = qlogis(Q0W01)

  logitUpdateHAW <- glm(Y01 ~ offset(qlogisQAW01), weights = HdW, family='quasibinomial')
  epsilon = logitUpdateHAW$coef # get intercept coefficient

  QAW.star<- plogis(qlogisQAW01+ epsilon)*diff(ab) + min(ab)
  Q1W.star<- plogis(qlogisQ1W01+ epsilon)*diff(ab) + min(ab)
  Q0W.star<- plogis(qlogisQ0W01+ epsilon)*diff(ab) + min(ab)

  psi = mean(Q1W.star*gstar1W + Q0W.star*gstar0W)
  IC = HdW*(Y - QAW.star) + (Q1W.star*gstar1W + Q0W.star*gstar0W) - psi - tauP*((1*gstar1W + 0*gstar0W) - kappa)
  return(list(psi = psi, IC = IC))

}



#' @name tmle.d.fun
#' @aliases tmle.d.fun
#' @title TMLE function
#' @description Compute E[Yd] using TMLE
#'
#' @param A txt
#' @param Y outcome
#' @param d txt under rule
#' @param Qd outcome regression predictions under rule
#' @param gAW probability observed txt predictions
#' @param ab range of Y
#'
#' @return psi and IC of tmle d
#'
#' @export
#'
tmle.d.fun = function(A, Y, d, Qd, gAW, ab){

  Y01 = (Y-min(ab))/diff(ab)
  Qd01 = (Qd-min(ab))/diff(ab)
  Qd01[Qd01>0.999] <- 0.999
  Qd01[Qd01<0.001] <- 0.001

  H = (A==d)/gAW
  logit.Qd01 = qlogis(Qd01)
  update = glm(Y01~offset(logit.Qd01), weights = H, family="quasibinomial")

  Qd.star = predict(update, type = "response")*diff(ab)+min(ab)

  Psi_TMLE = mean(Qd.star)
  IC = H*(Y - Qd.star) + Qd.star - Psi_TMLE
  return(list(psi = Psi_TMLE, IC = IC, Qd.star = Qd.star))

}




#' @name dopt.fun
#' @aliases dopt.fun
#' @title dopt function
#' @description Compute dopt given blip
#'
#' @param blip predicted blip
#' @param kappa proportion of people who can be treated in population
#'
#' @return dopt or rc (stochastic) dopt
#'
#' @export
#'
# dopt.fun
# function that takes as input blip and kappa
# outputs dopt. If kappa is present, computes dopt with resource
# constraints according to kappa = proportion of people who can get treatment.
dopt.fun = function(blip, kappa = NULL){
  n = length(blip)
  if (is.null(kappa)) {
    toreturn = as.numeric(blip > 0)
  } else {
    poss_blip = c(blip, min(blip) - 1, max(blip) + 1)
    midtaus = data.frame(x = sort(unique(poss_blip))[1:length(unique(poss_blip))-1], y = sort(unique(poss_blip))[2:length(unique(poss_blip))])
    tau = sort(unique(c(min(blip), blip, rowMeans(midtaus), max(blip)))) # let tau vary from min(blip) to max(blip)
    surv = sapply(tau, function(x) mean(blip > x)) # proportion of blips greater than each tau
    eta = min(tau[which(surv <= kappa)]) #the smallest tau such that the survival prob is <= kappa
    tauP = max(c(eta, 0)) # max between nu and 0
    Prd.is.1 = ifelse(blip == tauP & tauP > 0, round((kappa - mean(blip > tauP)), 10)/round(mean(blip == tauP), 10), as.numeric(blip > tauP))
    toreturn = list(Prd.is.1 = Prd.is.1, tauP = tauP)
  }
  return(toreturn)
}





#' @name estimatorsEYd_nonCVTMLE
#' @aliases estimatorsEYd_nonCVTMLE
#' @title Estimators of E[Yd] that are not CV-TMLE
#' @description Estimators of E[Yd] that are not CV-TMLE
#'
#' @param W covariates
#' @param A treatment
#' @param Y outcome
#' @param d txt under rule
#' @param QAW.reg Q(A,W) regression object
#' @param gAW.reg g(A|W) regression object
#' @param ab range of Y
#' @param contrast contrast
#'
#' @return psi and CI non CVTMLE estimates
#'
#' @export
#'
estimatorsEYd_nonCVTMLE = function(W, A, Y, d, QAW.reg, g.reg, ab, contrast) {

  n = length(Y)
  family = ifelse(max(Y) <= 1 & min(Y) >= 0, "binomial", "gaussian")

  Qd = predict(QAW.reg, newdata = data.frame(W, A = d), type = "response")$pred
  g1W = predict(g.reg, data.frame(W), type = "response")$pred
  gAW = ifelse(A == 1, g1W, 1 - g1W)

  # Unadj
  Psi_unadj = mean(Y[A==d])
  var_unadj = var(Y[A==d])/sum(A==d)
  CI_unadj = Psi_unadj + c(-1, 1) * qnorm(0.975) * sqrt(var_unadj)

  # g-comp
  Psi_gcomp = mean(Qd)

  # IPTW
  Psi_IPTW = mean(Y*(1/gAW)*as.numeric(A == d))
  IC_IPTW = ((1/gAW)*as.numeric(A == d)*Y - mean(as.numeric(A == d)/gAW*Y))
  varIC_IPTW = var(IC_IPTW)/n
  CI_IPTW = Psi_IPTW + c(-1,1)*qnorm(0.975)*sqrt(varIC_IPTW)

  # IPTW-DR
  Psi_IPTW_DR = mean(as.numeric(A == d)/gAW * (Y-Qd) + Qd)
  IC_IPTW_DR = (as.numeric(A == d)/gAW * (Y-Qd) + Qd - mean(as.numeric(A == d)/gAW * (Y-Qd) + Qd))
  varIC_IPTW_DR = var(IC_IPTW_DR)/n
  CI_IPTW_DR = Psi_IPTW_DR + c(-1,1)*qnorm(0.975)*as.numeric(sqrt(varIC_IPTW_DR))

  # TMLE handcoded
  tmle_objects.d = tmle.d.fun(A = A, d = d, Y = Y, Qd = Qd, gAW = gAW, ab = ab)
  Psi_TMLE = tmle_objects.d$psi
  varIC_TMLE = var(tmle_objects.d$IC)/n
  CI_TMLE = Psi_TMLE + c(-1,1)*qnorm(0.975)*sqrt(varIC_TMLE)

  toreturn = data.frame(EYd = c(Psi_unadj = Psi_unadj,
                                CI_unadj = CI_unadj,
                                Psi_gcomp = Psi_gcomp,
                                Psi_IPTW = Psi_IPTW,
                                CI_IPTW = CI_IPTW,
                                Psi_IPTW_DR = Psi_IPTW_DR,
                                CI_IPTW_DR = CI_IPTW_DR,
                                Psi_TMLE = Psi_TMLE,
                                CI_TMLE = CI_TMLE))

  if (!is.null(contrast)) {

    # contrast_i = contrast[,i]
    contrast_fun = function(contrast_i) {

      Qcontrast_i = predict(QAW.reg, newdata = data.frame(W, A = contrast_i), type = "response")$pred

      # Unadj
      Psi_unadj_i = Psi_unadj - mean(Y[A==contrast_i])
      SE_Psi_unadj_i = sqrt(var(Y[A==d])/sum(A==d) + var(Y[A==contrast_i])/sum(A==contrast_i))
      CI_unadj_i = Psi_unadj_i + c(-1, 1) * qnorm(0.975) * SE_Psi_unadj_i

      # g-comp
      Psi_gcomp_i = Psi_gcomp - mean(Qcontrast_i)

      # IPTW
      Psi_IPTW_i = Psi_IPTW - mean(Y*(1/gAW)*as.numeric(A == contrast_i))
      IC_IPTW_contrast_i = ((1/gAW)*as.numeric(A == contrast_i)*Y - mean(as.numeric(A == contrast_i)/gAW*Y))
      IC_IPTW_i = IC_IPTW - IC_IPTW_contrast_i
      varIC_IPTW_i = var(IC_IPTW_i)/n
      CI_IPTW_i = Psi_IPTW_i + c(-1,1)*qnorm(0.975)*sqrt(varIC_IPTW_i)

      # IPTW-DR
      Psi_IPTW_DR_i = Psi_IPTW_DR - mean(as.numeric(A == contrast_i)/gAW * (Y-Qcontrast_i) + Qcontrast_i)
      IC_IPTW_DR_contrast_i = (as.numeric(A == contrast_i)/gAW * (Y-Qcontrast_i) + Qcontrast_i - mean(as.numeric(A == contrast_i)/gAW * (Y-contrast_i) + Qcontrast_i))
      IC_IPTW_DR_i = IC_IPTW_DR - IC_IPTW_DR_contrast_i
      varIC_IPTW_DR_i = var(IC_IPTW_DR_i)/n
      CI_IPTW_DR_i = Psi_IPTW_DR_i + c(-1,1)*qnorm(0.975)*as.numeric(sqrt(varIC_IPTW_DR_i))

      # TMLE handcoded
      tmle_objects.contrast_i = tmle.d.fun(A = A, d = contrast_i, Y = Y, Qd = Qcontrast_i, gAW = gAW, ab = ab)
      Psi_TMLE_i = Psi_TMLE - tmle_objects.contrast_i$psi
      varIC_TMLE_i = var(tmle_objects.d$IC - tmle_objects.contrast_i$IC)/n
      CI_TMLE_i = Psi_TMLE_i + c(-1,1)*qnorm(0.975)*sqrt(as.numeric(varIC_TMLE_i))

      toreturn_contrast = c(Psi_unadj = Psi_unadj_i,
                            CI_unadj = CI_unadj_i,
                            Psi_gcomp = Psi_gcomp_i,
                            Psi_IPTW = Psi_IPTW_i,
                            CI_IPTW = CI_IPTW_i,
                            Psi_IPTW_DR = Psi_IPTW_DR_i,
                            CI_IPTW_DR = CI_IPTW_DR_i,
                            Psi_TMLE = Psi_TMLE_i,
                            CI_TMLE = CI_TMLE_i)
      return(toreturn_contrast)
    }

      toreturn = cbind(EYd = toreturn, apply(contrast, 2, contrast_fun))


    }


  # if (length(grep("SL.QAW", QAW.SL.library)) != 0) {
  #   ltmle_objects.d = NA
  #   Psi_LTMLE = NA
  #   CI_LTMLE = c(NA,NA)
  # } else {
  #   # TMLE using ltmle
  #   ltmle_objects.d = summary(ltmle(data = data.frame(W, A, Y), Anodes = "A", Lnodes = NULL, Ynodes = "Y", gform = "A ~ 1", abar = as.matrix(d, nrow = n), SL.library = QAW.SL.library))
  #   Psi_LTMLE = ltmle_objects.d$treatment$estimate["tmle"]
  #   CI_LTMLE = ltmle_objects.d$treatment$CI
  # }
               #Psi_LTMLE = Psi_LTMLE,
               #CI_LTMLE = CI_LTMLE)

  return(toreturn)

}






#' @name estimatorsEYgstar_nonCVTMLE
#' @aliases estimatorsEYgstar_nonCVTMLE
#' @title Estimators of E[Ygstar] that are not CV-TMLE
#' @description Estimators of E[Ygstar] that are not CV-TMLE
#'
#' @param W covariates
#' @param A treatment
#' @param Y outcome
#' @param gstar1W gstar1W
#' @param gstar0W gstar0W
#' @param QAW.reg Q(A,W) regression object
#' @param gAW.reg g(A|W) regression object
#' @param ab range of Y
#' @param contrast contrast
#'
#' @return psi and CI for non CVTMLE estimates
#'
#' @export
#'
estimatorsEYgstar_nonCVTMLE = function(W, A, Y, gstar1W, gstar0W, QAW.reg, g.reg, ab, contrast) {

  n = length(Y)
  family = ifelse(max(Y) <= 1 & min(Y) >= 0, "binomial", "gaussian")

  Q1W = predict(QAW.reg, newdata = data.frame(W, A = 1), type = "response")$pred
  Q0W = predict(QAW.reg, newdata = data.frame(W, A = 0), type = "response")$pred
  QAW = predict(QAW.reg, newdata = data.frame(W, A = A), type = "response")$pred
  g1W = predict(g.reg, data.frame(W), type = "response")$pred
  gAW = ifelse(A == 1, g1W, 1 - g1W)
  gstarAW = ifelse(A == 1, gstar1W, gstar0W)

  # TMLE handcoded
  tmle_objects.g = tmle.g.fun(A = A,
                              gstarAW = gstarAW, gstar1W = gstar1W, gstar0W = gstar0W,
                              gAW = gAW,
                              QAW = QAW, Q1W = Q1W, Q0W = Q0W,
                              Y = Y, ab = ab)
  Psi_TMLE = tmle_objects.g$psi
  varIC_TMLE = var(tmle_objects.g$IC)/n
  CI_TMLE = Psi_TMLE + c(-1,1)*qnorm(0.975)*sqrt(as.numeric(varIC_TMLE))

  toreturn = data.frame(EYgstar = c(Psi_TMLE = Psi_TMLE,
                                CI_TMLE = CI_TMLE))

  if (!is.null(contrast)) {

    # contrast_i = contrast[,i]
    contrast_fun = function(contrast_i) {

      Qcontrast_i = predict(QAW.reg, newdata = data.frame(W, A = contrast_i), type = "response")$pred

      # TMLE handcoded
      tmle_objects.contrast_i = tmle.d.fun(A = A, d = contrast_i, Y = Y, Qd = Qcontrast_i, gAW = gAW, ab = ab)
      Psi_TMLE_i = Psi_TMLE - tmle_objects.contrast_i$psi
      varIC_TMLE_i = var(tmle_objects.g$IC - tmle_objects.contrast_i$IC)/n
      CI_TMLE_i = Psi_TMLE_i + c(-1,1)*qnorm(0.975)*sqrt(as.numeric(varIC_TMLE_i))

      toreturn_contrast = c(Psi_TMLE = Psi_TMLE_i,
                            CI_TMLE = CI_TMLE_i)
      return(toreturn_contrast)
    }

    toreturn = cbind(EYgstar = toreturn, apply(contrast, 2, contrast_fun))


  }

  return(toreturn)

}



#' @name estimatorsEYgRC_nonCVTMLE
#' @aliases estimatorsEYgRC_nonCVTMLE
#' @title Estimators of E[YgRC] that are not CV-TMLE
#' @description Estimators of E[YgRC] that are not CV-TMLE
#'
#' @param W covariates
#' @param A treatment
#' @param Y outcome
#' @param rc.out rc.out
#' @param kappa kappa
#' @param QAW.reg Q(A,W) regression object
#' @param gAW.reg g(A|W) regression object
#' @param ab range of Y
#' @param contrast contrast
#'
#' @return psi and CI for non CVTMLE estimates
#'
#' @export
#'
estimatorsEYgRC_nonCVTMLE = function(W, A, Y, rc.out, kappa, QAW.reg, g.reg, ab, contrast) {

  n = length(Y)
  family = ifelse(max(Y) <= 1 & min(Y) >= 0, "binomial", "gaussian")

  Q1W = predict(QAW.reg, newdata = data.frame(W, A = 1), type = "response")$pred
  Q0W = predict(QAW.reg, newdata = data.frame(W, A = 0), type = "response")$pred
  QAW = predict(QAW.reg, newdata = data.frame(W, A = A), type = "response")$pred
  g1W = predict(g.reg, data.frame(W), type = "response")$pred
  gAW = ifelse(A == 1, g1W, 1 - g1W)
  Prd.is.1 = rc.out$Prd.is.1
  Prd.is.0 = 1 - Prd.is.1
  Prd.is.A = ifelse(A == 1, Prd.is.1, Prd.is.0)

  # TMLE handcoded
  tmle_objects.rc = tmle.rc.fun(A = A, Y = Y,
                                gstarAW = Prd.is.A, gstar1W = Prd.is.1, gstar0W = Prd.is.0,
                                gAW = gAW,
                                QAW = QAW, Q1W = Q1W, Q0W = Q0W,
                                ab = ab,
                                tauP = rc.out$tauP,
                                kappa = kappa)

  Psi_TMLE = tmle_objects.rc$psi
  varIC_TMLE = var(tmle_objects.rc$IC)/n
  CI_TMLE = Psi_TMLE + c(-1,1)*qnorm(0.975)*sqrt(as.numeric(varIC_TMLE))

  toreturn = data.frame(EYgRC = c(Psi_TMLE = Psi_TMLE,
                                  CI_TMLE = CI_TMLE))

  if (!is.null(contrast)) {

    # contrast_i = contrast[,i]
    contrast_fun = function(contrast_i) {

      Qcontrast_i = predict(QAW.reg, newdata = data.frame(W, A = contrast_i), type = "response")$pred

      # TMLE handcoded
      tmle_objects.contrast_i = tmle.d.fun(A = A, d = contrast_i, Y = Y, Qd = Qcontrast_i, gAW = gAW, ab = ab)
      Psi_TMLE_i = Psi_TMLE - tmle_objects.contrast_i$psi
      varIC_TMLE_i = var(tmle_objects.rc$IC - tmle_objects.contrast_i$IC)/n
      CI_TMLE_i = Psi_TMLE_i + c(-1,1)*qnorm(0.975)*sqrt(as.numeric(varIC_TMLE_i))

      toreturn_contrast = c(Psi_TMLE = Psi_TMLE_i,
                            CI_TMLE = CI_TMLE_i)
      return(toreturn_contrast)
    }

    toreturn = cbind(EYgRC = toreturn, apply(contrast, 2, contrast_fun))


  }

  return(toreturn)

}



#' @name getpreds.dopt.fun
#' @aliases getpreds.dopt.fun
#' @title Candidate dopt for direct search algorithms
#' @description Candidate dopt predictionsfor direct search algorithms
#'
#' @param dopt.SL.library dopt library
#' @param W covariates
#' @param gform gform
#' @param V subset of covariates for designing ODTR
#' @param A txt
#' @param Y outcome
#' @param newV new V
#' @param QAW.reg regression object for Q(A,W)
#' @param g.reg regression object for g(A|W)
#' @param family family
#'
#' @return predicted dopt
#'
#' @export
#'
# function that has library of dopt algorithms
getpreds.dopt.fun = function(dopt.SL.library, blip.SL.library = NULL, W, V, A, Y, newV, QAW.reg, g.reg, family = family) {

  if (is.null(newV)){ newV = V }

  if (any(dopt.SL.library %in% c("OWL", "EARL", "optclass", "RWL", "Qlearn"))) {

    # propensity model
    g.pred <- function(object, newdata, g.reg, ...) {
      res <- predict.SuperLearner(object = g.reg, newdata = newdata, ...)
      return(res$pred)
    }
    moPropen = buildModelObj(model = ~1,
                             solver.method = "glm",
                             predict.method=g.pred,
                             predict.args = list("g.reg" = g.reg,
                                                 "newdata" = "newdata",
                                                 "object" = "object"))

    # outcome model
    QAW.pred <- function(object, newdata, QAW.reg, ...) {
      res <- predict.SuperLearner(object = QAW.reg, newdata = newdata, ...)
      return(res$pred)
    }
    moMain <- buildModelObj(model = ~1,
                            solver.method = "glm",
                            predict.method=QAW.pred,
                            predict.args = list("QAW.reg" = QAW.reg,
                                                "newdata" = "newdata",
                                                "object" = "object"))

    # classification model
    moClass <- buildModelObj(model = as.formula(paste("~ ", paste(colnames(W), collapse= "+"))),
                             solver.method = 'rpart',
                             solver.args = list(control = list("maxdepth"=2)),
                             predict.method = 'predict',
                             predict.args = list(type='class'))

    # regime
    regime <- as.formula(paste("~ ", paste(colnames(W), collapse= "+")))

  }



  dopts.list = list()

  if (any(dopt.SL.library == "DonV")) {
    QAW.pred = predict(QAW.reg, newdata = data.frame(W, A = A), type = "response")$pred
    Q1W.pred = predict(QAW.reg, newdata = data.frame(W, A = 1), type = "response")$pred
    Q0W.pred = predict(QAW.reg, newdata = data.frame(W, A = 0), type = "response")$pred
    g1W.pred = predict(g.reg, data.frame(W), type = "response")$pred
    gAW.pred = ifelse(A == 1, g1W.pred, 1 - g1W.pred)
    D = (2*A-1)/gAW.pred * (Y-QAW.pred) + Q1W.pred - Q0W.pred
    SL.blips = getpreds.blip.fun(Y = D, X = V, SL.library = blip.SL.library, newX = newV, family = 'gaussian')
    candidate.blips = SL.blips$library.predict
    dopt = data.frame(apply(candidate.blips > 0, 2, as.numeric))
    colnames(dopt) = paste0("DonV.", colnames(dopt))
    dopts.list$DonV = list(dopt = dopt, dopt.fit = SL.blips)
  }

  if (any(dopt.SL.library == "Qlearn")) {
    Qlearn.est = qLearn(moMain = moMain,
                        data = data.frame(W, A=A),
                        response = Y,
                        txName = 'A',
                        verbose = F)
    dopt = data.frame(optTx(Qlearn.est,  newdata = newV)$optimalTx)
    colnames(dopt) = "Qlearn"
    dopts.list$Qlearn = list(dopt = dopt, dopt.fit = Qlearn.est)
  }

  if (any(dopt.SL.library == "OWL")) {
    OWL.est = owl(moPropen = moPropen,
                  data = data.frame(W, A=A),
                  reward = Y,
                  txName = "A",
                  kernel = "poly",
                  regime = regime,
                  verbose = F)
    dopt = data.frame(optTx(OWL.est,newdata = newV)$optimalTx)
    colnames(dopt) = "OWL"
    dopts.list$OWL = list(dopt = dopt, dopt.fit = OWL.est)
  }

  if (any(dopt.SL.library == "EARL")) {
    EARL.est <- earl(moPropen = moPropen,
                     momain = moMain,
                     data = data.frame(W, A,Y),
                     response = Y,
                     txName = 'A',
                     regime = regime,
                     verbose = F)
    dopt = data.frame(optTx(EARL.est, newdata = newV)$optimalTx)
    colnames(dopt) = "EARL"
    dopts.list$EARL = list(dopt = dopt, dopt.fit = EARL.est)
  }

  if (any(dopt.SL.library == "optclass")) {
    optclass.est <- optimalClass(moPropen = moPropen,
                                 moClass = moClass,
                                 data = data.frame(W, A,Y),
                                 response = Y,
                                 txName = 'A',
                                 regime = regime,
                                 verbose = F)
    dopt = data.frame(optTx(x = optclass.est, newdata = newV)$optimalTx)
    colnames(dopt) = "optclass"
    dopts.list$optclass = list(dopt = dopt, dopt.fit = optclass.est)
  }

  if (any(dopt.SL.library == "RWL")) {
    responseType = ifelse(max(Y) <= 1 & min(Y) >= 0, "binary", "continuous")
    RWL.est <- rwl(moPropen = moPropen,
                   moMain = moMain,
                   data = data.frame(W, A),
                   reward = Y,
                   txName = 'A',
                   regime = regime,
                   verbose = F,
                   responseType = responseType)
    dopt = data.frame(optTx(RWL.est, newdata = newV)$optimalTx)
    colnames(dopt) = "RWL"
    dopts.list$RWL = list(dopt = dopt, dopt.fit = RWL.est)
  }

  if (any(dopt.SL.library == "treatall")) {
    dopt = data.frame(rep(1, nrow(newV)))
    colnames(dopt) = "treatall"
    dopts.list$treatall = list(dopt = dopt, dopt.fit = "treat all")
  }

  if (any(dopt.SL.library == "treatnone")) {
    dopt = data.frame(rep(0, nrow(newV)))
    colnames(dopt) = "treatnone"
    dopts.list$treatnone = list(dopt = dopt, dopt.fit = "treat no one")
  }

  toreturn = list()
  toreturn$library.predict = do.call("cbind", lapply(dopts.list, function(x) x$dopt))
  toreturn$fitLibrary = lapply(dopts.list, function(x) x$dopt.fit)

  return(toreturn)


}


