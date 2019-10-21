#' @name getpreds.fun
#' @aliases getpreds.fun
#' @title Predictions
#' @description Get library predictions (what would be library.predict). Code mostly from SuperLearner.
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
#' @return
#'
#' @export
#'
# Helper Functions

# get what would've been library.predict
# code mostly from SuperLearner

getpreds.fun <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
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


#' @name SL.QAW.incorrect
#' @aliases SL.QAW.incorrect
#' @title Epi HTE GLM
#' @description Standard HTE GLM used in Epi analyses.
#'
#' @param Y outcome
#' @param X predictors
#' @param newX new X
#' @param family family
#' @param SL.library SL library
#' @param obsWeights obsWeights
#' @param model model
#' @param ... other
#'
#' @return
#'
#' @export
#'
#SL.QAW.incorrect
#correctly specified param model QAW
SL.QAW.incorrect = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ W1 + W2 + W3 + W4 + A*(W1 + W2 +W3 + W4), data = X, family = family, weights = obsWeights,
                 model = model)
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}



#' @name SL.QAW.correct_smooth
#' @aliases SL.QAW.correct_smooth
#' @title Correctly specified model for DB smooth function
#' @description Correctly specified outcome regression model for DB smooth function
#'
#' @param Y outcome
#' @param X predictors
#' @param newX new X
#' @param family family
#' @param SL.library SL library
#' @param obsWeights obsWeights
#' @param model model
#' @param ... other
#'
#' @return
#'
#' @export
#'
#SL.QAW.correct_smooth
#correctly specified param model QAW for David's DGP with continuous outcome
SL.QAW.correct_smooth = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ W1 + A:I(W1^2) + W2 + A:W2 + W3:W1:A + I(W4^2) + W4 + A, data = X, family = family, weights = obsWeights,
                 model = model)
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}





#' @name SL.blip.correct_smooth
#' @aliases SL.blip.correct_smooth
#' @title Correctly specified model for DB smooth function
#' @description Correctly specified blip model for DB smooth function
#'
#' @param Y outcome
#' @param X predictors
#' @param newX new X
#' @param family family
#' @param SL.library SL library
#' @param obsWeights obsWeights
#' @param model model
#' @param ... other
#'
#' @return
#'
#' @export
#'

#SL.blip.correct_smooth
#correctly specified param model blip for David's DGP with cont outcome
SL.blip.correct_smooth = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ I(W1^2) + W2 + W3:W1, data = X, family = family, weights = obsWeights,
                 model = model)
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}




#' @name tmle.fun
#' @aliases tmle.fun
#' @title TMLE function
#' @description Compute E[Yd] using TMLE
#'
#' @param A txt
#' @param Y outcome
#' @param d txt under rule
#' @param Qd outcome regression predictions under rule
#' @param gAW P(A|W)
#' @param ab range of Y
#'
#' @return
#'
#' @export
#'
# tmle.fun
# function that takes as input A, Y, txt under decision rule, mean under decision rule, gAW, and family
# outputs TMLE estimate of mean under decision rule
tmle.fun = function(A, Y, d, Qd, gAW, ab){

  Y01 = (Y-min(ab))/diff(ab)
  Qd01 = (Qd-min(ab))/diff(ab)
  Qd01[Qd01>0.999] <- 0.999
  Qd01[Qd01<0.001] <- 0.001

  H = (A==d)/gAW
  logit.Qd01 = qlogis(Qd01)
  update = glm(Y01~offset(logit.Qd01), weights = H, family="quasibinomial")

  Qd.star = predict(update, type = "response")*diff(ab)+min(ab)
  Y = Y01*diff(ab)+min(ab)

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
#' @return
#'
#' @export
#'
# dopt.fun
# function that takes as input blip and kappa
# outputs dopt. If kappa is present, computes dopt with resource
# constraints according to kappa = proportion of poeple who can get treatment.
dopt.fun = function(blip, kappa){
  n = length(blip)
  if (is.null(kappa)) {
    dopt = as.numeric(blip > 0)
  } else {
    # estimate S0
    tau = seq(from = min(blip)-1, to = max(blip)+1, length.out = 500) # let tau vary from min(blip) to max(blip)
    surv = sapply(tau, function(x) mean(blip > x)) #probability that the blip is greater than some varying tau
    # estimate nu
    nu = ifelse(sum(surv<=kappa)==0, 0, min(tau[which(surv <= kappa)])) #the biggest tau such that the survival prob is <= kappa
    # estimate tau0
    tauP = max(c(nu, 0)) # max between nu and 0
    # estimate dopt with RC
    if (sum(!(blip == tauP & tauP > 0)) == 0) {
      prob.d.is.1 = dopt = rep(NA, n)
      for (i in 1:n) {
        prob.d.is.1[i] = kappa - mean(blip > tauP)
        dopt[i] = rbinom(n = 1, size = 1, prob = prob.d.is.1[i])
      }
    } else {
      dopt = as.numeric(blip > tauP)
    }
  }
  return(dopt)
}





#' @name estimatorsEYdopt_nonCVTMLE
#' @aliases estimatorsEYdopt_nonCVTMLE
#' @title Estimators of E[Yd] that are not CV-TMLE
#' @description Estimators of E[Yd] that are not CV-TMLE
#'
#' @param W covariates
#' @param A treatment
#' @param Y outcome
#' @param dopt txt under rule
#' @param QAW.reg QAW regression object
#' @param gAW P(A|W)
#' @param QAW.SL.library library for QAW
#' @param ab range of Y
#'
#' @return
#'
#' @export
#'
estimatorsEYdopt_nonCVTMLE = function(W, A, Y, dopt, QAW.reg, gAW, QAW.SL.library, ab) {

  n = length(Y)
  family = ifelse(max(Y) <= 1 & min(Y) >= 0, "binomial", "gaussian")

  Qdopt = predict(QAW.reg, newdata = data.frame(W, A = dopt), type = "response")$pred

  # Unadj
  Psi_unadj = mean(Y[A==dopt])
  var_unadj = var(Y[A==dopt])/sum(A==dopt)
  CI_unadj = Psi_unadj + c(-1, 1) * qnorm(0.975) * sqrt(var_unadj)

  # g-comp
  Psi_gcomp = mean(Qdopt)

  # IPTW
  Psi_IPTW = mean(Y*(1/gAW)*as.numeric(A == dopt))
  IC_IPTW = ((1/gAW)*as.numeric(A == dopt)*Y - mean(as.numeric(A == dopt)/gAW*Y))
  varIC_IPTW = var(IC_IPTW)/n
  CI_IPTW = Psi_IPTW + c(-1,1)*qnorm(0.975)*sqrt(varIC_IPTW)

  # IPTW-DR
  Psi_IPTW_DR = mean(as.numeric(A == dopt)/gAW * (Y-Qdopt) + Qdopt)
  IC_IPTW_DR = (as.numeric(A == dopt)/gAW * (Y-Qdopt) + Qdopt - mean(as.numeric(A == dopt)/gAW * (Y-Qdopt) + Qdopt))
  varIC_IPTW_DR = var(IC_IPTW_DR)/n
  CI_IPTW_DR = Psi_IPTW_DR + c(-1,1)*qnorm(0.975)*as.numeric(sqrt(varIC_IPTW_DR))

  # TMLE handcoded
  tmle_objects.dopt = tmle.fun(A = A, d = dopt, Y = Y, Qd = Qdopt, gAW = gAW, ab = ab)
  Psi_TMLE = tmle_objects.dopt$psi
  varIC_TMLE = var(tmle_objects.dopt$IC)/n
  CI_TMLE = Psi_TMLE + c(-1,1)*qnorm(0.975)*sqrt(varIC_TMLE)

  if (any(QAW.SL.library == "SL.QAW.correct_cont" | QAW.SL.library == "SL.QAW.incorrect" | QAW.SL.library == "SL.QAW.correct_smooth")) {
    ltmle_objects.dopt = NA
    Psi_LTMLE = NA
    CI_LTMLE = c(NA,NA)
  } else {
    # TMLE using ltmle
    ltmle_objects.dopt = summary(ltmle(data = data.frame(W, A, Y), Anodes = "A", Lnodes = NULL, Ynodes = "Y", gform = "A ~ 1", abar = as.matrix(dopt, nrow = n), SL.library = QAW.SL.library))
    Psi_LTMLE = ltmle_objects.dopt$treatment$estimate["tmle"]
    CI_LTMLE = ltmle_objects.dopt$treatment$CI
  }


  toreturn = c(Psi_unadj = Psi_unadj,
               CI_unadj = CI_unadj,
               Psi_gcomp = Psi_gcomp,
               Psi_IPTW = Psi_IPTW,
               CI_IPTW = CI_IPTW,
               Psi_IPTW_DR = Psi_IPTW_DR,
               CI_IPTW_DR = CI_IPTW_DR,
               Psi_TMLE = Psi_TMLE,
               CI_TMLE = CI_TMLE,
               Psi_LTMLE = Psi_LTMLE,
               CI_LTMLE = CI_LTMLE)

  return(toreturn)

}



#' @name candidate_dopts
#' @aliases candidate_dopts
#' @title Candidate dopt for direct search algorithms
#' @description Candidate dopt predictionsfor direct search algorithms
#'
#' @param dopt.SL.library dopt library
#' @param W covariates
#' @param W_for_g covariates for estimating g
#' @param V subset of covariates for designing ODTR
#' @param A txt
#' @param Y outcome
#' @param newW new W
#' @param newV new V
#' @param newA new A
#' @param new Y
#' @param QAW.reg regression object for QAW
#' @param gAW P(A|W)
#' @param moMain_model for DynTxRegime
#' @param moCont_model for DynTxRegime
#' @param family family
#'
#' @return
#'
#' @export
#'
# function that has library of dopt algorithms
candidate_dopts = function(dopt.SL.library, W, W_for_g, V, A, Y, newW, newV, newA, newY, QAW.reg, gAW, moMain_model = NULL, moCont_model = NULL, family = family) {

  if (any(dopt.SL.library %in% c("OWL", "EARL", "optclass", "RWL", "Qlearn"))) {

   # form = as.formula(paste("~ ", paste(colnames(W), collapse= "+")))

    # propensity model
    moPropen <- buildModelObj(model = as.formula(paste("~ ", paste(W_for_g, collapse= "+"))),
                              solver.method = 'glm',
                              solver.args = list(family='binomial'),
                              predict.method = 'predict.glm',
                              predict.args = list(type='response'))

    if (is.null(moMain_model) & is.null(moCont_model)) {
      # main model
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
    } else {
      moCont <- buildModelObj(model = as.formula(paste("~ ", moCont_model)),
                                  solver.method = 'glm',
                                  solver.args = list(family=family),
                                  predict.method = 'predict.glm',
                                  predict.args = list(type='response'))
      moMain <- buildModelObj(model = as.formula(paste("~ ", moMain_model)),
                              solver.method = 'glm',
                              solver.args = list(family=family),
                              predict.method = 'predict.glm',
                              predict.args = list(type='response'))
    }

    # classification model
    moClass <- buildModelObj(model = as.formula(paste("~ ", paste(colnames(W), collapse= "+"))),
                             solver.method = 'rpart',
                             solver.args = list(control = list("maxdepth"=2)),
                             predict.method = 'predict',
                             predict.args = list(type='class'))

    # regime
    regime <- as.formula(paste("~ ", paste(colnames(W), collapse= "+")))


    optclass = function(W, A, Y, moPropen, moClass, regime, newW, newA, newY) {
      optclass.est <- optimalClass(moPropen = moPropen,
                                   moClass = moClass,
                                   data = data.frame(W, A,Y),
                                   response = Y,
                                   txName = 'A',
                                   regime = regime,
                                   verbose = F)

      dopt = data.frame(optTx(optclass.est, data.frame(newW, A=newA, Y=newY))$optimalTx)
      colnames(dopt) = "optclass"
      return(dopt)
    }

    EARL = function(W, A, Y, moPropen, moMain, regime, newW, newA, newY) {
      EARL.est <- earl(moPropen = moPropen,
                       momain = moMain,
                     #  moCont = moCont,
                       data = data.frame(W, A,Y),
                       response = Y,
                       txName = 'A',
                       regime = regime,
                       verbose = F)
      dopt = data.frame(optTx(EARL.est, data.frame(newW, A=newA, Y=newY))$optimalTx)
      colnames(dopt) = "EARL"
      return(dopt)
    }

    RWL = function(W, A, Y, moPropen, moMain, regime, newW, newA, newY) {
      responseType = ifelse(max(Y) <= 1 & min(Y) >= 0, "binary", "continuous")
      RWL.est <- rwl(moPropen = moPropen,
                     moMain = moMain,
                     data = data.frame(W, A),
                     reward = Y,
                     txName = 'A',
                     regime = regime,
                     verbose = F,
                     responseType = responseType)
      dopt = data.frame(optTx(RWL.est, data.frame(newW, A=newA, Y=newY))$optimalTx)
      colnames(dopt) = "RWL"
      return(dopt)
    }

    OWL = function (W, A, Y, moPropen, regime, newW, newA, newY) {
      OWL.est = owl(moPropen = moPropen,
                    data = data.frame(W, A=A),
                    reward = Y,
                    txName = "A",
                    kernel = "poly",
                    regime = regime,
                    verbose = F)
      dopt = data.frame(optTx(OWL.est, data.frame(newW, A=newA, Y=newY))$optimalTx)
      colnames(dopt) = "OWL"
      return(dopt)
    }

    Qlearn = function (W, A, Y, moMain, regime, newW, newA, newY) {
      Qlearn.est = qLearn(moMain = moMain,
                          data = data.frame(W, A=A),
                          response = Y,
                          txName = 'A',
                          verbose = F)
      dopt = data.frame(optTx(Qlearn.est, newdata = data.frame(newW, A=newA))$optimalTx)
      colnames(dopt) = "Qlearn"
      return(dopt)
    }

  }


  DonV = function(W, V, A, Y, gAW, QAW.reg, blip.SL.library, newV) {
    QAW.pred = predict(QAW.reg, newdata = data.frame(W, A = A), type = "response")$pred
    Q1W.pred = predict(QAW.reg, newdata = data.frame(W, A = 1), type = "response")$pred
    Q0W.pred = predict(QAW.reg, newdata = data.frame(W, A = 0), type = "response")$pred
    D = (2*A-1)/gAW * (Y-QAW.pred) + Q1W.pred - Q0W.pred
    SL.blips = getpreds.fun(Y = D, X = V, SL.library = blip.SL.library, newX = newV, family = 'gaussian')
    candidate.blips = SL.blips$library.predict
    dopt = data.frame(apply(candidate.blips > 0, 2, as.numeric))
    colnames(dopt) = paste0("DonV.", colnames(dopt))
    return(dopt)
  }

  treatall = function(newV) {
    dopt = data.frame(rep(1, nrow(newV)))
    colnames(dopt) = "treatall"
    return(dopt)
  }


  treatnone = function(newV) {
    dopt = data.frame(rep(0, nrow(newV)))
    colnames(dopt) = "treatnone"
    return(dopt)
  }


  dopts.list = list()

  if (any(dopt.SL.library == "DonV")) { dopts.list$DonV = DonV(W = W, V = V, A = A, Y = Y, gAW = gAW, QAW.reg = QAW.reg, blip.SL.library = blip.SL.library, newV = newV)}
  if (any(dopt.SL.library == "Qlearn")) { dopts.list$Qlearn = Qlearn(W = W, A = A, Y = Y, moMain = moMain, regime = regime, newW = newW, newA = newA, newY = newY)}
  if (any(dopt.SL.library == "OWL")) {dopts.list$OWL = OWL(W = W, A = A, Y = Y, moPropen = moPropen, regime = regime, newW = newW, newA = newA, newY = newY)}
  if (any(dopt.SL.library == "EARL")) {dopts.list$EARL = EARL(W = W, A = A, Y = Y, moPropen = moPropen, moMain = moMain, regime = regime, newW = newW, newA = newA, newY = newY)}
  if (any(dopt.SL.library == "optclass")) {dopts.list$optclass = optclass(W = W, A = A, Y = Y, moPropen = moPropen, moClass = moClass, regime = regime, newW = newW, newA = newA, newY = newY)}
  if (any(dopt.SL.library == "RWL")) {dopts.list$RWL = RWL(W = W, A = A, Y = Y, moPropen = moPropen, moMain = moMain, regime = regime, newW = newW, newA = newA, newY = newY)}
  if (any(dopt.SL.library == "treatall")) {dopts.list$treatall = treatall(newV = newV)}
  if (any(dopt.SL.library == "treatnone")) {dopts.list$treatnone = treatnone(newV = newV)}

  candidate_dopts = do.call("cbind", dopts.list)

  return(candidate_dopts)


}


