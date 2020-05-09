#' @name SL.blip
#' @aliases SL.blip
#' @title Blip-based SL
#' @description SuperLearner for ODTR that returns estimated blips.
#'
#' @param V subset covariates for designing ODTR
#' @param W covariates
#' @param A txt
#' @param Y outcome
#' @param ab range of Y
#' @param QAW.reg Q(A,W) regression object
#' @param g.reg g(A|W) regression object
#' @param blip.SL.library blip SL library
#' @param risk.type risk type
#' @param grid size
#' @param newV new V
#' @param VFolds number of folds
#' @param family family for outcome
#' @param discrete.SL whether discrete SL (choose one algorithm) or continuous SL (weighted combination of algorithms)
#'
#' @return
#'
#' @export
#'

SL.blip = function(V, W, A, Y, ab, QAW.reg, g.reg, blip.SL.library,
                   risk.type, grid.size,
                   newV = NULL, VFolds, family, discrete.SL){

  n = length(A)

  libraryNames = c(blip.SL.library) # will be trouble if screeners are used?
  numalgs = length(libraryNames)

  if (discrete.SL) {
    simplex.grid = diag(numalgs)
  } else {
    simplex.grid = rbind(diag(numalgs), simplex.sample(n = numalgs, N = grid.size)$samples)
  }

  colnames(simplex.grid) = libraryNames

  SL.out = list()

  folds = sample(1:VFolds, size = n, replace = T)
  CV.risk_fun = function(i) {
    train_ind = folds != i
    test_ind = folds == i
    g.reg.train = SuperLearner(Y = A[train_ind], X = W[train_ind,], SL.library = g.reg$SL.library$library$predAlgorithm, family = "binomial")
    g1W.pred = predict(g.reg.train, data.frame(W), type = "response")$pred
    gAW.pred = ifelse(A == 1, g1W.pred, 1 - g1W.pred)
    QAW.reg.train = SuperLearner(Y = Y[train_ind], X = data.frame(A, W)[train_ind,], SL.library = QAW.reg$SL.library$library$predAlgorithm, family = family)
    QAW.pred = predict(QAW.reg.train, newdata = data.frame(W, A = A), type = "response")$pred
    Q1W.pred = predict(QAW.reg.train, newdata = data.frame(W, A = 1), type = "response")$pred
    Q0W.pred = predict(QAW.reg.train, newdata = data.frame(W, A = 0), type = "response")$pred
    D = (2*A-1)/gAW.pred * (Y-QAW.pred) + Q1W.pred - Q0W.pred
    SL.init.train = getpreds.blip.fun(Y = D[train_ind], X = V[train_ind,],
                                      SL.library = blip.SL.library,
                                      newX = V[test_ind,], family = 'gaussian')
    candidate.blips.test = SL.init.train$library.predict
    candidates.blipsXalpha.test = candidate.blips.test%*%t(simplex.grid)
    dopt.combos.test = apply(candidates.blipsXalpha.test > 0, 2, as.numeric)
    Qdopt.combos.test = sapply(1:nrow(simplex.grid), function(x) predict(QAW.reg.train, newdata = data.frame(W[test_ind,], A = dopt.combos.test[,x]), type = "response")$pred)
    if (risk.type == "CV TMLE" | risk.type == "CV TMLE CI") {
      tmle.obj.test = lapply(1:nrow(simplex.grid), function(x) tmle.fun(A = A[test_ind], Y = Y[test_ind], d = dopt.combos.test[,x], Qd = Qdopt.combos.test[,x], gAW = gAW.pred[test_ind], ab = ab))
      risk.combos.test = -unlist(lapply(tmle.obj.test, function(x) x$psi))
      risk.var.combos.test = unlist(lapply(tmle.obj.test, function(x) var(x$IC)))
      toreturn = list(risk.combos.test = risk.combos.test, risk.var.combos.test = risk.var.combos.test)
    } else if (risk.type == "CV MSE") {
      risk.combos.test = sapply(1:nrow(simplex.grid), function(x) mean((D[test_ind] - candidates.blipsXalpha.test[,x])^2))
      toreturn = list(risk.combos.test = risk.combos.test)
    }
    return(toreturn)
  }

  CV.risk.obj = lapply(1:VFolds, CV.risk_fun)
  CV.risk = colMeans(t(sapply(1:VFolds, function(i) CV.risk.obj[[i]]$risk.combos.test)))
  if (risk.type == "CV TMLE" | risk.type == "CV TMLE CI") {
    var_CV.TMLE = colMeans(t(sapply(1:VFolds, function(i) CV.risk.obj[[i]]$risk.var.combos.test)))/n
    CI_CV.TMLE_upper = CV.risk + qnorm(0.975)*sqrt(var_CV.TMLE)
    CI_CV.TMLE_lower = CV.risk - qnorm(0.975)*sqrt(var_CV.TMLE)
  }

  if (risk.type == "CV TMLE CI") {
    SL.out$CV.risk_min = list(est = min(CI_CV.TMLE_upper))
    SL.out$coef = simplex.grid[which.min(CI_CV.TMLE_upper),]
  } else {
    SL.out$CV.risk_min = list(est = min(CV.risk))
    SL.out$coef = simplex.grid[which.min(CV.risk),]
  }

  g1W.pred = predict(g.reg, W, type = "response")$pred
  gAW.pred = ifelse(A == 1, g1W.pred, 1 - g1W.pred)
  QAW.pred = predict(QAW.reg, newdata = data.frame(W, A = A), type = "response")$pred
  Q1W.pred = predict(QAW.reg, newdata = data.frame(W, A = 1), type = "response")$pred
  Q0W.pred = predict(QAW.reg, newdata = data.frame(W, A = 0), type = "response")$pred
  D = (2*A-1)/gAW.pred * (Y-QAW.pred) + Q1W.pred - Q0W.pred # this is same thing as D1 - D0 (weighted Y1 - Y0)

  SL.init = getpreds.blip.fun(Y = D, X = V, SL.library = blip.SL.library, newX = newV, family = "gaussian")

  # applying chosen coefficients
  SL.out$libraryNames = names(SL.out$coef) = libraryNames
  SL.out$fitBlipLibrary = SL.init$fitLibrary
  SL.out$blipFamily = SL.init$family
  SL.out$libraryBlipPredict = SL.init$library.predict
  SL.out$SL.predict = SL.out$libraryBlipPredict%*%SL.out$coef
  if (risk.type == "CV TMLE") {
    SL.out$CV.risk_min$CI = c(lowerCI = CI_CV.TMLE_lower[which.min(CV.risk)],
                              upperCI = CI_CV.TMLE_upper[which.min(CV.risk)])
  }
  if (risk.type == "CV TMLE CI") {
    SL.out$CV.risk_min$CI = c(lowerCI = CI_CV.TMLE_lower[which.min(CI_CV.TMLE_upper)],
                              upperCI = CI_CV.TMLE_upper[which.min(CI_CV.TMLE_upper)])
  }

  return(SL.out)


}




#' @name SL.vote
#' @aliases SL.vote
#' @title Vote-based SL
#' @description SuperLearner for ODTR that returns estimated txts under rule
#'
#' @param V subset covariates for designing ODTR
#' @param W covariates
#' @param A txt
#' @param Y outcome
#' @param ab range of Y
#' @param QAW.reg Q(A,W) regression object
#' @param g.reg g(A|W) regression object
#' @param blip.SL.library blip SL library
#' @param risk.type risk type
#' @param grid size
#' @param newV newV
#' @param VFolds number of folds
#' @param dopt.SL.library dopt SL library. Options: "DonV", "Qlearn", "OWL", "EARL", "optclass", "RWL", "treatall", "treatnone". Can also be "all".
#' @param family family for outcome
#' @param discrete.SL whether discrete SL (choose one algorithm) or continuous SL (weighted combination of algorithms)
#'
#' @return
#'
#' @export
#'

SL.vote = function(V, W, A, Y, ab, QAW.reg, g.reg, blip.SL.library,
                   dopt.SL.library,
                   risk.type,
                   grid.size,
                   newV = NULL,
                   VFolds, family, discrete.SL){

  n = length(A)
  family = ifelse(max(Y) <= 1 & min(Y) >= 0, "binomial", "gaussian")

  if (sum(dopt.SL.library == "all")>0) { dopt.SL.library = c("DonV", "Qlearn", "OWL", "EARL", "optclass", "RWL", "treatall", "treatnone")}
  numalgs = length(dopt.SL.library)-1 + length(blip.SL.library) #must have DonV

  if (discrete.SL) {
    simplex.grid = diag(numalgs)
  } else {
    simplex.grid = rbind(diag(numalgs), simplex.sample(n = numalgs, N = grid.size)$samples)
  }

  SL.out = list()

  folds = sample(1:VFolds, size = n, replace = T)
  CV.risk_fun = function(i){
    train_ind = folds != i
    test_ind = folds == i
    g.reg.train = SuperLearner(Y = A[train_ind], X = W[train_ind,], SL.library = g.reg$SL.library$library$predAlgorithm, family = "binomial")
    g1W.pred = predict(g.reg.train, data.frame(W), type = "response")$pred
    gAW.pred = ifelse(A == 1, g1W.pred, 1 - g1W.pred)
    QAW.reg.train = SuperLearner(Y = Y[train_ind], X = data.frame(A, W)[train_ind,], SL.library = QAW.reg$SL.library$library$predAlgorithm, family = family)
    candidate.dopts.test = getpreds.dopt.fun(dopt.SL.library = dopt.SL.library, blip.SL.library = blip.SL.library,
                                             W = W[train_ind,], V = V[train_ind,], A = A[train_ind], Y = Y[train_ind],
                                             newV = V[test_ind,], QAW.reg = QAW.reg.train, g.reg = g.reg.train,
                                             family = family)
    candidates.doptsXalpha.test = as.matrix(candidate.dopts.test$library.predict)%*%t(simplex.grid)
    dopt.combos.test = apply(candidates.doptsXalpha.test > .5, 2, as.numeric)
    Qdopt.combos.test = sapply(1:nrow(simplex.grid), function(x) predict(QAW.reg.train, newdata = data.frame(W[test_ind,], A = dopt.combos.test[,x]), type = "response")$pred)
    tmle.obj.test = lapply(1:nrow(simplex.grid), function(x) tmle.fun(A = A[test_ind], Y = Y[test_ind], d = dopt.combos.test[,x], Qd = Qdopt.combos.test[,x], gAW = gAW.pred[test_ind], ab = ab))
    risk.combos.test = -unlist(lapply(tmle.obj.test, function(x) x$psi))
    risk.var.combos.test = unlist(lapply(tmle.obj.test, function(x) var(x$IC)))
    toreturn = list(risk.combos.test = risk.combos.test, risk.var.combos.test = risk.var.combos.test)
    return(toreturn)
  }
  CV.risk.obj = lapply(1:VFolds, CV.risk_fun)
  CV.risk = colMeans(t(sapply(1:VFolds, function(i) CV.risk.obj[[i]]$risk.combos.test)))
  if (risk.type == "CV TMLE" | risk.type == "CV TMLE CI") {
    var_CV.TMLE = colMeans(t(sapply(1:VFolds, function(i) CV.risk.obj[[i]]$risk.var.combos.test)))/n
    CI_CV.TMLE_upper = CV.risk + qnorm(0.975)*sqrt(var_CV.TMLE)
    CI_CV.TMLE_lower = CV.risk - qnorm(0.975)*sqrt(var_CV.TMLE)
  }

  if (risk.type == "CV TMLE CI") {
    SL.out$CV.risk_min = list(est = min(CI_CV.TMLE_upper))
    SL.out$coef = simplex.grid[which.min(CI_CV.TMLE_upper),]
  } else {
    SL.out$CV.risk_min = list(est = min(CV.risk))
    SL.out$coef = simplex.grid[which.min(CV.risk),]
  }


  # predict on new data
  SL.init = getpreds.dopt.fun(dopt.SL.library = dopt.SL.library, blip.SL.library = blip.SL.library,
                              W = W, V = V, A = A, Y = Y, newV = newV,
                              QAW.reg = QAW.reg, g.reg = g.reg,
                              family = family)
  SL.out$librarydoptPredict = SL.init$library.predict
  SL.out$SL.predict = as.numeric(as.matrix(SL.out$librarydoptPredict)%*%SL.out$coef > .5)
  SL.out$libraryNames = names(SL.out$coef) = names(SL.out$librarydoptPredict)
  SL.out$fitdoptLibrary = SL.init$fitLibrary
  SL.out$doptFamily = family

  if (risk.type == "CV TMLE") {
    SL.out$CV.risk_min$CI = c(lowerCI = CI_CV.TMLE_lower[which.min(CV.risk)],
                              upperCI = CI_CV.TMLE_upper[which.min(CV.risk)])
  }
  if (risk.type == "CV TMLE CI") {
    SL.out$CV.risk_min$CI = c(lowerCI = CI_CV.TMLE_lower[which.min(CI_CV.TMLE_upper)],
                              upperCI = CI_CV.TMLE_upper[which.min(CI_CV.TMLE_upper)])
  }

  return(SL.out)


}




