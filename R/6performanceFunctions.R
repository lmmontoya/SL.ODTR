#' @name performance_ODTR
#' @aliases performance_ODTR
#' @title Performance ODTR
#' @description performance function for the ODTR
#'
#' @param x dummy
#' @param n n
#' @param risk.type risk.type
#' @param DGP_fun DGP_fun
#' @param QAW QAW
#' @param QAW.SL.library library for QAW
#' @param blip.SL.library library for blip
#' @param dopt.SL.library library for dopt
#' @param metalearner SL type
#'
#' @return
#'
#' @export
#'
# optimal dynamic regime performance function
# odtr_performance

performance_ODTR = function(x, n, risk.type, DGP_fun, QAW, QAW.SL.library, blip.SL.library, dopt.SL.library, metalearner){

  ObsData = subset(DGP_fun(n), select = -c(A_star, Y_star))
  W = subset(ObsData, select = -c(A, Y))
  V = W
  A = ObsData$A
  Y = ObsData$Y
  grid.size = 500
  g.SL.library = "SL.mean"

  results = odtr(V=V, W=W, A=A, Y=Y, g.SL.library = g.SL.library, QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library,
                 dopt.SL.library = dopt.SL.library, metalearner = metalearner,
                 risk.type=risk.type, grid.size=grid.size, QAW = QAW)
  print(x)
  return(results)
}








#' @name performance_EYdopt
#' @aliases performance_EYdopt
#' @title Performance EYdopt
#' @description performance function for EYdopt
#'
#' @param x dummy
#' @param n n
#' @param DGP_fun DGP_fun
#' @param QAW QAW
#' @param g.SL.library library for g
#' @param QAW.SL.library library for QAW
#' @param blip.SL.library library for blip
#' @param VFolds CV Folds
#'
#' @return
#'
#' @export
#'
# EYdopt performance function
performance_EYdopt = function(x, n, DGP_fun, QAW, QAW.SL.library, blip.SL.library, grid.size, contrast = NULL){

  ObsData = subset(DGP_fun(n), select = -c(A_star, Y_star))
  W = subset(ObsData, select = -c(A, Y))
  V = W
  A = ObsData$A
  Y = ObsData$Y
  risk.type = "CV TMLE"
  kappa = NULL
  metalearner = "blip"
  VFolds = 10
  g.SL.library = "SL.mean"

  results = EYdopt(V=V, W=W, A=A, Y=Y, g.SL.library = g.SL.library, QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library, dopt.SL.library = dopt.SL.library,
                   metalearner = metalearner, risk.type=risk.type, grid.size=grid.size, VFolds=VFolds, QAW = QAW, contrast = contrast)
  print(x)
  return(results)
}







#' @name performance_EYgstar
#' @aliases performance_EYgstar
#' @title Performance EYgstar
#' @description performance function for EYgstar
#'
#' @param x dummy
#' @param risk.type risk.type
#' @param cs_to_try cs_to_try
#' @param alphas_to_try alphas_to_try
#' @param VFolds CV Folds
#'
#' @return
#'
#' @export
#'
# EYgstar performance function
performance_EYgstar = function(x, n, risk.type, cs_to_try, alphas_to_try, QAW.SL.library, blip.SL.library){

  QAW = QAW_null
  DGP_fun = DGP_null
  ObsData = subset(DGP_fun(441), select = -c(A_star, Y_star))
  W = subset(ObsData, select = -c(A, Y))
  V = W
  A = ObsData$A
  Y = ObsData$Y
  kappa = NULL
  metalearner = "discrete"
  VFolds = 10
  g.SL.library = "SL.mean"
  grid.size = 10
  contrast = NULL

  results = EYgstar(V=V, W=W, A=A, Y=Y, g.SL.library = g.SL.library, QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library,
                   metalearner = metalearner, risk.type=risk.type, grid.size=grid.size, VFolds=VFolds, QAW = QAW, contrast = contrast, cs_to_try = cs_to_try, alphas_to_try = alphas_to_try)
  print(x)
  return(results)
}

