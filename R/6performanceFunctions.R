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
#' @param moMain_model moMain_model
#' @param moCont_model moCont_model
#' @param grid.size grid size
#'
#' @return
#'
#' @export
#'
# optimal dynamic regime performance function
# odtr_performance

performance_ODTR = function(x, n, risk.type, DGP_fun, QAW, QAW.SL.library, blip.SL.library, dopt.SL.library, metalearner, moMain_model, moCont_model, grid.size){

  ObsData = subset(DGP_fun(n), select = -c(A_star, Y_star))
  W = subset(ObsData, select = -c(A, Y))
  V = W
  A = ObsData$A
  Y = ObsData$Y
  grid.size = 1000
  VFolds = 10
  kappa = NULL

  results = odtr(V=V, W=W, A=A, Y=Y, QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library,
                 dopt.SL.library = dopt.SL.library, metalearner = metalearner,
                 risk.type=risk.type, grid.size=grid.size, VFolds=VFolds, QAW = QAW,
                 moMain_model = moMain_model, moCont_model = moCont_model)

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
#' @param QAW.SL.library library for QAW
#' @param blip.SL.library library for blip
#' @param VFolds CV Folds
#'
#' @return
#'
#' @export
#'
# EYdopt performance function
performance_EYdopt = function(x, n, DGP_fun, QAW, QAW.SL.library, blip.SL.library, VFolds){

  ObsData = subset(DGP_fun(n), select = -c(A_star, Y_star))
  W = subset(ObsData, select = -c(A, Y))
  V = W
  A = ObsData$A
  Y = ObsData$Y
  risk.type = "CV TMLE"
  kappa = NULL
  metalearner = "blip"
  moMain_model = NULL
  moCont_model = NULL
  grid.size = 1000

  results = EYdopt(V=V, W=W, A=A, Y=Y, QAW.SL.library = QAW.SL.library, blip.SL.library=blip.SL.library,
                   dopt.SL.library = dopt.SL.library, metalearner = metalearner,
                   risk.type=risk.type, grid.size=grid.size, VFolds=VFolds, QAW = QAW,
                   moMain_model = moMain_model, moCont_model = moCont_model)

  return(results)
}
