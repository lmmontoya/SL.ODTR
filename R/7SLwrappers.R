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


#' @name SL.blip.incorrect1
#' @aliases SL.blip.incorrect1
#' @title Epi HTE GLM for blip 1
#' @description Standard HTE GLM used in Epi analyses for blip 1.
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
#SL.blip.incorrect1
#correctly specified param model blip
SL.blip.incorrect1 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ W1, data = X, family = family, weights = obsWeights,
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



#' @name SL.blip.incorrect2
#' @aliases SL.blip.incorrect2
#' @title Epi HTE GLM for blip 2
#' @description Standard HTE GLM used in Epi analyses for blip 2.
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
#SL.blip.incorrect2
#correctly specified param model blip
SL.blip.incorrect2 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ W2, data = X, family = family, weights = obsWeights,
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


#' @name SL.blip.incorrect3
#' @aliases SL.blip.incorrect3
#' @title Epi HTE GLM for blip 3
#' @description Standard HTE GLM used in Epi analyses for blip 3.
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
#SL.blip.incorrect3
#correctly specified param model blip
SL.blip.incorrect3 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ W3, data = X, family = family, weights = obsWeights,
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


#' @name SL.blip.incorrect4
#' @aliases SL.blip.incorrect4
#' @title Epi HTE GLM for blip 4
#' @description Standard HTE GLM used in Epi analyses for blip 4.
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
#SL.blip.incorrect4
#correctly specified param model blip
SL.blip.incorrect4 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ W4, data = X, family = family, weights = obsWeights,
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


#' @name SL.QAW.correct_cont
#' @aliases SL.QAW.correct_cont
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
#SL.QAW.correct_cont
#correctly specified param model QAW for David's DGP with continuous outcome
SL.QAW.correct_cont = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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





#' @name SL.blip.correct_cont
#' @aliases SL.blip.correct_cont
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

#SL.blip.correct_cont
#correctly specified param model blip for David's DGP with cont outcome
SL.blip.correct_cont = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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
