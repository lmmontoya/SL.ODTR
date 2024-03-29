#' @name SL.QAW.HTEepi
#' @aliases SL.QAW.HTEepi
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
#' @return SL.QAW.HTEepi
#'
#' @export
#'
SL.QAW.HTEepi = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  Wnames = colnames(X)[grep(pattern = "W", colnames(X))]

  fit.glm <- glm(as.formula(paste("Y ~ ", "A*(", paste(Wnames, collapse= "+"), ")")),
                 data = X, family = family, weights = obsWeights,
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



#' @name SL.blip.HTEepi
#' @aliases SL.blip.HTEepi
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
#' @return SL.blip.HTEepi
#'
#' @export
#'
SL.blip.HTEepi = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  Wnames = colnames(X)[grep(pattern = "W", colnames(X))]

  fit.glm <- glm(as.formula(paste("Y ~ ", paste(Wnames, collapse= "+")))
                 , data = X, family = family, weights = obsWeights,
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



#' @name SL.blip.W1
#' @aliases SL.blip.W1
#' @title Blip W1
#' @description Blip W1.
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
#' @return SL.blip.W1
#'
#' @export
#'
SL.blip.W1 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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



#' @name SL.blip.W2
#' @aliases SL.blip.W2
#' @title Blip W2
#' @description Blip W2.
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
#' @return SL.blip.W2
#'
#' @export
#'
SL.blip.W2 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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


#' @name SL.blip.W3
#' @aliases SL.blip.W3
#' @title Blip W3
#' @description Blip W3.
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
#' @return SL.blip.W3
#'
#' @export
#'
SL.blip.W3 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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


#' @name SL.blip.W4
#' @aliases SL.blip.W4
#' @title Blip W4
#' @description Blip W4.
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
#' @return SL.blip.W4
#'
#' @export
#'
SL.blip.W4 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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


#' @name SL.blip.W5
#' @aliases SL.blip.W5
#' @title Blip W5
#' @description Blip W5.
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
#' @return SL.blip.W5
#'
#' @export
#'
SL.blip.W5 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ W5, data = X, family = family, weights = obsWeights,
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






#' @name SL.QAW.W1
#' @aliases SL.QAW.W1
#' @title QAW for W1
#' @description QAW for W1.
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
#' @return SL.QAW.W1
#'
#' @export
#'
SL.QAW.W1 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ W1*A, data = X, family = family, weights = obsWeights,
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



#' @name SL.QAW.W2
#' @aliases SL.QAW.W2
#' @title QAW for W2
#' @description QAW for W2.
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
#' @return SL.QAW.W2
#'
#' @export
#'
SL.QAW.W2 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ A*W2, data = X, family = family, weights = obsWeights,
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


#' @name SL.QAW.W3
#' @aliases SL.QAW.W3
#' @title QAW for W3
#' @description QAW for W3.
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
#' @return SL.QAW.W3
#'
#' @export
#'
SL.QAW.W3 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ A*W3, data = X, family = family, weights = obsWeights,
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


#' @name SL.QAW.W4
#' @aliases SL.QAW.W4
#' @title QAW for W5
#' @description QAW for W5.
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
#' @return SL.QAW.W4
#'
#' @export
#'
#SL.QAW.W4
#correctly specified param model QAW
SL.QAW.W4 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ A*W4, data = X, family = family, weights = obsWeights,
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



#' @name SL.QAW.W5
#' @aliases SL.QAW.W5
#' @title QAW for W5
#' @description QAW for W5.
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
#' @return SL.QAW.W5
#'
#' @export
#'
SL.QAW.W5 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ A*W5, data = X, family = family, weights = obsWeights,
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
#' @title QAW for cont. dist
#' @description QAW for cont. dist
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
#' @return SL.QAW.correct_cont
#'
#' @export
#'
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
#' @title blip for cont. dist
#' @description blip for cont. dist
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
#' @return SL.blip.correct_cont
#'
#' @export
#'
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




#' @name SL.HAL1
#' @aliases SL.HAL1
#' @title HAL1
#' @description HAL1
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
#' @return SL.HAL1
#'
#' @export
#'
SL.HAL1 = function (Y, X, newX = NULL, max_degree = 1, fit_type = c("glmnet", "lassi"),
                        n_folds = 10, use_min = TRUE, family,
                        obsWeights = rep(1, length(Y)), ...)
{
  if (!is.matrix(X)) {
    X_in <- as.matrix(X)
  }
  else {
    X_in <- X
  }
  if (!is.null(newX) & !is.matrix(newX)) {
    newX_in <- as.matrix(newX)
  }
  else {
    newX_in <- newX
  }
  if (family$family == "gaussian") {
    hal_out <- fit_hal(Y = Y, X = X_in, max_degree = max_degree,
                       fit_type = fit_type, n_folds = n_folds, use_min = use_min,
                       family = "gaussian", weights = obsWeights, yolo = FALSE)
  }
  if (family$family == "binomial") {
    hal_out <- fit_hal(Y = Y, X = X_in, max_degree = max_degree,
                       fit_type = fit_type, n_folds = n_folds, use_min = use_min,
                       family = "binomial", weights = obsWeights, yolo = FALSE)
  }
  if (!is.null(newX)) {
    pred <- stats::predict(hal_out, new_data = newX_in)
  }
  else {
    pred <- stats::predict(hal_out, new_data = X_in)
  }
  fit <- list(object = hal_out)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- "SL.hal9001"
  return(out)
}


#' @name SL.HAL2
#' @aliases SL.HAL2
#' @title HAL2
#' @description HAL2
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
#' @return SL.HAL2
#'
#' @export
#'
SL.HAL2 = function (Y, X, newX = NULL, max_degree = 2, fit_type = c("glmnet", "lassi"),
                        n_folds = 10, use_min = TRUE, family,
                        obsWeights = rep(1, length(Y)), ...)
{
  if (!is.matrix(X)) {
    X_in <- as.matrix(X)
  }
  else {
    X_in <- X
  }
  if (!is.null(newX) & !is.matrix(newX)) {
    newX_in <- as.matrix(newX)
  }
  else {
    newX_in <- newX
  }
  if (family$family == "gaussian") {
    hal_out <- fit_hal(Y = Y, X = X_in, max_degree = max_degree,
                       fit_type = fit_type, n_folds = n_folds, use_min = use_min,
                       family = "gaussian", weights = obsWeights, yolo = FALSE)
  }
  if (family$family == "binomial") {
    hal_out <- fit_hal(Y = Y, X = X_in, max_degree = max_degree,
                       fit_type = fit_type, n_folds = n_folds, use_min = use_min,
                       family = "binomial", weights = obsWeights, yolo = FALSE)
  }
  if (!is.null(newX)) {
    pred <- stats::predict(hal_out, new_data = newX_in)
  }
  else {
    pred <- stats::predict(hal_out, new_data = X_in)
  }
  fit <- list(object = hal_out)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- "SL.hal9001"
  return(out)
}



#' @name SL.hal9001
#' @aliases SL.hal9001
#' @title HAL with binning
#' @description HAL with binning
#'
#' @param Y Y
#' @param X X
#' @param newX newX
#' @param max_degree max_degree
#' @param fit_type fit_type
#' @param n_folds n_folds
#' @param family family
#' @param use_min use_min
#' @param obsWeights obsWeights
#' @param bins bins
#'
#' @return SL.hal9001
#'
#' @export
#'
SL.hal9001 <- function(Y,
                       X,
                       newX = NULL,
                       max_degree = 2,
                       fit_type = c("glmnet", "lassi"),
                       n_folds = 10,
                       use_min = TRUE,
                       family, #= stats::gaussian(),
                       obsWeights = rep(1, length(Y)),
                       bins = 75,
                       ...) {
  # create matrix version of X and newX for use with hal9001::fit_hal
  if (!is.matrix(X)) {
    X_in <- as.matrix(X)
  } else {
    X_in <- X
  }

  ## This function discretizes a matrix
  quantizer = function(X, bins) {
    if (is.null(bins)) {
      return(X)
    }
    X = as.matrix(X)

    convertColumn = function(x) {
      quants = seq(0, 0.97, length.out = bins)
      q = quantile(x, quants)

      nearest <- findInterval(x, q)
      x <- q[nearest]
      return(x)
    }
    quantizer = function(X) {
      as.matrix(apply(X, MARGIN = 2, FUN = convertColumn))
    }
    return(quantizer(X))
  }

  # Discretizing matrix into “bins” bins
  X_in <- quantizer(X_in, bins)


  if (!is.null(newX) & !is.matrix(newX)) {
    newX_in <- as.matrix(newX)
  } else {
    newX_in <- newX
  }



  if (family$family == "gaussian") {
    # fit HAL
    hal_out <- fit_hal(
      Y = Y, X = X_in, max_degree = max_degree, fit_type = fit_type,
      n_folds = n_folds, use_min = use_min, family = "gaussian",
      weights = obsWeights, yolo = FALSE
    )
  }

  if (family$family == "binomial") {
    # fit HAL with logistic regression
    hal_out <- fit_hal(
      Y = Y, X = X_in, max_degree = max_degree, fit_type = fit_type,
      n_folds = n_folds, use_min = use_min, family = "binomial",
      weights = obsWeights, yolo = FALSE
    )
  }

  # compute predictions based on `newX` or input `X`
  if (!is.null(newX)) {
    pred <- stats::predict(hal_out, new_data = newX_in)
  } else {
    pred <- stats::predict(hal_out, new_data = X_in)
  }

  # build output object
  fit <- list(object = hal_out)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- "SL.hal9001"
  return(out)
}
