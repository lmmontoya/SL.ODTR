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
#' @return
#'
#' @export
#'
SL.QAW.HTEepi = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  Wnames = colnames(X)[grep(pattern = "W", colnames(X))]

  fit.glm <- glm(as.formula(paste("Y ~ ", paste(Wnames, collapse= "+"), "+ A + A*(", paste(Wnames, collapse= "+"), ")"))
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
#' @return
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
#' @return
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
#' @return
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
#' @return
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
#' @return
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
#' @return
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
#' @return
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
#' @return
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
#' @return
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
#' @return
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
#' @return
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

