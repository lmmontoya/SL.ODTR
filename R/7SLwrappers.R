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
SL.QAW.incorrect = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  Wnames = colnames(X)[grep(pattern = "W", colnames(X))]

  fit.glm <- glm(as.formula(paste("Y ~ ", paste(Wnames, collapse= "+"), "+ A*(", paste(Wnames, collapse= "+"), ")"))
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



#' @name SL.blip.incorrect1
#' @aliases SL.blip.incorrect1
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


#' @name SL.blip.incorrect5
#' @aliases SL.blip.incorrect5
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
SL.blip.incorrect5 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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






#' @name SL.QAW.incorrect1
#' @aliases SL.QAW.incorrect1
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
SL.QAW.incorrect1 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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



#' @name SL.QAW.incorrect2
#' @aliases SL.QAW.incorrect2
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
SL.QAW.incorrect2 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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


#' @name SL.QAW.incorrect3
#' @aliases SL.QAW.incorrect3
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
SL.QAW.incorrect3 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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


#' @name SL.QAW.incorrect4
#' @aliases SL.QAW.incorrect4
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
#SL.QAW.incorrect4
#correctly specified param model QAW
SL.QAW.incorrect4 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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



#' @name SL.QAW.incorrect5
#' @aliases SL.QAW.incorrect5
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
#SL.QAW.incorrect5
#correctly specified param model QAW
SL.QAW.incorrect5 = function (Y, X, newX, family, obsWeights, model = TRUE, ...) {
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
