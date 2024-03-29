% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/7SLwrappers.R
\name{SL.HAL1}
\alias{SL.HAL1}
\title{HAL1}
\usage{
SL.HAL1(
  Y,
  X,
  newX = NULL,
  max_degree = 1,
  fit_type = c("glmnet", "lassi"),
  n_folds = 10,
  use_min = TRUE,
  family,
  obsWeights = rep(1, length(Y)),
  ...
)
}
\arguments{
\item{Y}{outcome}

\item{X}{predictors}

\item{newX}{new X}

\item{family}{family}

\item{obsWeights}{obsWeights}

\item{...}{other}

\item{SL.library}{SL library}

\item{model}{model}
}
\value{
SL.HAL1
}
\description{
HAL1
}
