% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2helperFunctions.R
\name{getpreds.blip.fun}
\alias{getpreds.blip.fun}
\title{Predictions}
\usage{
getpreds.blip.fun(
  Y,
  X,
  newX = NULL,
  family = gaussian(),
  SL.library,
  id = NULL,
  verbose = FALSE,
  control = list(),
  obsWeights = NULL,
  env = parent.frame()
)
}
\arguments{
\item{Y}{outcome}

\item{X}{predictors}

\item{newX}{new X}

\item{family}{family}

\item{SL.library}{SL library}

\item{id}{for unique ids}

\item{verbose}{verbose}

\item{control}{control}

\item{obsWeights}{obsWeights}

\item{env}{env}
}
\value{
predicted blips
}
\description{
Get library predictions (what would be library.predict) for blip. Code mostly from SuperLearner.
}
