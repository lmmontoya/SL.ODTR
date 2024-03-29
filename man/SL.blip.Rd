% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3SLodtr.R
\name{SL.blip}
\alias{SL.blip}
\title{Blip-based SL}
\usage{
SL.blip(
  V,
  W,
  A,
  Y,
  ab,
  QAW.reg,
  g.reg,
  blip.SL.library,
  risk.type,
  grid.size,
  newV = NULL,
  VFolds,
  family,
  discrete.SL
)
}
\arguments{
\item{V}{subset covariates for designing ODTR}

\item{W}{covariates}

\item{A}{txt}

\item{Y}{outcome}

\item{ab}{range of Y}

\item{QAW.reg}{Q(A,W) regression object}

\item{g.reg}{g(A|W) regression object}

\item{blip.SL.library}{blip SL library}

\item{risk.type}{risk type}

\item{newV}{new V}

\item{VFolds}{number of folds}

\item{family}{family for outcome}

\item{discrete.SL}{whether discrete SL (choose one algorithm) or continuous SL (weighted combination of algorithms)}

\item{grid}{size}
}
\value{
SL blip object
}
\description{
SuperLearner for ODTR that returns estimated blips.
}
