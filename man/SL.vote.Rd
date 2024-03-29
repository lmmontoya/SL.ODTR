% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3SLodtr.R
\name{SL.vote}
\alias{SL.vote}
\title{Vote-based SL}
\usage{
SL.vote(
  V,
  W,
  A,
  Y,
  ab,
  QAW.reg,
  g.reg,
  blip.SL.library,
  dopt.SL.library,
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

\item{dopt.SL.library}{dopt SL library. Options: "DonV", "Qlearn", "OWL", "EARL", "optclass", "RWL", "treatall", "treatnone". Can also be "all".}

\item{risk.type}{risk type}

\item{newV}{newV}

\item{VFolds}{number of folds}

\item{family}{family for outcome}

\item{discrete.SL}{whether discrete SL (choose one algorithm) or continuous SL (weighted combination of algorithms)}

\item{grid}{size}
}
\value{
SL vote object
}
\description{
SuperLearner for ODTR that returns estimated txts under rule
}
