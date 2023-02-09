#' @name QAW_null
#' @aliases QAW_null
#' @title Simulate with null
#' @description Generate QAW according to null
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'

QAW_null = function(A, W) {

  W1 = W$W1
  W2 = W$W2
  W3 = W$W3
  W4 = W$W4

  return(plogis(W1 + W4 + 0.01*A))
  #return(plogis(W1 - W2 - W4 + 0.0001*A*(1 + W4 + W2*abs(W3) + W1^2)))
  #return(plogis(W2 + W1))

}



#' @name DGP_null
#' @aliases DGP_null
#' @title Simulate with null
#' @description Generate data according to null
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_null
#'
#' @export
#'

DGP_null = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)
  W5 = rbinom(n, 1, .5)
  W6 = rbinom(n, 1, .5)
  W7 = rnorm(n, sd =20)
  W8 = rnorm(n, sd =20)
  W9 = rnorm(n, sd =20)
  W10 = rnorm(n, sd =20)
  W = data.frame(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10)

  A = rbinom(n, size = 1, prob = 0.5)
  #A = rbinom(n, size = 1, prob = plogis(W1 + W2))

  u = runif(n)
  Y = as.numeric(u<QAW_null(A,W))

  # Blip function
  QAW1 = QAW_null(A = 1, W)
  QAW0 = QAW_null(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip <= 0)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_null(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}



#' @name DGP_null_obs
#' @aliases DGP_null_obs
#' @title Simulate with null
#' @description Generate data according to null with obs g
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_null_obs
#'
#' @export
#'

DGP_null_obs = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)
  W5 = rbinom(n, 1, .5)
  W6 = rbinom(n, 1, .5)
  W7 = rnorm(n, sd =20)
  W8 = rnorm(n, sd =20)
  W9 = rnorm(n, sd =20)
  W10 = rnorm(n, sd =20)
  W = data.frame(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10)

  #A = rbinom(n, size = 1, prob = 0.5)
  A = rbinom(n, size = 1, prob = plogis(W1 + W2))

  u = runif(n)
  Y = as.numeric(u<QAW_null(A,W))

  # Blip function
  QAW1 = QAW_null(A = 1, W)
  QAW0 = QAW_null(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip <= 0)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_null(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}



#' @name QAW_eff
#' @aliases QAW_eff
#' @title Simulate with eff
#' @description Generate QAW according to eff
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'

QAW_eff = function(A, W) {

  W1 = W$W1
  W2 = W$W2
  W3 = W$W3
  W4 = W$W4

  return(plogis(W1 + 0.01*A + 5*W1*A))
  #return(plogis(W1 + 0.1*A + W1*A))

}



#' @name DGP_eff
#' @aliases DGP_eff
#' @title Simulate with eff
#' @description Generate data according to eff
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_eff
#'
#' @export
#'

DGP_eff = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)
  W5 = rbinom(n, 1, .5)
  W6 = rbinom(n, 1, .5)
  W7 = rnorm(n, sd =20)
  W8 = rnorm(n, sd =20)
  W9 = rnorm(n, sd =20)
  W10 = rnorm(n, sd =20)
  W = data.frame(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10)

  A = rbinom(n, size = 1, prob = 0.5)
  #A = rbinom(n, size = 1, prob = plogis(W1 + W2))

  u = runif(n)
  Y = as.numeric(u<QAW_eff(A,W))

  # Blip function
  QAW1 = QAW_eff(A = 1, W)
  QAW0 = QAW_eff(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip <= 0)
    }

  # Outcome
  Y_star = as.numeric(u<QAW_eff(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}



#' @name DGP_eff_obs
#' @aliases DGP_eff_obs
#' @title Simulate with eff
#' @description Generate data according to eff with obs g
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_eff_obs
#'
#' @export
#'

DGP_eff_obs = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)
  W5 = rbinom(n, 1, .5)
  W6 = rbinom(n, 1, .5)
  W7 = rnorm(n, sd =20)
  W8 = rnorm(n, sd =20)
  W9 = rnorm(n, sd =20)
  W10 = rnorm(n, sd =20)
  W = data.frame(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10)

  #A = rbinom(n, size = 1, prob = 0.5)
  A = rbinom(n, size = 1, prob = plogis(W1 + W2))

  u = runif(n)
  Y = as.numeric(u<QAW_eff(A,W))

  # Blip function
  QAW1 = QAW_eff(A = 1, W)
  QAW0 = QAW_eff(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip <= 0)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_eff(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}



#' @name QAW_bin_complex
#' @aliases QAW_bin_complex
#' @title Simulate with AL bin DGP
#' @description Generate QAW according to AL bin DGP
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'


###############################################
### AL DGP binary outcome #####################
###############################################
# QAW
QAW_bin_complex = function(A, W) {

  W1 = W$W1
  W2 = W$W2
  W3 = W$W3
  W4 = W$W4

  return(0.5*plogis(1-W1^2 + 3*W2 + 5*W3^2*A - 4.45*A)+0.5*plogis(-0.5- W3 + 2*W1*W2 + 3*abs(W2)*A - 1.5*A))

}



#' @name DGP_bin_complex
#' @aliases DGP_bin_complex
#' @title Simulate with AL bin DGP
#' @description Generate data according to AL bin DGP
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#' @param kappa kappa
#' @param QAW.fun QAW.fun
#'
#' @return data for DGP_bin_complex
#'
#' @export
#'

DGP_bin_complex = function(n, dA = NULL, a = NULL, kappa = NULL, QAW.fun = QAW_bin_complex){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)

  A = rbinom(n, size = 1, prob = 0.5)

  W = data.frame(W1, W2, W3, W4)

  u = runif(n)
  Y = as.numeric(u<QAW_bin_complex(A,W))

  # Blip function
  QAW1 = QAW_bin_complex(A = 1, W)
  QAW0 = QAW_bin_complex(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA) & is.null(kappa)) {
    A_star = A
  } else if (!is.null(a) & is.null(dA) & is.null(kappa)){
    A_star = a
  } else if (!is.null(kappa) & is.null(dA) & is.null(a)) {
      rc.out = dopt.fun(blip = blip, kappa = kappa)
      A_star = rbinom(n, 1, prob = rc.out$Prd.is.1)
  } else if (!is.null(dA) & is.null(a) & is.null(kappa) & dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (!is.null(dA) & is.null(a) & is.null(kappa) & dA == "ODTR"){
    A_star = as.numeric(blip > 0)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_bin_complex(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}




#' @name QAW_AL_RC
#' @aliases QAW_AL_RC
#' @title Simulate with AL bin DGP RC
#' @description Generate QAW according to AL bin DGP RC
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'
###############################################
### AL DGP binary outcome2 (RC) #####################
###############################################
# QAW
QAW_AL_RC = function(A, W) {

  Wtilde = W+5/6
  QAW = rep(NA, times = length(A))
  QAW[A == 1 & W <= 1/3 & W >= -1/2] = 0
  QAW[A == 1 & W < -1/2] = (-Wtilde^3 + Wtilde^2 - (1/3)*Wtilde + 1/27)[A == 1 & W < -1/2]
  QAW[A == 1 & W > 1/3] = (-W^3 + W^2 - (1/3)*W + 1/27)[A == 1 & W > 1/3]
  QAW[is.na(QAW)] = -(3/10)

  QAW = QAW + (6/10)

  return(QAW)

}



#' @name DGP_AL_RC
#' @aliases DGP_AL_RC
#' @title Simulate with AL bin DGP RC
#' @description Generate data according to AL bin DGP RC
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#' @param kappa kappa
#' @param QAW.fun QAW.fun
#'
#' @return data for DGP_AL_RC
#'
#' @export
#'

DGP_AL_RC = function(n, dA = NULL, a = NULL, kappa = NULL, QAW.fun = QAW_AL_RC){

  # Covariates
  W = runif(n)
  A = rbinom(n, size = 1, prob = 0.5)

  u = runif(n)
  Y = rbinom(n, 1, p = QAW_AL_RC(A,W))

  # Blip function
  QAW1 = QAW_AL_RC(A = 1, W = W)
  QAW0 = QAW_AL_RC(A = 0, W = W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA) & is.null(kappa)) {
    A_star = A
  } else if (!is.null(a) & is.null(dA) & is.null(kappa)){
    A_star = a
  } else if (!is.null(kappa) & is.null(dA) & is.null(a)) {
    rc.out = dopt.fun(blip = blip, kappa = kappa)
    A_star = rbinom(n, 1, prob = rc.out$Prd.is.1)
  } else if (!is.null(dA) & is.null(a) & is.null(kappa) & dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (!is.null(dA) & is.null(a) & is.null(kappa) & dA == "ODTR"){
    A_star = as.numeric(blip > 0)
  }

  # Outcome
  Y_star = rbinom(n, 1, p = QAW_AL_RC(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}


#' @name DGP_bin_complex_obs
#' @aliases DGP_bin_complex_obs
#' @title Simulate with AL bin DGP
#' @description Generate data according to AL bin DGP obs g
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_bin_complex_obs
#'
#' @export
#'

DGP_bin_complex_obs = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)

  #A = rbinom(n, size = 1, prob = 0.5)
  A = rbinom(n, size = 1, prob = plogis(W1 + W2))

  W = data.frame(W1, W2, W3, W4)

  u = runif(n)
  Y = as.numeric(u<QAW_bin_complex(A,W))

  # Blip function
  QAW1 = QAW_bin_complex(A = 1, W)
  QAW0 = QAW_bin_complex(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip > 0)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_bin_complex(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}




#' @name DGP_bin_complex_min
#' @aliases DGP_bin_complex_min
#' @title Simulate with AL bin DGP
#' @description Generate data according to AL bin DGP - want lower outcomes
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_bin_complex_min
#'
#' @export
#'

DGP_bin_complex_min = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)

  A = rbinom(n, size = 1, prob = 0.5)

  W = data.frame(W1, W2, W3, W4)

  u = runif(n)
  Y = as.numeric(u<QAW_bin_complex(A,W))

  # Blip function
  QAW1 = QAW_bin_complex(A = 1, W)
  QAW0 = QAW_bin_complex(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip < 0)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_bin_complex(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}




#' @name QAW_bin_dep
#' @aliases QAW_bin_dep
#' @title Simulate with AL bin DGP dep W
#' @description Generate QAW according to AL bin dep W
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'

# QAW_bin_dep
QAW_bin_dep = function(A, W) {

  W1 = W$W1
  W2 = W$W2
  W3 = W$W3
  W4 = W$W4

  return(0.5*plogis(1-W1^2 + 3*W2 + 5*W3^2*A - 4.45*A)+0.5*plogis(-0.5- W3 + 2*W1*W2 + 3*abs(W2)*A - 1.5*A))

}



#' @name DGP_bin_dep
#' @aliases DGP_bin_dep
#' @title Simulate with AL bin DGP with influential variable
#' @description Generate data according to AL bin DGP with influential variable
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_bin_dep
#'
#' @export
#'

DGP_bin_dep = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  Sigma = matrix(c(1,.3,.7,.3,1,.8,.7,.8,1), ncol=3)
  W234 = mvrnorm(n = n, mu = c(0,0,0), Sigma, tol = 1e-06, empirical = FALSE)

  A = rbinom(n, size = 1, prob = 0.5)

  W = data.frame(W1, W234)
  colnames(W) = c("W1", "W2", "W3", "W4")

  u = runif(n)
  Y = as.numeric(u<QAW_bin_dep(A,W))

  # Blip function
  QAW1 = QAW_bin_dep(A = 1, W)
  QAW0 = QAW_bin_dep(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip > 0)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_bin_dep(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}



#' @name QAW_bin_simple
#' @aliases QAW_bin_simple
#' @title Simulate with AL bin DGP simple
#' @description Generate QAW according to AL bin simple
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'


###############################################
### AL DGP binary outcome #####################
###############################################
# QAW_bin_simple
QAW_bin_simple = function(A, W) {

  W1 = W$W1
  W2 = W$W2
  W3 = W$W3
  W4 = W$W4

  return(plogis(W1 + 0.1*A + W1*A))

}



#' @name DGP_bin_simple
#' @aliases DGP_bin_simple
#' @title Simulate with AL bin DGP with influential variable
#' @description Generate data according to AL bin DGP with influential variable
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_bin_simple
#'
#' @export
#'

DGP_bin_simple = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)

  A = rbinom(n, size = 1, prob = 0.5)

  W = data.frame(W1, W2, W3, W4)

  u = runif(n)
  Y = as.numeric(u<QAW_bin_simple(A,W))

  # Blip function
  QAW1 = QAW_bin_simple(A = 1, W)
  QAW0 = QAW_bin_simple(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip > 0)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_bin_simple(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}


#' @name DGP_bin_simple_obs
#' @aliases DGP_bin_simple_obs
#' @title Simulate with AL bin DGP with influential variable
#' @description Generate data according to AL bin DGP with influential variable obs g
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_bin_simple_obs
#'
#' @export
#'

DGP_bin_simple_obs = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)

  #A = rbinom(n, size = 1, prob = 0.5)
  A = rbinom(n, size = 1, prob = plogis(W1 + W2))

  W = data.frame(W1, W2, W3, W4)

  u = runif(n)
  Y = as.numeric(u<QAW_bin_simple(A,W))

  # Blip function
  QAW1 = QAW_bin_simple(A = 1, W)
  QAW0 = QAW_bin_simple(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip > 0)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_bin_simple(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}















#' @name QAW_bin6
#' @aliases QAW_bin6
#' @title Simulate with AL bin DGP6 treat all optimal
#' @description Generate QAW according to AL bin DGP6 treat all optimal
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'
# QAW6
QAW_bin6 = function(A, W) {

  W1 = W$W1
  W2 = W$W2
  W3 = W$W3
  W4 = W$W4
  W5 = W$W5

  return(plogis(W1 + W2*W3 - 5*A + 0.5*A*W1 + A*W2*W5))

}

#' @name DGP_bin6
#' @aliases DGP_bin6
#' @title Simulate with AL bin DGP6 with treat all optimal
#' @description Generate data according to AL bin DGP6 with treat all optimal
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_bin6
#'
#' @export
#'
DGP_bin6 = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)
  W5 = rbinom(n, 1, .5)

  w = (W5 - .5)*4 + 0.5*(W1 + W2 + W3 + W4) # make this bimodal and symmetric about 0, with extremes around -6 and 6
  g1 = plogis(w) # make this symmetrical, and most density at extremes (positivity is here)
  A = rbinom(n, size = 1, prob = g1) # make sure marginal prob is .5

  W = data.frame(W1, W2, W3, W4, W5)

  u = runif(n)
  Y = as.numeric(u<QAW_bin6(A,W))

  # Blip function
  QAW1 = QAW_bin6(A = 1, W)
  QAW0 = QAW_bin6(A = 0, W)
  blip = QAW1 - QAW0
  mean(blip>0) # make this 1
  d0 = as.numeric(blip <0)
  A == d0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip > 0)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_bin6(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}


















#' @name QAW_bin_complex_3tx
#' @aliases QAW_bin_complex_3tx
#' @title Simulate with AL bin DGP
#' @description Generate QAW according to AL bin DGP
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'


###############################################
### AL DGP binary outcome #####################
###############################################
# QAW
QAW_bin_complex_3tx = function(A, W) {

  W1 = W$W1
  W2 = W$W2
  W3 = W$W3
  W4 = W$W4

  return(0.5*plogis(1-W1^2 + 3*W2 + 5*W3^2*A - 4.45*A)+0.5*plogis(-0.5- W3 + 2*W1*W2 + 3*abs(W2)*A - 1.5*A))

}



#' @name DGP_bin_complex_3tx
#' @aliases DGP_bin_complex_3tx
#' @title Simulate with AL bin DGP
#' @description Generate data according to AL bin DGP
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_bin_complex_3tx
#'
#' @export
#'

DGP_bin_complex_3tx = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)

  A = replicate(n, sample(0:2,1))
  W = data.frame(W1, W2, W3, W4)

  u = runif(n)
  Y = as.numeric(u<QAW_bin_complex_3tx(A,W))

  QAW = data.frame(QAW1 = QAW_bin_complex_3tx(A = 1, W),
                   QAW2 = QAW_bin_complex_3tx(A = 2, W),
                   QAW3 = QAW_bin_complex_3tx(A = 3, W))

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = apply(QAW, 1, which.max)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_bin_complex_3tx(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}







#' @name QAW_cont
#' @aliases QAW_cont
#' @title Simulate with DB DGP
#' @description Generate QAW according to DB DGP
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'
QAW_cont = function(A, W) {

  W1 = W$W1
  W2 = W$W2
  W3 = W$W3
  W4 = W$W4

  return(W1/10 - 0.3*A*W1^2 + 0.25*W2 + 0.5*A*W2 - 0.5*W3*W1*A + 0.2*W4^2/5 - 0.1*W4 + 2*A)

}

#' @name DGP_cont
#' @aliases DGP_cont
#' @title Simulate with DB DGP
#' @description Generate data according to DB DGP
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#'
#' @return data for DGP_cont
#'
#' @export
#'
#'
DGP_cont = function(n, dA = NULL, a = NULL){

  # Covariates
  W1 <- runif(n,-4,4)
  W2 <- runif(n,-4,4)
  A <- rbinom(n, 1, 0.5)
  W3 <- rnorm(n)
  W4 <- rgamma(n, 2, 1)

  W = data.frame(W1, W2, W3, W4)

  z = rnorm(n)
  Y = QAW_cont(A,W)+z

  # Blip function
  QAW1 = QAW_cont(A = 1, W)
  QAW0 = QAW_cont(A = 0, W)
  blip = QAW1 - QAW0

  # Treatment under rule
  if (!is.null(dA) & !is.null(a)){
    stop("Can only have dA or a")
  } else if (is.null(a) & is.null(dA)) {
    A_star = A
  } else if (!is.null(a)){
    A_star = a
  } else if (dA == "simple dynamic") {
    A_star = ifelse(W2 > 0, 1, 0)
  } else if (dA == "ODTR"){
    A_star = as.numeric(blip > 0)
  }

  # Outcome
  Y_star = QAW_cont(A_star,W)+z

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}









#' @name QAW.rc.allpos
#' @aliases QAW.rc.allpos
#' @title QAW RC all pos
#' @description Generate QAW according to DGP RC - all positive
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'
QAW.rc.allpos = function(A, W) {A + W + A*(W+10)}

#' @name QAW.rc.somepos
#' @aliases QAW.rc.somepos
#' @title QAW RC some pos
#' @description Generate QAW according to DGP RC - some positive
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'
QAW.rc.somepos = function(A, W) {A + W + (-2)*A*W}

#' @name QAW.rc.nopos
#' @aliases QAW.rc.nopos
#' @title QAW RC no pos
#' @description Generate QAW according to DGP RC - no positive
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return conditional mean of Y given A and W
#'
#' @export
#'
QAW.rc.nopos = function(A, W) {A - W + A*(W-10)}




#' @name DGP.rc.contW
#' @aliases DGP.rc.contW
#' @title DGP.rc.contW
#' @description Generate data according to DGP RC - cont W
#'
#' @param n n
#' @param a static txt
#' @param kappa prop can be treated in population
#' @param QAW.fun QAW function
#'
#' @return data for DGP.rc.contW
#'
#' @export
#'
#'
DGP.rc.contW = function(n, a = NULL, kappa = NULL, QAW.fun){

  W = rnorm(n)
  A = rbinom(n, 1, 0.5)
  U = rnorm(n)
  Y = U + QAW.fun(A, W)
  blip = QAW.fun(A = 1, W = W) - QAW.fun(A = 0,W = W)

  # Treatment under rule
  if (is.null(kappa) & is.null(a)){
    toreturn = data.frame(W = W, A = A, Y = Y)
  } else if (is.null(a) & !is.null(kappa)) {
    rc.out = dopt.fun(blip = blip, kappa = kappa)
    A_star = rbinom(n, 1, prob = rc.out$Prd.is.1)
    Y_star = U + QAW.fun(A_star, W)
    toreturn = data.frame(W = W, A = A, Y = Y, A_star = A_star, Y = Y_star, Prd.is.1 = rc.out$Prd.is.1, tauP = rc.out$tauP)
  } else if (!is.null(a) & is.null(kappa)) {
    A_star = a
    Y_star = U + QAW.fun(A_star, W)
    toreturn = data.frame(W = W, A = A, Y = Y, A_star = A_star, Y = Y_star)
  }

  return(toreturn)

}






#' @name DGP.rc.discreteW
#' @aliases DGP.rc.discreteW
#' @title DGP.rc.discreteW
#' @description Generate data according to DGP RC - discrete W
#'
#' @param n n
#' @param a static txt
#' @param kappa prop can be treated in population
#' @param QAW.fun QAW function
#'
#' @return data for DGP.rc.discreteW
#'
#' @export
#'
#'
DGP.rc.discreteW = function(n, a = NULL, kappa = NULL, QAW.fun){

  W = rbinom(n, 1, 0.5)
  A = rbinom(n, 1, 0.5)
  U = rnorm(n)
  Y = U + QAW.fun(A, W)
  blip = QAW.fun(A = 1, W = W) - QAW.fun(A = 0,W = W)

  # Treatment under rule
  if (is.null(kappa) & is.null(a)){
    toreturn = data.frame(W = W, A = A, Y = Y)
  } else if (is.null(a) & !is.null(kappa)) {
    rc.out = dopt.fun(blip = blip, kappa = kappa)
    A_star = rbinom(n, 1, prob = rc.out$Prd.is.1)
    Y_star = U + QAW.fun(A_star, W)
    toreturn = data.frame(W = W, A = A, Y = Y, A_star = A_star, Y = Y_star, Prd.is.1 = rc.out$Prd.is.1, tauP = rc.out$tauP)
  } else if (!is.null(a) & is.null(kappa)) {
    A_star = a
    Y_star = U + QAW.fun(A_star, W)
    toreturn = data.frame(W = W, A = A, Y = Y, A_star = A_star, Y = Y_star)
  }

  return(toreturn)

}
