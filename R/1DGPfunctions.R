#' @name QAW_bin_complex
#' @aliases QAW_bin_complex
#' @title Simulate with AL bin DGP
#' @description Generate QAW according to AL bin DGP
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return
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
#' @param kappa for resource constraints
#'
#' @return
#'
#' @export
#'

DGP_bin_complex = function(n, dA = NULL, a = NULL, kappa = NULL){

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
    A_star = as.numeric(blip > 0)
  } else if (dA == "ODTR-RC" & is.null(kappa)){
    stop("If you have dA as ODTR-RC you must specify a kappa")
  } else if (dA == "ODTR-RC"){
    tau = seq(from = min(blip), to = max(blip), length.out = 500) # let tau vary from min blip to max blip
    surv = sapply(tau, function(x) mean(blip > x)) #probability that the blip is greater than some varying tau
    nu = min(tau[which(surv <= kappa)]) #the biggest tau such that the survival prob is <= kappa
    tauP = max(c(nu, 0)) # max between nu and 0
    A_star = as.numeric(blip > tauP)
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
#' @param kappa for resource constraints
#'
#' @return
#'
#' @export
#'

DGP_bin_complex_min = function(n, dA = NULL, a = NULL, kappa = NULL){

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
  } else if (dA == "ODTR-RC" & is.null(kappa)){
    stop("If you have dA as ODTR-RC you must specify a kappa")
  } else if (dA == "ODTR-RC"){
    tau = seq(from = min(blip), to = max(blip), length.out = 500) # let tau vary from min blip to max blip
    surv = sapply(tau, function(x) mean(blip > x)) #probability that the blip is greater than some varying tau
    nu = min(tau[which(surv <= kappa)]) #the biggest tau such that the survival prob is <= kappa
    tauP = max(c(nu, 0)) # max between nu and 0
    A_star = as.numeric(blip > tauP)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_bin_complex(A_star,W))

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
#' @return
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
#' @param kappa for resource constraints
#'
#' @return
#'
#' @export
#'

DGP_bin_simple = function(n, dA = NULL, a = NULL, kappa = NULL){

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
  } else if (dA == "ODTR-RC" & is.null(kappa)){
    stop("If you have dA as ODTR-RC you must specify a kappa")
  } else if (dA == "ODTR-RC"){
    tau = seq(from = min(blip), to = max(blip), length.out = 500) # let tau vary from min blip to max blip
    surv = sapply(tau, function(x) mean(blip > x)) #probability that the blip is greater than some varying tau
    nu = min(tau[which(surv <= kappa)]) #the biggest tau such that the survival prob is <= kappa
    tauP = max(c(nu, 0)) # max between nu and 0
    A_star = as.numeric(blip > tauP)
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
#' @return
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

  return(plogis(W1 + W2*W3 + 5*A + 0.5*A*W1 + A*W2*W5))

}

#' @name DGP_bin6
#' @aliases DGP_bin6
#' @title Simulate with AL bin DGP6 with treat all optimal
#' @description Generate data according to AL bin DGP6 with treat all optimal
#'
#' @param n n
#' @param dA rule type
#' @param a static txt
#' @param kappa for resource constraints
#'
#' @return
#'
#' @export
#'
DGP_bin6 = function(n, dA = NULL, a = NULL, kappa = NULL){

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
 # mean(blip>0) # make this 1

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
  } else if (dA == "ODTR-RC" & is.null(kappa)){
    stop("If you have dA as ODTR-RC you must specify a kappa")
  } else if (dA == "ODTR-RC"){
    tau = seq(from = min(blip), to = max(blip), length.out = 500) # let tau vary from min blip to max blip
    surv = sapply(tau, function(x) mean(blip > x)) #probability that the blip is greater than some varying tau
    nu = min(tau[which(surv <= kappa)]) #the biggest tau such that the survival prob is <= kappa
    tauP = max(c(nu, 0)) # max between nu and 0
    A_star = as.numeric(blip > tauP)
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
#' @return
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
#' @param kappa for resource constraints
#'
#' @return
#'
#' @export
#'

DGP_bin_complex_3tx = function(n, dA = NULL, a = NULL, kappa = NULL){

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
  } else if (dA == "ODTR-RC" & is.null(kappa)){
    stop("If you have dA as ODTR-RC you must specify a kappa")
  } else if (dA == "ODTR-RC"){
    tau = seq(from = min(blip), to = max(blip), length.out = 500) # let tau vary from min blip to max blip
    surv = sapply(tau, function(x) mean(blip > x)) #probability that the blip is greater than some varying tau
    nu = min(tau[which(surv <= kappa)]) #the biggest tau such that the survival prob is <= kappa
    tauP = max(c(nu, 0)) # max between nu and 0
    A_star = as.numeric(blip > tauP)
  }

  # Outcome
  Y_star = as.numeric(u<QAW_bin_complex_3tx(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}
