#' @name QAW_AL_bin
#' @aliases QAW_AL_bin
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
QAW_AL_bin = function(A, W) {

  W1 = W$W1
  W2 = W$W2
  W3 = W$W3
  W4 = W$W4

  return(0.5*plogis(1-W1^2 + 3*W2 + 5*W3^2*A - 4.45*A)+0.5*plogis(-0.5- W3 + 2*W1*W2 + 3*abs(W2)*A - 1.5*A))

}



#' @name DGP_AL_bin
#' @aliases DGP_AL_bin
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

DGP_AL_bin = function(n, dA = NULL, a = NULL, kappa = NULL){

  # Covariates
  W1 = rnorm(n)
  W2 = rnorm(n)
  W3 = rnorm(n)
  W4 = rnorm(n)

  A = rbinom(n, size = 1, prob = 0.5)

  W = data.frame(W1, W2, W3, W4)

  u = runif(n)
  Y = as.numeric(u<QAW_AL_bin(A,W))

  # Blip function
  QAW1 = QAW_AL_bin(A = 1, W)
  QAW0 = QAW_AL_bin(A = 0, W)
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
  Y_star = as.numeric(u<QAW_AL_bin(A_star,W))

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}















#' @name QAW_smooth2
#' @aliases QAW_smooth2
#' @title Simulate with DB DGP
#' @description Generate QAW according to DB DGP
#'
#' @param W Data frame of observed baseline covariates
#' @param A Vector of treatment
#'
#' @return
#'
#' @export
#'

###############################################
#### DB smooth2 outcome #########################
###############################################
# QAW
QAW_smooth2 = function(A, W) {

  W1 = W$W1
  W2 = W$W2
  W3 = W$W3
  W4 = W$W4

  return(W1/10 - 0.3*A*W1^2 + 0.25*W2 + 0.5*A*W2 - 0.5*W3*W1*A + 0.2*W4^2/5 - 0.1*W4 + 2*A)

}

#' @name DGP_smooth2
#' @aliases DGP_smooth2
#' @title Simulate with DB DGP
#' @description Generate data according to DB DGP
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
#'
DGP_smooth2 = function(n, dA = NULL, a = NULL, kappa = NULL){

  # Covariates
  W1 <- runif(n,-4,4)
  W2 <- runif(n,-4,4)
  A <- rbinom(n, 1, 0.5)
  W3 <- rnorm(n)
  W4 <- rgamma(n, 2, 1)

  W = data.frame(W1, W2, W3, W4)

  z = rnorm(n)
  Y = QAW_smooth2(A,W)+z

  # Blip function
  QAW1 = QAW_smooth2(A = 1, W)
  QAW0 = QAW_smooth2(A = 0, W)
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
  Y_star = QAW_smooth2(A_star,W)+z

  # Data and target parameter
  O = data.frame(W, A, A_star, Y, Y_star)

  return(O)

}











