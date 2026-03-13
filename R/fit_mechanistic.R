nllik_normal <- function(mu, sigma, y) {
  sum(log(sigma) + ((y - mu) / sigma)^2)
}

optim_normal <- function(Dt, Rt, Ct, Pt, sc = 0.2, sp = 0.3) {
  nllik <- function(theta) {
    eta <- theta[1]
    sigma <- theta[2:4]
    nllik_normal(eta * Rt, sigma[1], Dt) +
      nllik_normal(eta * sc * Rt, sigma[2], Ct) +
      nllik_normal(eta * sp * Rt, sigma[3], Pt)
  }
  
  optim(par = c(1, 2, 2.5, 3), fn = nllik,
        method = "L-BFGS-B",
        lower = rep(1e-8, 4),
        control = list(maxit = 10000))
}

nllik_poisson <- function(theta, y) {
  -sum(y * log(theta) - theta)
}


optim_poisson <- function(Dt, Rt, Ct, Pt, sc = 0.2, sp = 0.3) {
  nllik <- function(theta) {
    eta <- theta[1]
    nllik_poisson(eta * Rt, Dt) +
    nllik_poisson(eta * sc * Rt, Ct) +
    nllik_poisson(eta * sp * Rt, Pt)
  }
    
  optim(par = c(1), fn = nllik, control = list(maxit = 10000), method = "Brent", lower = 0, upper = 10000)
}
nllik_negbinom <- function(mu, size, y) {
  -sum(lgamma(y + size) - lgamma(size) - lgamma(y + 1) +
       size * log(size / (size + mu)) + y * log(mu / (size + mu)))
}

optim_negbinom <- function(Dt, Rt, Ct, Pt, sc = 0.2, sp = 0.3) {
  nllik <- function(theta) {
    eta   <- theta[1]          # shared multiplier
    sizeD <- theta[2]          # dispersion for Dt
    sizeC <- theta[3]          # dispersion for Ct
    sizeP <- theta[4]          # dispersion for Pt

    nllik_negbinom(eta * Rt, sizeD, Dt) +
      nllik_negbinom(eta * sc * Rt, sizeC, Ct) +
      nllik_negbinom(eta * sp * Rt, sizeP, Pt)
  }

  ## enforce positivity for all four parameters
  lower_bounds <- rep(1e-8, 4)

  optim(par = c(1, 2, 2.5, 3), fn = nllik,
        method = "L-BFGS-B",
        lower = lower_bounds,
        control = list(maxit = 10000))
}


#' Fit a mechanistic model to the data
#' 
#' @param Dt,Ct,Pt,Rt The data correspoding to DAD, CNISP, PTSOS, and RVDSS, respectively.
#' @param Rt_nowcast The RVDSS data for the nowcast period. This is used to create the nowcast predictions for the mechanistic model.
#' @param sc,sp The scaling factors for the CNISP and PTSOS data.
#' @param method Either "normal", "poisson", or "negbinom".
#'
#' @returns A list with the parameter estimates and the negative log-likelihood.
#' 
#' @examples
#' sim_poisson_data <- function(eta, Rt, sc = 0.2, sp = 0.3) {
#'   data.frame(
#'     Dt = rpois(length(Rt), eta * Rt),
#'     Ct = rpois(length(Rt), eta * sc * Rt),
#'     Pt = rpois(length(Rt), eta * sp * Rt),
#'     Rt = Rt
#'   )
#' }
#' d2 <- sim_poisson_data(eta = 5, Rt = rpois(100, 100))
#' fit_mechanistic(d2$Dt, d2$Ct, d2$Pt, d2$Rt, Rt_nowcast = rpois(10, 100), method = "poisson")
#' @export
fit_mechanistic <- function(Dt, Ct, Pt, Rt, Rt_nowcast, sc = 0.2, sp = 0.3, theta = c(1, 1.5, 0.5), method = "normal") {
  optim_res <- switch(method,
    "normal" = optim_normal(Dt, Rt, Ct, Pt, sc, sp),
    "poisson" = optim_poisson(Dt, Rt, Ct, Pt, sc, sp),
    "negbinom" = optim_negbinom(Dt, Rt, Ct, Pt, sc, sp)
  )
  if (optim_res$convergence != 0) warning("Model did not converge.")

  list(model = optim_res$par, predictions = optim_res$par[1] * Rt_nowcast)
}
