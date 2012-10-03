#' Extract Samples from a Distribution in Order to Pass Them to the `qi' Function
#' (this is primarily a helper function for the gamma.mixed model)
#' @S3method param gamma.mixed
#' @usage \method{param}{gamma.mixed}(obj, num=1000, ...)
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... ignored parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary 
#'         parameters
#' @export
param.gamma.mixed <- function(obj, num=1000, ...) {
  zelig <- obj
  # 
  obj <- GetObject(obj)

  # this is kludge, while functions are not being forwarded correctly
  sigma <- getMethod('sigma', 'mer')

  fixed <- fixef(obj)
  vars <- ranef(obj, postVars=TRUE)
  corr <- VarCorr(obj)
  gammas <- NULL

  for (key in names(vars)) {
    # Sigma makes sense, but why is mu the zero vector?
    Sigma <- corr[[key]]
    mu <- rep(0, ncol(Sigma))

    #
    gammas[[key]] <- mvrnorm(num, mu, Sigma)
  }

  names(gammas) <- names(vars)
  betas <- mvrnorm(num, fixef(.fitted), vcov(.fitted))
  scale <- sigma(.fitted)

  list(
       coef = betas,
       alpha = list(scale = scale, gammas = gammas),
       fam = eval(zelig$call$family, zelig$env)
       )
}
