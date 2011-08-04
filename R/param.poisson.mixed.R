#' Extract Samples from a Distribution in Order to Pass Them to the `qi'
#' Function (this is primarily a helper function for the logit.mixed model)
#' @S3method param poisson.mixed
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... ignored parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary parameters
#' @author Matt Owen and Ferdinand Alimadhi and Delia
param.poisson.mixed <- function(obj, num=1000, ...) {
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
  betas <- mvrnorm(num, fixef(obj), vcov(obj))
  scale <- sigma(obj)

  list(
       coef = betas,
       alpha = list(scale = scale, gammas = gammas),
       fam = zelig$zc$family
       )
}
