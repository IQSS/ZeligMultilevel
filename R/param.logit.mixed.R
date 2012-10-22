#' @S3method param logit.mixed
param.logit.mixed <- function(obj, num=1000, ...) {
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
