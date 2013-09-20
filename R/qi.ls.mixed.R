#' @S3method qi ls.mixed
qi.ls.mixed <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  # extract parameters from `zelig' object
  data <- obj$data
  form <- get("formula", attr(obj, "state"))

  # extract simulated parameters from `param' object
  betas <- coef(param)
  gammas <- alpha(param)$gamma

  #x1.matrix <- ZeligMultilevel:::setup.x.matrix(form, x)
  #x2.matrix <- ZeligMultilevel:::setup.x.matrix(form, x1)
  # New CRAN requirements:
  x1.matrix <- setup.x.matrix(form, x)
  x2.matrix <- setup.x.matrix(form, x1)

  # compute terms
  #mixed.terms1 <- ZeligMultilevel:::compute.mixed.terms(form, x1.matrix, data)
  #mixed.terms2 <- ZeligMultilevel:::compute.mixed.terms(form, x2.matrix, data)
  # New CRAN requirements:
  mixed.terms1 <- compute.mixed.terms(form, x1.matrix, data)
  mixed.terms2 <- compute.mixed.terms(form, x2.matrix, data)

  # extract relevant terms
  # this is purely for code clarity
  fixed1 <- mixed.terms1$f.terms
  random1 <- mixed.terms1$r.terms

  fixed2 <- mixed.terms2$f.terms
  random2 <- mixed.terms2$r.terms

  #
  #qi1 <- ZeligMultilevel:::.compute.ls.ev.and.pv(fixed1, random1, param=param)
  qi1 <- .compute.ls.ev.and.pv(fixed1, random1, param=param)
  if(!is.null(fixed2)){
    #qi2 <- ZeligMultilevel:::.compute.ls.ev.and.pv(fixed2, random2, param=param)
    qi2 <- .compute.ls.ev.and.pv(fixed2, random2, param=param)
    } else {
  	  qi2<-qi1
  	  qi2$ev<-NA
  	  qi2$pv<-NA  	
    }



  list(
       "Expected Values: E(Y|X)" = qi1$ev,
       "Predicted Values: Y|X" = qi1$pv,
       "Expected Values: E(Y|X1)" = qi2$ev,
       "Predicted Values: Y|X1" = qi2$pv,
       "Risk Ratios: E(Y|X1) / E(Y|X)" = qi2$ev / qi1$ev,
       "First Differences: E(Y|X1) - E(Y|X)" = qi2$ev - qi1$ev
       )
}

#' Compute Multilevel Logit Expected Values
#' It seems this function is in need of heavy repair.
#' In particular, the exclusive presence of Inf and
#' numerically zero results 
#' @param fixed fixed values
#' @param random random values
#' @param param the `parameters' object
#' @return a list containing simulated expected values (ev),
#'         predicted values (pv) and warnings accrued from
#'         simulating the predicted values
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
.compute.ls.ev.and.pv <- function(fixed, random, ..., param) {
  betas <- coef(param)
  alpha <- alpha(param)$scale

  # number of simulations
  num <- nrow(betas)

  fixed.matrix <- as.matrix(fixed)
  
  mu <- eta <- betas %*% fixed.matrix

  # Something is wrong here...
  #for (k in 1:length(random)) {
  #  print(random[[k]])
  #  left <- gammas[[names(random[[k]])]]
  #  right <- t(as.matrix(random[[k]]))
  #  mu <- mu + left %*% right
  #}

  theta <- matrix(eta, nrow = num)

  ev <- theta
  pr <- matrix(NA, num)
  pv.warnings <- c()

  for (k in 1:ncol(mu))
    pr[, k] <- rnorm(length(mu[, 1]), mean=mu[, k], sd=alpha)

  # return qi's and predicted value warnings
  list(ev = ev, pv = pr, pv.warnings = pv.warnings)
}
