zlsmixed <- setRefClass("Zelig-lsmixed",
                        fields = list(formula.full = "ANY"), # Zelig formula)
                        contains = c("Zelig-mixed", "Zelig-ls"))

zlsmixed$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(lme4::lmer)
    # .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    # .self$packageauthors <- "Thomas W. Yee"
    # .self$year <- 2007
    .self$category <- "continuous"
  }
)

zlsmixed$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by)
    .self$formula.full <- .self$formula # fixed and random effects
    .self$formula <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE) # fixed effects only
  }
)

zlsmixed$methods(
  param = function(z.out) {
    fixed <- fixef(z.out)
    vars <- ranef(z.out, condVar = TRUE)
    gammas <- NULL
    n.G <- length(vars)
    object <- summary(z.out)
    # sample fixed effects
    betasF <- NULL
    vcov.fix <- vcov(z.out)
    if (length(fixed) > 0){
      betasF <- MASS::mvrnorm(.self$num, fixed, vcov.fix)
    }
    # sample random effects
    for (m in 1:n.G){
      vars.m <- attr(vars[[m]], "postVar")
      V.beta <- VarCorr(z.out)[[m]]
      J <- dim(vars.m)[1]
      gammas[[m]] <- MASS::mvrnorm(.self$num, rep(0, J), as.data.frame(V.beta))
    }
    names(gammas) <- names(vars)
    betas <- betasF
    scale <- sigma(z.out)
    return(list(simparam = betas,
           # simalpha = rep(summary(z.out)$sigma, .self$num),
           simalpha = list(gammas = gammas, scale = scale)))
  }
)

zlsmixed$methods(
  qi = function(simparam, mm) {
    eta <- simparam$simparam %*% t(mm)
    mu <- eta
    # ## For predicted values, add in random effects draws
    rTerms <- ranef(.self$zelig.out$z.out[[1]])
    for (i in 1:length(rTerms)){
      mu <- as.vector(mu) + as.matrix(simparam$simalpha$gammas[[names(rTerms[i])]]) %*% t(as.matrix(rTerms[[i]]))
    }
    ev <- eta
    n <- length(mu[, 1])
    pv <- matrix(NA, nrow = nrow(mu), ncol = ncol(mu))
    for (i in 1:ncol(mu)) {
      pv[, i] <- rnorm(n, mean = mu[, i], sd = simparam$simalpha$scale)
    }
    return(list(ev = ev, pv = pv))
  }
)

# zlsmixed$methods(
#   mcfun = function(x, b0 = 0, b1 = 1, alpha = 1, sim = TRUE) {
#     y <- b0 + b1*x + sim * rnorm(n = length(x), sd = alpha)
#     return(y)
#   }
# )

# zlsmixed$methods((
#   mcfun = function(x, ) {
#     N <- 100
#     nj <- 100
#     g00 <- 10
#     e <- rnorm(N * nj)
#     j <- c(sapply(1:N, function(x) rep(x, nj)))
#     uj <- c(sapply(1:N, function(x) rep(rnorm(1), nj)))
#     y <- g00+uj+e 
#   }
# ))
