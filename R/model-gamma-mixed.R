zgammamixed <- setRefClass("Zelig-gammamixed",
                        fields = list(formula.full = "ANY",
                                      family.lme4 = "ANY"),
                        contains = c("Zelig-mixed", "Zelig-gamma"))

zgammamixed$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(lme4::glmer)
    # .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    # .self$packageauthors <- "Thomas W. Yee"
    # .self$year <- 2007
    .self$category <- "continous"
    .self$family <- "Gamma"
    .self$link <- "log"
    .self$family.lme4 <- quote(Gamma('log'))
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
  }
)

zgammamixed$methods(
  param = function(z.out) {
    shape <- MASS::gamma.shape(z.out)
    simalpha <- rnorm(n = .self$num, mean = shape$alpha, sd = shape$SE)
    simparam.local <- mvrnorm(n = .self$num, mu = fixef(z.out),
                              Sigma = vcov(z.out))
    simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
    return(simparam.local)
  }
)

# zgammamixed$methods(
#   qi = function(simparam, mm) {
#     ev <- simparam$simparam %*% t(mm)
#     pv <- as.matrix(rnorm(n = length(ev), mean = ev, sd = simparam$simalpha),
#                     nrow = length(ev),
#                     ncol = 1)
#     return(list(ev = ev, pv = pv))
#   }
# )

# zgammamixed$methods(
#   mcfun = function(x, b0 = 0, b1 = 1, alpha = 1, sim = TRUE) {
#     y <- b0 + b1*x + sim * rnorm(n = length(x), sd = alpha)
#     return(y)
#   }
# )

# zgammamixed$methods((
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
