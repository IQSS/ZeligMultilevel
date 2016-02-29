zpoissonmixed <- setRefClass("Zelig-poissonmixed",
                        fields = list(formula.full = "ANY"), # Zelig formula)
                        contains = c("Zelig-mixed", "Zelig-poisson"))

zpoissonmixed$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(lme4::glmer)
    # .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    # .self$packageauthors <- "Thomas W. Yee"
    # .self$year <- 2007
    .self$category <- "discrete"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
  }
)

zpoissonmixed$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, fixef(z.out), vcov(z.out)))
  }
)

# zpoissonmixed$methods(
#   qi = function(simparam, mm) {
#     ev <- simparam$simparam %*% t(mm)
#     pv <- as.matrix(rnorm(n = length(ev), mean = ev, sd = simparam$simalpha),
#                     nrow = length(ev),
#                     ncol = 1)
#     return(list(ev = ev, pv = pv))
#   }
# )

# zpoissonmixed$methods(
#   mcfun = function(x, b0 = 0, b1 = 1, alpha = 1, sim = TRUE) {
#     y <- b0 + b1*x + sim * rnorm(n = length(x), sd = alpha)
#     return(y)
#   }
# )

# zpoissonmixed$methods((
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
