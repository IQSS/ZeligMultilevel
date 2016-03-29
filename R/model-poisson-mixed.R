zpoissonmixed <- setRefClass("Zelig-poissonmixed",
                        fields = list(family = "ANY",
                                      link = "ANY",
                                      linkinv = "ANY"),
                        contains = c("Zelig-mixed", "Zelig-poisson"))

zpoissonmixed$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson.mixed"
    .self$fn <- quote(lme4::glmer)
    .self$category <- "discrete"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self
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
