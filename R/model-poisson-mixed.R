zpoissonmixed <- setRefClass("Zelig-poissonmixed",
                        fields = list(family = "ANY",
                                      link = "ANY",
                                      linkinv = "ANY"),
                        contains = c("Zelig-mixed"))

zpoissonmixed$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson.mixed"
    .self$fn <- quote(lme4::glmer)
    .self$category <- "discrete"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
  }
)

zpoissonmixed$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    # .self$model.call$family <- .self$family
    # .self$model.call$family <- paste0(.self$family, '(', .self$link, ')')
    .self$model.call$family <- quote(poisson("log"))
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by)
    .self$formula.full <- .self$formula # fixed and random effects
    .self$formula <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE) # fixed effects only
  }
)