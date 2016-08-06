zprobitmixed <- setRefClass("Zelig-probitmixed",
                        fields = list(formula.full = "ANY",
                                      family.lme4 = "ANY"),
                        contains = c("Zelig-mixed", "Zelig-probit"))

zprobitmixed$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probit.mixed"
    .self$fn <- quote(lme4::glmer)
    .self$category <- "continous"
    .self$family <- "binomial"
    .self$link <- "probit"
    .self$family.lme4 <- quote(binomial('probit'))
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$acceptweights <- TRUE
    .self$simtype <- "probability"
    .self$wrapper <- "probit.mixed"
  }
)

zprobitmixed$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    # .self$model.call$family <- .self$family
    # .self$model.call$family <- paste0(.self$family, '(', .self$link, ')')
    .self$model.call$family <- quote(binomial("probit"))
    callSuper(formula = formula, data = data, ..., weights = weights, by = by)
    .self$formula.full <- .self$formula # fixed and random effects
    .self$formula <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE) # fixed effects only
  }
)
