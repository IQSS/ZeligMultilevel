zgammamixed <- setRefClass("Zelig-gammamixed",
                        fields = list(formula.full = "ANY",
                                      family.lme4 = "ANY"),
                        contains = c("Zelig-mixed", "Zelig-gamma"))

zgammamixed$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(lme4::glmer)
    .self$category <- "continous"
    .self$family <- "Gamma"
    .self$link <- "log"
    .self$family.lme4 <- quote(Gamma('log'))
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$acceptweights <- TRUE
    .self$wrapper <- "gamma.mixed"
    .self$sim_type <- "probability"
  }
)

zgammamixed$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    # .self$model.call$family <- .self$family
    # .self$model.call$family <- paste0(.self$family, '(', .self$link, ')')
    .self$model.call$family <- quote(Gamma("log"))
    callSuper(formula = formula, data = data, ..., weights = weights, by = by)
    .self$formula.full <- .self$formula # fixed and random effects
    .self$formula <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE) # fixed effects only
  }
)
