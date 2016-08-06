zlogitmixed <- setRefClass("Zelig-logitmixed",
                        fields = list(formula.full = "ANY",
                                      family.lme4 = "ANY"),
                        contains = c("Zelig-mixed", "Zelig-logit"))

zlogitmixed$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit.mixed"
    .self$fn <- quote(lme4::glmer)
    .self$category <- "continous"
    .self$family <- "binomial"
    .self$link <- "logit"
    .self$family.lme4 <- quote(binomial('logit'))
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$acceptweights <- TRUE
    .self$simtype <- "probability"
    .self$wrapper <- "logit.mixed"
  }
)

zlogitmixed$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    # .self$model.call$family <- .self$family
    # .self$model.call$family <- paste0(.self$family, '(', .self$link, ')')
    .self$model.call$family <- quote(binomial("logit"))
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by)
    .self$formula.full <- .self$formula # fixed and random effects
    .self$formula <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE) # fixed effects only
  }
)
