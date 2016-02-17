zlsmixed <- setRefClass("Zelig-lsmixed",
                           contains = "Zelig")

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
    # .self$model.call$family <- .self$family
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by)
  }
)

zlsmixed$methods(
  param = function(z.out) {
    # return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)

zlsmixed$methods(
  # From Zelig 4
  qi = function(simparam, mm) {
  }
)
