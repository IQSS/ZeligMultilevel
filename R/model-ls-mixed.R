zlsmixed <- setRefClass("Zelig-lsmixed",
                        contains = c("Zelig-mixed"))

zlsmixed$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ls.mixed"
    .self$fn <- quote(lme4::lmer)
    .self$category <- "continuous"
    .self$acceptweights <- TRUE
    .self$simtype <- "linear"
    .self$wrapper <- "ls.mixed"
  }
)

zlsmixed$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ..., weights = weights, by = by)
    .self$formula.full <- .self$formula # fixed and random effects
    .self$formula <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE) # fixed effects only
  }
)

zlsmixed$methods(
  set = function(...) {
    "Setting Explanatory Variable Values"
    new.data <- as.data.frame(c(as.list(environment()), list(...)))
    print(new.data)
    if (length(new.data) == 0) # take the average
      update <- .self$data %>%
      group_by_(.self$by) %>%
      do(mm = merTools::draw(.self$zelig.out$z.out[[1]], type = "average"))
    else
      update <- .self$data %>%
      group_by_(.self$by) %>%
      do(mm = new.data)
    return(update)
  }
)

zlsmixed$methods(
  setx = function(...) {
    .self$bsetx <- TRUE
    .self$setx.out$x  <- .self$set(...)
  }
)

zlsmixed$methods(
  setx1 = function(...) {
    .self$bsetx1 <- TRUE
    .self$setx.out$x1 <- .self$set(...)
  }
)
