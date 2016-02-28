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
  set = function(...) {
    "Setting Explanatory Variable Values"
    formula_fe <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE)
    s <-list(...)
    # This eliminates warning messages when factor rhs passed to lm() model in reduce() utility function
    if(.self$category=="multinomial"){  # Perhaps find more robust way to test if dep.var. is factor
      f2 <- update(formula_fe, as.numeric(.) ~ .)
    }else{
      f2 <- formula_fe
    }
    f <- update(formula_fe, 1 ~ .)      
    # update <- na.omit(.self$data) %>% # remove missing values
    update <- .self$data %>%
      group_by_(.self$by) %>%
      do(mm = model.matrix(f, reduce(dataset = ., s, 
                                     formula = f2, 
                                     data = .self$data)))
    return(update)
  }
)

zlsmixed$methods(
  param = function(z.out) {
    # summary(z.out)$sigma: scale estimate
    return(list(simparam = mvrnorm(.self$num, fixef(z.out), vcov(z.out)),
                # simalpha = rep(summary(z.out)$sigma, .self$num),
                simalpha = summary(z.out)$sigma))
  }
)

# zlsmixed$methods(
#   param = function(z.out) {
#     # return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
#   }
# )

zlsmixed$methods(
  qi = function(simparam, mm) {
    ev <- simparam$simparam %*% t(mm)
    pv <- as.matrix(rnorm(n = length(ev), mean = ev,  sd = simparam$simalpha),
                    nrow = length(ev),
                    ncol = 1)
    # pv <- ev
    return(list(ev = ev, pv = pv))
  }
)

# zlsmixed$methods(
#   # From Zelig 4
#   qi = function(simparam, mm) {
#   }
# )
