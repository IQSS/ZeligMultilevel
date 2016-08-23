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
    s <- as.data.frame(c(as.list(environment()), list(...)))
    
    f <- function(z.out) {
      mm <- merTools::draw(z.out, type = "average")
      variables <- names(.self$data)
      variables.to.set <- intersect(names(s), variables)
      print(variables.to.set)
      if (length(variables.to.set) > 0)
        for (i in seq(length(variables.to.set)))
          mm[[variables.to.set[i]]] <- s[names(s) == variables.to.set[i]][[1]]
      return(as.data.frame(mm))
    }
  
    # avg.data <<- .self$zelig.out %>% 
    #   do(avg = merTools::draw(.$z.out, type = "average"))
    
    mm <- merTools::draw(.self$zelig.out$z.out[[1]], type = "average")  # TODO: fix, not only [[1]], see above
    variables <- names(.self$data)
    variables.to.set <- intersect(names(s), variables)
    print(variables.to.set)
    if (length(variables.to.set) > 0)
      for (i in seq(length(variables.to.set)))
        mm[[variables.to.set[i]]] <- s[names(s) == variables.to.set[i]][[1]]
    update <- .self$data %>%
      group_by_(.self$by) %>%
      do(mm = as.data.frame(mm))
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

## TODO: Add setrange* functions

zlsmixed$methods(
  qi = function(simparam, mm) {
    print(simparam$simparam)
    print(mm)
    PI <- merTools::predictInterval(merMod = simparam$simparam,
                                    newdata = mm,
                                    n.sims = .self$num,
                                    returnSims = TRUE,
                                    type = .self$simtype)
    ev <- t(attr(PI, "sim.results"))
    return(list(ev = ev, pv = ev))
  }
)
