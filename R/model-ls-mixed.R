zlsmixed <- setRefClass("Zelig-lsmixed",
                        contains = c("Zelig-mixed"))

zlsmixed$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ls.mixed"
    .self$fn <- quote(lme4::lmer)
    .self$category <- "continuous"
    .self$wrapper <- "ls.mixed"
  }
)

zlsmixed$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by)
    .self$formula.full <- .self$formula # fixed and random effects
    .self$formula <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE) # fixed effects only
  }
)

zlsmixed$methods(
  qi = function(simparam, mm) {
    sim_fixed_effects <<- simparam$simparam@fixef
    sim_random_effects <<- simparam$simparam@ranef
    z.out <<- simparam$simalpha$z.out
    betas <- sim_fixed_effects %*% t(mm)
    Zt <- getME(z.out, "Zt")
    gr <- unique(rownames(Zt))[1]
    si <- sim_random_effects$Subject[, "308", ]
    zi <- Zt[rownames(Zt) == "308", ]
    z <- zi[, apply(zi, 2, function(x) sum(x) != 0)]
    p <- si %*% z
    pv <<- t(as.vector(betas) + as.matrix(p))
    return(list(ev = betas, pv = pv))
  }
)

