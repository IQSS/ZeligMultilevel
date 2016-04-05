zmixed <- setRefClass("Zelig-mixed",
                      fields = list(formula.full = "ANY",# Zelig formula)
                                    group = "ANY", 
                                    group.value = "ANY"),
                      contains = "Zelig")

zmixed$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(lme4::lmer)
    .self$authors <- "Ferdinand Alimadhi, Delia Bailey"
    .self$packageauthors <- "Douglas Bates [aut], Martin Maechler [aut], Ben Bolker [aut, cre], Steven Walker [aut], Rune Haubo Bojesen Christensen [ctb], Henrik Singmann [ctb], Bin Dai [ctb], Gabor Grothendieck [ctb], Peter Green [ctb]"
    .self$year <- 2012
  }
)

zmixed$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    # .self$call$family <- .self$family.lme4
    # .self$model.call$family <- paste0(.self$family, '(', .self$link, ')')
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by)
    .self$formula.full <- .self$formula # fixed and random effects
    .self$formula <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE) # fixed effects only
    # lme4:::getFixedFormula(.self$formula.full)
  }
)

zmixed$methods(
  param = function(z.out) {
    return(list(simparam = arm::sim(z.out, .self$num), simalpha = list(z.out = z.out)))
  }
)

zmixed$methods(
  sim = function(num = 1000, group = NULL, group.value = NULL) {
    .self$group <- group
    .self$group.value <- group.value
    callSuper(num = num)
  }
)
