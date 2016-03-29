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
    # .self$model.call$family <- .self$family.lme4
    # .self$model.call$family <- paste0(.self$family, '(', .self$link, ')')
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by)
    .self$formula.full <- .self$formula # fixed and random effects
    .self$formula <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE) # fixed effects only
    # lme4:::getFixedFormula(.self$formula.full)
  }
)

zmixed$methods(
  param = function(z.out) {
    fixed_effects <- fixef(z.out)
    random_effects <- ranef(z.out, condVar = TRUE)
    gammas <- NULL
    # object <- summary(z.out)
    # sample fixed effects
    if (length(fixed_effects) > 0){
      betas <- MASS::mvrnorm(.self$num, fixed_effects, vcov(z.out))
    }
    # sample random effects
    for (m in seq(random_effects)) {
      vars.m <- attr(random_effects[[m]], "postVar")
      V.beta <<- VarCorr(z.out)[[m]]
      J <- dim(vars.m)[1]
      gammas[[m]] <- MASS::mvrnorm(.self$num, rep(0, J), as.data.frame(V.beta))
    }
    names(gammas) <- names(random_effects)
    # for (i in seq(gamma)) {
    #   rownames(gammas[[i]]) <- rownames(ranef(z.out, condVar = TRUE)[[1]])
    # }
    scale <- sigma(z.out)
    return(list(simparam = betas,
                # simalpha = rep(summary(z.out)$sigma, .self$num),
                simalpha = list(gammas = gammas, scale = scale, rTerms = random_effects)))
  }
)

zmixed$methods(
  sim = function(num = 1000, group = NULL, group.value = NULL) {
    .self$group <- group
    .self$group.value <- group.value
    callSuper(num = num)
  }
)
