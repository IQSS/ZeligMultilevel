zlsmixed <- setRefClass("Zelig-lsmixed",
                        contains = c("Zelig-mixed", "Zelig-ls"))

zlsmixed$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ls.mixed"
    .self$fn <- quote(lme4::lmer)
    .self$authors <- "Ferdinand Alimadhi, Delia Bailey"
    .self$packageauthors <- "Douglas Bates [aut], Martin Maechler [aut], Ben Bolker [aut, cre], Steven Walker [aut], Rune Haubo Bojesen Christensen [ctb], Henrik Singmann [ctb], Bin Dai [ctb], Gabor Grothendieck [ctb], Peter Green [ctb]"
    .self$year <- 2012
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

zlsmixed$methods(
  qi = function(simparam, mm) {
    if (!is.null(.self$group))
      print(.self$group)
    mu <- simparam$simparam %*% t(mm) # corresponds to X * beta
    # rTerms <- ranef(.self$zelig.out$z.out[[1]]) # fix for the 'by' argument
    rTerms <- simparam$simalpha$rTerms
    # ## For predicted values, add in random effects draws according to "group" membership
    if (.self$group == "none") {
      pv <- 0
    } else if (.self$group == "all") {
      for (i in 1:length(rTerms)){
        mu <- as.vector(mu) + as.matrix(simparam$simalpha$gammas[[names(rTerms[i])]]) %*% t(as.matrix(rTerms[[i]]))
      }
      n <- length(mu[, 1])
      pv <- matrix(NA, nrow = nrow(mu), ncol = ncol(mu))
      for (i in 1:ncol(mu)) {
        pv[, i] <- rnorm(n, mean = mu[, i], sd = simparam$simalpha$scale)
      }
      colnames(pv) <- colnames(mu)
    }
    if (!is.null(.self$group.value)) {
      mu <- mu[, .self$group.value, drop = FALSE]
      pv <- pv[, .self$group.value, drop = FALSE]
    } 
    return(list(ev = mu, pv = pv))
  }
)

