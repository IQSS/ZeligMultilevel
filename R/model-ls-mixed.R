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
    if (!is.null(.self$group))
      print(.self$group)
    rTerms <- simparam$simalpha$rTerms
    # ## For predicted values, add in random effects draws according to "group" membership
    if (.self$group == "all") {
      for (i in 1:length(rTerms)){
        mu <<- as.vector(mu) + as.matrix(simparam$simalpha$gammas[[names(rTerms[i])]]) %*% t(as.matrix(rTerms[[i]]))
        }
      n <- length(mu[, 1])
      pv <- matrix(NA, nrow = nrow(mu), ncol = ncol(mu))
      for (i in 1:ncol(mu)) {
        pv[, i] <- rnorm(n, mean = mu[, i], sd = simparam$simalpha$scale)
      }
      colnames(pv) <- colnames(mu)
      PR <<- pv
    }
    if (!is.null(.self$group.value)) {
      mu <- mu[, .self$group.value, drop = FALSE]
      pv <- pv[, .self$group.value, drop = FALSE]
    } else if (.self$group == "none") {
      for (i in 1:length(rTerms)){
        mu <- as.vector(mu) + as.matrix(simparam$simalpha$gammas[[names(rTerms[i])]]) %*% t(as.matrix(rTerms[[i]]))
      }
      n <- length(mu[, 1])
      mu <- as.matrix(apply(mu, MARGIN = 1, sample, 1)) # take one group at random
      MU_NONE <<- mu
      pv <- matrix(NA, nrow = nrow(mu), ncol = ncol(mu))
      for (i in 1:ncol(mu)) {
        pv[, i] <- rnorm(n, mean = mu[, i], sd = simparam$simalpha$scale)
      }
    }
    return(list(ev = mu, pv = pv))
  }
)

