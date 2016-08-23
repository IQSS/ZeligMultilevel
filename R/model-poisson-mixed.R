zpoissonmixed <- setRefClass("Zelig-poissonmixed",
                        fields = list(family = "ANY",
                                      link = "ANY",
                                      linkinv = "ANY"),
                        contains = c("Zelig-mixed"))

zpoissonmixed$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson.mixed"
    .self$fn <- quote(lme4::glmer)
    .self$category <- "discrete"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$acceptweights <- TRUE
    .self$simtype <- "probability"
    .self$wrapper <- "poisson.mixed"
  }
)

zpoissonmixed$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    # .self$model.call$family <- .self$family
    # .self$model.call$family <- paste0(.self$family, '(', .self$link, ')')
    .self$model.call$family <- quote(poisson("log"))
    callSuper(formula = formula, data = data, ..., weights = weights, by = by)
    .self$formula.full <- .self$formula # fixed and random effects
    .self$formula <- formula(.self$zelig.out$z.out[[1]], fixed.only = TRUE) # fixed effects only
  }
)

zpoissonmixed$methods(
  qi = function(simparam, mm) {
    regression <- simparam$simalpha;
    sims <- simparam$simparam;
    
    sims.tmp <- sims
    ## Check if group memberships are specified
    if (!is.null(.self$mm.RE)){
      for (group in names(.self$mm.RE)) {
        sims@ranef[[group]][, , ] <- 0
        sims@ranef[[group]][, unlist(.self$mm.RE[group]), ] <- sims.tmp@ranef[[group]][, unlist(.self$mm.RE[group]), ]
      }
    }
    
    numSimulations <- dim(sims@fixef)[1];
    devcomp <- getME(regression, "devcomp");
    dims <- devcomp$dims;
    
    numRanef  <- dims[["q"]];
    numLevels <- dims[["reTrms"]];
    
    simulatedRanef <- matrix(0, numRanef, numSimulations);
    
    index <- 0;
    for (i in 1:length(sims@ranef)) {
      levelSims <- sims@ranef[[i]];
      numCoefficientsPerLevel <- dim(levelSims)[2];
      numGroupsPerLevel <- dim(levelSims)[3];
      for (j in 1:numCoefficientsPerLevel) {
        ranefRange <- index + 1:numGroupsPerLevel;
        index <- index + numGroupsPerLevel;
        
        simulatedRanef[ranefRange,] <- t(levelSims[,j,]);
      }
    }
    
    X <- getME(regression, "X");
    X <- matrix(rep(mm, length(X)), nrow(X), ncol(X), byrow = TRUE)
    
    Zt <- getME(regression, "Zt");
    
    ## Linear predictor
    x.beta <- as.matrix(tcrossprod(as.matrix(X), sims@fixef))
    z.b <- crossprod(as.matrix(Zt), simulatedRanef)
    lp <- x.beta + z.b + matrix(getME(regression, "offset"), dims[["n"]], numSimulations);
    
    
    # eta <- simparam %*% t(mm)
    # theta.local <- matrix(.self$linkinv(eta), nrow = nrow(simparam))
    # ev <- theta.local
    # pv <- matrix(NA, nrow = nrow(theta.local), ncol = ncol(theta.local))
    # for (i in 1:ncol(theta.local))
    #   pv[, i] <- rpois(nrow(theta.local), lambda = theta.local[, i])
    # 
    
    ## FE only
    eta <- as.matrix(colMeans(x.beta, 1))
    theta.local <- matrix(.self$linkinv(eta), nrow = .self$num)
    ev <- theta.local
    
    ## FE + RE
    eta <- as.matrix(colMeans(lp, 1))
    theta.local <- matrix(.self$linkinv(eta), nrow = .self$num)
    
    pv <- matrix(NA, nrow = nrow(theta.local), ncol = ncol(theta.local))
    for (i in 1:ncol(theta.local))
      pv[, i] <- rpois(nrow(theta.local), lambda = theta.local[, i])
    
    return(list(ev = ev, pv = pv))
  }
)