zmixed <- setRefClass("Zelig-mixed",
                      fields = list(x.beta = "ANY",
                                    z.b = "ANY",
                                    offset = "ANY",
                                    error = "ANY",
                                    formula.full = "ANY",# Zelig formula
                                    mm.RE = "ANY", # group membership
                                    simtype = "ANY"), # linear or probability 
                      contains = "Zelig")

zmixed$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(lme4::lmer)
    .self$packageauthors <- "Douglas Bates [aut], Martin Maechler [aut], Ben Bolker [aut, cre], Steven Walker [aut], Rune Haubo Bojesen Christensen [ctb], Henrik Singmann [ctb], Bin Dai [ctb], Gabor Grothendieck [ctb], Peter Green [ctb]"
    .self$modelauthors <- "TBD" 
    .self$year <- 2016
    .self$mm.RE <- NULL
    .self$acceptweights <- TRUE
  }
)

zmixed$methods(
  set = function(...) {
    s <- list(...)
    group <- names(ranef(.self$zelig.out$z.out[[1]]))
    print(group)
    set.RE <- intersect(names(s), group)
    if (length(set.RE) > 0)
      .self$mm.RE <- as.data.frame(s[set.RE])
    callSuper(...)
  }
)

zmixed$methods(
  param = function(z.out) {
    return(list(simparam = arm::sim(z.out, .self$num), simalpha = z.out))
  }
)

# zmixed$methods(
#   qi = function(simparam, mm) {
#     regression <<- simparam$simalpha;
#     sims <<- simparam$simparam;
#     print("there")
#     numSimulations <- dim(sims@fixef)[1];
#     devcomp <- getME(regression, "devcomp");
#     dims <- devcomp$dims;
#     
#     numRanef  <- dims[["q"]];
#     numLevels <- dims[["reTrms"]];
#     
#     simulatedRanef <- matrix(0, numRanef, numSimulations);
#     
#     index <- 0;
#     for (i in 1:length(sims@ranef)) {
#       levelSims <- sims@ranef[[i]];
#       numCoefficientsPerLevel <- dim(levelSims)[2];
#       numGroupsPerLevel <- dim(levelSims)[3];
#       for (j in 1:numCoefficientsPerLevel) {
#         ranefRange <- index + 1:numGroupsPerLevel;
#         index <- index + numGroupsPerLevel;
#         
#         simulatedRanef[ranefRange,] <- t(levelSims[,j,]);
#       }
#     }
#     
#     # X <- getME(regression, "X");
#     X <<- matrix(rep(mm, length(X)), nrow(X), ncol(X), byrow = TRUE)
#     
#     
#     Zt <- getME(regression, "Zt");
#     # 
#     # linearPredictor <- as.matrix(tcrossprod(as.matrix(X), sims@fixef) + crossprod(as.matrix(Zt), simulatedRanef)) +
#     #   matrix(getME(regression, "offset"), dims[["n"]], numSimulations);
#     
#     .self$x.beta <- as.matrix(tcrossprod(as.matrix(X), sims@fixef))
#     .self$z.b <- as.matrix(crossprod(as.matrix(Zt), simulatedRanef))
#     .self$offset <- matrix(getME(regression, "offset"), dims[["n"]], numSimulations)
#     
#     # return(linearPredictor)
#     # if (.self$name %in% c("logit.mixed", "probit.mixed")) {
#     #   print("binomial")
#     #   ev <- .self$linkinv(ev)
#     #   # pv <- .self$linkinv(pv)
#     #   # pv <- rbinom(.self$num, 1, mean(pv))
#     #   pv <- matrix(nrow = nrow(ev), ncol = ncol(ev))
#     #   for (j in 1:ncol(ev))
#     #     pv[, j] <- rbinom(length(ev[, j]), 1, prob = ev[, j])
#     #   levels(pv) <- c(0, 1)
#     # } else if (.self$name %in% c("ls.mixed")) {
#     #   # ev <- .self$linkinv(ev)
#     #   # pv <- .self$linkinv(pv)
#     #   pv <- as.matrix(rnorm(n=length(ev), mean=ev, sd=sd(ev)), nrow=length(ev), ncol=1)
#     # }
#     # print("##----- ev")
#     # print(dim(ev))
#     # print(ev)
#     # print("##----- pv")
#     # print(dim(pv))
#     # print(pv)
#     # return(list(ev = ev, pv = pv))
#   }
# )

