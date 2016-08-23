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
    .self$mm.RE <- NULL # reset group membership
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
