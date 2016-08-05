zmixed <- setRefClass("Zelig-mixed",
                      fields = list(formula.full = "ANY",# Zelig formula
                                    mm_RE = "ANY", # group membership
                                    sim_type = "ANY"), # linear or probability 
                      contains = "Zelig")

zmixed$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(lme4::lmer)
    .self$packageauthors <- "Douglas Bates [aut], Martin Maechler [aut], Ben Bolker [aut, cre], Steven Walker [aut], Rune Haubo Bojesen Christensen [ctb], Henrik Singmann [ctb], Bin Dai [ctb], Gabor Grothendieck [ctb], Peter Green [ctb]"
    .self$year <- 2016
    .self$mm_RE <- NULL
  }
)

zmixed$methods(
  set = function(...) {
    s <- list(...)
    group <- names(ranef(.self$zelig.out$z.out[[1]]))
    print(group)
    set_RE <- intersect(names(s), group)
    if (length(set_RE) > 0)
      .self$mm_RE <- as.data.frame(s[set_RE])
    print(.self$mm_RE)
    callSuper(...)
  }
)

zmixed$methods(
  param = function(z.out) {
    return(list(simparam = z.out))
  }
)

zmixed$methods(
  qi = function(simparam, mm) {
    group <- names(ranef(simparam$simparam))
    
    if (is.null(.self$mm_RE)) {
      print("NULL RE")
      RE <- NULL
      for (g in group) {
        y <- sample(simparam$simparam@frame[[g]], 1)
        # http://stackoverflow.com/questions/30943516/cbind-factor-vector-with-level-names
        RE <- cbind(RE, levels(y)[y])
      }
      print("RE")
      print(RE)
      RE <- as.data.frame(RE)
      names(RE) <- group
      
      mm <- cbind(as.data.frame(mm), RE)
      print(mm)
    } else {
      mm <- cbind(as.data.frame(mm), .self$mm_RE)
    }
    
    # TODO: check whether group is specified
    # Now: if no group, select one at random
    mm_all <- NULL
    for (i in 1:.self$num)
      mm_all <- rbind(mm_all, mm)
    
    PI <- merTools::predictInterval(merMod = simparam$simparam,
                                    newdata = mm,
                                    n.sims = .self$num,
                                    returnSims = TRUE,
                                    type = .self$sim_type)
    
    # print(PI)
    
    PI_all <- merTools::predictInterval(merMod = simparam$simparam,
                                        newdata = mm_all,
                                        n.sims = .self$num,
                                        returnSims = TRUE,
                                        type = .self$sim_type)
    
    ev_all <- as.matrix(t(attr(PI_all, "sim.results")))
    
    ev <- as.matrix(apply(ev_all, 1, mean, na.rm = TRUE))
    pv <- t(attr(PI, "sim.results"))
    
    if (.self$family == "binomial") {
      print("binomial")
      ev <- .self$linkinv(ev)
      pv <- .self$linkinv(pv)
      pv <- rbinom(.self$num, 1, mean(pv))
      levels(pv) <- c(0, 1)
    } else {
      ev <- .self$linkinv(ev)
      pv <- .self$linkinv(pv)
    }
    
    return(list(ev = ev, pv = pv))
  }
)

