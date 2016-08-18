zmixed <- setRefClass("Zelig-mixed",
                      fields = list(formula.full = "ANY",# Zelig formula
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
    return(list(simparam = z.out))
  }
)

zmixed$methods(
  qi = function(simparam, mm) {
    ## Get the grouping variables from the estimated model
    group <- names(ranef(simparam$simparam))
    
    get.random.group.mm <- function() {
      RE <- NULL
      for (g in group) {
        y <- sample(simparam$simparam@frame[[g]], 1)
        # http://stackoverflow.com/questions/30943516/cbind-factor-vector-with-level-names
        RE <- cbind(RE, levels(y)[y])
      }
      RE <- as.data.frame(RE)
      names(RE) <- group
      return(RE)
    }
    
    mm.with.random.group <- function() {
      return(cbind(as.data.frame(mm), get.random.group.mm()))
    }
    
    ## If no group is specified, take one at random
    if (is.null(.self$mm.RE)) {
      # mm <- cbind(as.data.frame(mm), get.random.group.mm())
      mm <- mm.with.random.group()
    } else { ## If a group is specified, make sure the information is passed to the model matrix
      mm <- cbind(as.data.frame(mm), .self$mm.RE)
    }
    
    ## If no group is specified, take one at random
    mm.all <- NULL
    if (is.null(.self$mm.RE)) {
      for (i in 1:.self$num) {
        mm.all <- rbind(mm.all, mm.with.random.group())
      }
    } else {
      for (i in 1:.self$num) {
        mm.all <- rbind(mm.all, mm)
      }
    }
    
    PI <- merTools::predictInterval(merMod = simparam$simparam,
                                    newdata = mm,
                                    n.sims = .self$num,
                                    returnSims = TRUE,
                                    type = .self$simtype)
    
    PI.all <- merTools::predictInterval(merMod = simparam$simparam,
                                        newdata = mm.all,
                                        n.sims = .self$num,
                                        returnSims = TRUE,
                                        type = .self$simtype)
    
    ev.all <- as.matrix(t(attr(PI.all, "sim.results")))
    
    ev <- as.matrix(apply(ev.all, 1, mean, na.rm = TRUE))
    pv <- t(attr(PI, "sim.results"))
    
    if (.self$family == "binomial") {
      print("binomial")
      ev <- .self$linkinv(ev)
      # pv <- .self$linkinv(pv)
      # pv <- rbinom(.self$num, 1, mean(pv))
      pv <- matrix(nrow = nrow(ev), ncol = ncol(ev))
      for (j in 1:ncol(ev))
        pv[, j] <- rbinom(length(ev[, j]), 1, prob = ev[, j])
      levels(pv) <- c(0, 1)
    } else {
      ev <- .self$linkinv(ev)
      pv <- .self$linkinv(pv)
    }
    print("##----- ev")
    print(dim(ev))
    print(ev)
    print("##----- pv")
    print(dim(pv))
    print(pv)
    return(list(ev = ev, pv = pv))
  }
)

