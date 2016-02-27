reduce <- function(dataset, s, formula, data) {
  pred <- try(terms(fit <- lm(formula, data), "predvars"), silent = TRUE)
  if ("try-error" %in% class(pred)) # exp and weibull
    pred <- try(terms(fit <- survreg(formula, data), "predvars"), silent = TRUE)
  dataset <- model.frame(fit)
  ldata <- lapply(dataset, avg)
  if (length(s) > 0) {
    n <- union(as.character(attr(pred, "predvars"))[-1],
               names(dataset))
    if (is.list(s[[1]]))
      s <- s[[1]]
    m <- match(names(s), n)
    ma <- m[!is.na(m)]
    if (!all(complete.cases(m))) {
      w <- paste("Variable '", names(s[is.na(m)]),
                 "' not in data set.\n", sep = "")
      warning(w)
    }
    for (i in seq(n[ma]))
      ldata[n[ma]][i][[1]] <- setval(dataset[n[ma]][i][[1]],
                                     s[n[ma]][i][[1]])
  }
  return(ldata)
}