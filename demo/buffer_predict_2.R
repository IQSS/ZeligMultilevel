function (object, newdata = NULL, newparams = NULL, re.form = NULL, 
          ReForm, REForm, REform, terms = NULL, type = c("link", "response"), 
          allow.new.levels = FALSE, na.action = na.pass, ...) 
{
  re.form <- reFormHack(re.form, ReForm, REForm, REform)
  if (length(list(...)) > 0) 
    warning("unused arguments ignored")
  type <- match.arg(type)
  if (!is.null(terms)) 
    stop("terms functionality for predict not yet implemented")
  if (!is.null(newparams)) 
    object <- setParams(object, newparams)
  if ((is.null(newdata) && is.null(re.form) && is.null(newparams))) {
    if (isLMM(object) || isNLMM(object)) {
      pred <- na.omit(fitted(object))
    }
    else {
      pred <- switch(type, response = object@resp$mu, link = object@resp$eta)
      if (is.null(nm <- rownames(model.frame(object)))) 
        nm <- seq_along(pred)
      names(pred) <- nm
    }
    fit.na.action <- NULL
  }
  else {
    X <- getME(object, "X")
    X.col.dropped <- attr(X, "col.dropped")
    if (is.null(newdata)) {
      fit.na.action <- attr(object@frame, "na.action")
      offset <- model.offset(model.frame(object))
      if (is.null(offset)) 
        offset <- 0
    }
    else {
      RHS <- formula(substitute(~R, list(R = RHSForm(formula(object, 
                                                             fixed.only = TRUE)))))
      Terms <- terms(object, fixed.only = TRUE)
      mf <- model.frame(object, fixed.only = TRUE)
      isFac <- vapply(mf, is.factor, FUN.VALUE = TRUE)
      isFac[attr(Terms, "response")] <- FALSE
      orig_levs <- if (length(isFac) == 0) 
        NULL
      else lapply(mf[isFac], levels)
      mfnew <- model.frame(delete.response(Terms), newdata, 
                           na.action = na.action, xlev = orig_levs)
      X <- model.matrix(RHS, data = mfnew, contrasts.arg = attr(X, 
                                                                "contrasts"))
      offset <- 0
      tt <- terms(object)
      if (!is.null(off.num <- attr(tt, "offset"))) {
        for (i in off.num) offset <- offset + eval(attr(tt, 
                                                        "variables")[[i + 1]], newdata)
      }
      fit.na.action <- attr(mfnew, "na.action")
      if (is.numeric(X.col.dropped) && length(X.col.dropped) > 
          0) 
        X <- X[, -X.col.dropped, drop = FALSE]
    }
    pred <- drop(X %*% fixef(object))
    pred <- pred + offset
    if (!noReForm(re.form)) {
      if (is.null(re.form)) 
        re.form <- reOnly(formula(object))
      newRE <- mkNewReTrms(object, newdata, re.form, na.action = na.action, 
                           allow.new.levels = allow.new.levels)
      pred <- pred + base::drop(as(newRE$b %*% newRE$Zt, 
                                   "matrix"))
    }
    if (isGLMM(object) && type == "response") {
      pred <- object@resp$family$linkinv(pred)
    }
  }
  old.fit.na.action <- attr(model.frame(object), "na.action")
  if (!is.null(fit.na.action) || (!is.null(fit.na.action <- old.fit.na.action))) {
    if (!missing(na.action)) {
      class(fit.na.action) <- class(attr(na.action(NA), 
                                         "na.action"))
    }
    pred <- napredict(fit.na.action, pred)
  }
  pred
}
s@sigma
m2 <- lme4:::setParams(m1, list(beta = s@fixef[1, ]))
set.seed(02138); arm::sim(m2, 3)
set.seed(02138); arm::sim(m1, 3)

arm::sim(m2, 3)@fixef == arm::sim(m1, 3)@fixef
arm::sim(m2, 3)@sigma == arm::sim(m1, 3)@sigma

simulate(m1)
