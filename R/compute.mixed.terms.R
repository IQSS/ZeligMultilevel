#' Compute Random and Fixed Terms
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
compute.mixed.terms <- function (form, x, data) {
  if (is.null(x))
    return (NULL)

  x <- as.matrix(x)

  # parse formula
  parsed.formula <- .getRandAndFixedTerms(form)

  # there is a better way to do this.
  f.term.names <- colnames(model.matrix(parsed.formula$fixed, data = data))

  # what are f-terms
  f.terms <-  x[, f.term.names]

  # what are r-terms
  r.terms <- list()

  # ...
  for (k in 1:length(parsed.formula$random)) {
    rand <- parsed.formula$random[[k]]
    rand.terms <- terms(rand)

    # this is a hack to guarantee the presence of an intercept.
    # this may not be legitimate for the general-case model
    # and is explicitly noted in the previous code revision with:
    ## for now, intercept is always present
    attr(rand.terms, 'intercept') <- 1

    r.term.names <- colnames(model.matrix(rand.terms, data=data))
    r.terms[[k]] <- x[, r.term.names]
    names(r.terms[[k]]) <- r.term.names
  }

  names(r.terms) <- names(parsed.formula$random)

  # return both f-terms and r-terms
  list(f.terms = f.terms, r.terms = r.terms)
}
