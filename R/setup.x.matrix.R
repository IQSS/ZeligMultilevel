#' Setup Matrix
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
setup.x.matrix <- function(form, x) {

  # Error-catching
  if (is.null(x))
    return(NULL)

  parsed.formula <- .getRandAndFixedTerms(form)
  untagged.vars <- all.vars(parsed.formula$fixed)

  # Get only the explanatory parameters from
  # the x-matrix
  x.matrix <- x$updated[untagged.vars]

  x.matrix <- model.matrix(parsed.formula$fixed, data=x$updated[untagged.vars])

  #x.matrix <- as.matrix(x.matrix)

  x.matrix
}
