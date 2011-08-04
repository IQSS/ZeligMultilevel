#' Interface between the Zelig Model poisson.mixed and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param family a \code{family} object. This parameter should be changed to
#'   \code{link}, since it currently allows to interpret a \code{gamma.mixed}
#'   model as any other
#' @param ... parameter to be passed to the \code{lmer} function
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2poisson.mixed <- function (formula, family=gaussian, ..., data) {

  formula <- tolmerFormat(reduceMI(formula))

  list(
       .function = "lmer",
       formula = (formula),
       family  = family,
       data = data,
       ...
       )
}
