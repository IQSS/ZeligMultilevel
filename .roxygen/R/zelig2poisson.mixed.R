#' Interface between the Zelig Model poisson.mixed and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... 
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2poisson.mixed <- function (formula, ..., data) {
  list(
       .function = "",
       formula = formula,
       data = data
       )
}
