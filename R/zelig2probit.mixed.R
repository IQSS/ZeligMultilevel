#' Interface between the Zelig Model probit.mixed and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... 
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2probit.mixed <- function (formula, ..., data) {
  list(
       .function = "",
       formula = formula,
       data = data
       )
}
