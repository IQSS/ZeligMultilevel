#' Interface between the Zelig Model gamma.mixed and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... 
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2gamma.mixed <- function (formula, ..., data) {

  formula <- tolmerFormat(reduceMI(formula))

  list(
       .function = "lmer",
       formula = formula,
       family  = Gamma(link='identity'),
       data = data,
       ...
       )
}
