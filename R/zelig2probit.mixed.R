#' Interface between the Zelig Model probit.mixed and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... parameter to be passed to the \code{lmer} function
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2probit.mixed <- function (formula, ..., data) {

  formula <- tolmerFormat(reduceMI(formula))

  list(
       .function = "glmer",
       formula = (formula),
       family = binomial(link='probit'),
       data = data,
       ...
       )
}
