#' Interface between the Zelig Model logit.mixed and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param family a \code{family} object. This parameter should be changed to
#'   \code{link}, since it currently allows to interpret a \code{gamma.mixed}
#'   model as any other
#' @param ... parameter to be passed to the \code{lmer} function
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
#' @author Matt Owen and Ferdinand Alimadhi and Delia
zelig2logit.mixed <- function (formula, family=binomial(link='logit'), ..., data) {

  formula <- tolmerFormat(reduceMI(formula))

  list(
       .function = "glmer",
       formula = (formula),
       family = family,
       data = data,
       ...
       )
}
