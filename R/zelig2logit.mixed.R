#' Interface between the Zelig Model logit.mixed and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... 
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
#' @author Matt Owen and Ferdinand Alimadhi and Delia
zelig2logit.mixed <- function (formula, family=binomial(link='logit'), ..., data) {

  formula <- tolmerFormat(reduceMI(formula))

  list(
       .function = "lmer",
       formula = literal(formula),
       family = family,
       data = data,
       ...
       )
}
