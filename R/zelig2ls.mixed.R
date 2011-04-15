#' Interface between the Zelig Model ls.mixed and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... 
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2ls.mixed <- function (formula, family, ..., data) {
  if (!missing(family))
    warning("family parameter is ignored for the `ls.mixed' model")

  formula <- tolmerFormat(reduceMI(formula))

  list(
       .function = "lmer",
       formula = literal(formula),
       family = NULL,
       data = data,
       ...
       )
}
