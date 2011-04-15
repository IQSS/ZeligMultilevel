#' Extract the Environment of a `lmer' object
#' @param formula a `lmer' object
#' @param ... ignored parameters
#' @return a `model.frame' object
#' @export
#' @author Matt Owen and Ferdinand Alimadhi and Delia
#'         \email{mowen@@iq.harvard.edu}
model.frame.lmer <- function(formula, ...)
  formula@frame
