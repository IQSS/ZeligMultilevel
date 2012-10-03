#' @S3method model.frame lmer
model.frame.lmer <- function(formula, ...)
  formula@frame
