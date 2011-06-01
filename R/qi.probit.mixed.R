#' Compute Quantities of Interest for the Zelig Model \code{probit.mixed}
#' @usage \method{qi}{probit.mixed}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi probit.mixed
#' @param obj a zelig object
#' @param x a setx object
#' @param x1 an optional setx object
#' @param y ...
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of interest
#'         with their simulations
#' @export
qi.probit.mixed <- qi.logit.mixed
