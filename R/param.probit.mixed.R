#' Extract Samples from a Distribution in Order to Pass Them to the `qi' Function
#' (this is primarily a helper function for the probit.mixed model)
#' @S3method param probit.mixed
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... ignored parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary parameters
param.probit.mixed <- param.logit.mixed
