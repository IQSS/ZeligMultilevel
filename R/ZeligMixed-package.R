#' Multilevel Regressions for Zelig
#'
#' \tabular{ll}{
#' Package: \tab ZeligMultilevel\cr
#' Version: \tab 0.7-0\cr
#' Date: \tab 2012-10-22\cr
#' Depends: \tab Zelig (>= 4.0-11), lme4\cr
#' License: \tab GPL version 2 or newer\cr
#' URL: \tab http://projects.iq.harvard.edu/zelig\cr
#' }
#'
#' Add-on pack for Zelig, containing the models:
#' \tabular{ll}{
#' gamma.mixed: \tab Multi-level Gamma Regression \cr
#' logit.mixed: \tab Multi-level Logit Regression \cr
#' ls.mixed: \tab Multi-level Least-squares Regression \cr
#' poisson.mixed: \tab Multi-level Poisson Regression \cr
#' probit.mixed: \tab Multi-level Probit Regression \cr
#' }
#' 
#' @name ZeligMultilevel-package
#' @aliases ZeligMultilevel
#' @docType package
#' @author Ferdinand Alimadhi, Delia Bailey
#' Maintainer: Matt Owen \email{mowen@@iq.harvard.edu}
#' @keywords package Zelig Multilevel Mixed-effect
#' @importFrom Zelig describe param qi
NULL

.fitted <- NULL
