#' Describe a ``gamma.mixed'' model to Zelig
#' @usage \method{describe}{gamma.mixed}(...)
#' @S3method describe gamma.mixed
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.gamma.mixed <- function(...) {
  list(authors  = c("Ferdinand Alimadhi", "Delia Bailey"),
       year     = 2012,
       text = "Multilevel Gamma Regression"
       )
}

#' Describe a ``logit.mixed'' model to Zelig
#' @usage \method{describe}{logit.mixed}(...)
#' @S3method describe logit.mixed
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.logit.mixed <- function(...) {
  list(authors  = c("Ferdinand Alimadhi", "Delia Bailey"),
       year     = 2012,
       text = "Multilevel Logit Regression"
       )
}

#' Describe a ``ls.mixed'' model to Zelig
#' @usage \method{describe}{ls.mixed}(...)
#' @S3method describe ls.mixed
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.ls.mixed <- function(...) {
  list(authors  = c("Ferdinand Alimadhi", "Delia Bailey"),
       year     = 2012,
       text = "Multilevel Least Squares Regression"
       )
}

#' Describe a ``poisson.mixed'' model to Zelig
#' @usage \method{describe}{poisson.mixed}(...)
#' @S3method describe poisson.mixed
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.poisson.mixed <- function(...) {
  list(authors  = c("Ferdinand Alimadhi", "Delia Bailey"),
       year     = 2012,
       text = "Multilevel Poisson Regression"
       )
}

#' Describe a ``probit.mixed'' model to Zelig
#' @usage \method{describe}{probit.mixed}(...)
#' @S3method describe probit.mixed
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.probit.mixed <- function(...) {
  list(authors  = c("Ferdinand Alimadhi", "Delia Bailey"),
       year     = 2012,
       text = "Multilevel Probit Regression"
       )
}
