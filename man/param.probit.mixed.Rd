\name{param.probit.mixed}
\alias{param.probit.mixed}
\title{Extract Samples from a Distribution in Order to Pass Them to the `qi' Function
(this is primarily a helper function for the probit.mixed model)}
\usage{
  \method{param}{probit.mixed}(obj, num=1000, ...)
}
\arguments{
  \item{obj}{a zelig object}

  \item{num}{an integer specifying the number of
  simulations to compute}

  \item{...}{ignored parameters}
}
\value{
  a list specifying link, link-inverse, random samples, and
  ancillary parameters
}
\description{
  Extract Samples from a Distribution in Order to Pass Them
  to the `qi' Function (this is primarily a helper function
  for the probit.mixed model)
}

