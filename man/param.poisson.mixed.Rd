\name{param.poisson.mixed}
\alias{param.poisson.mixed}
\title{Extract Samples from a Distribution in Order to Pass Them to the `qi'
Function (this is primarily a helper function for the logit.mixed model)}
\usage{
  \method{param}{poisson.mixed}(obj, num=1000, ...)
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
  for the logit.mixed model)
}
\author{
  Matt Owen and Ferdinand Alimadhi and Delia
}

