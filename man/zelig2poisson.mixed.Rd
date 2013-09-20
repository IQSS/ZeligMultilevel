\name{zelig2poisson.mixed}
\alias{zelig2poisson.mixed}
\title{Interface between the Zelig Model poisson.mixed and
the Pre-existing Model-fitting Method}
\usage{
  zelig2poisson.mixed(formula, family = poisson, ...,
    data)
}
\arguments{
  \item{formula}{a formula}

  \item{family}{a \code{family} object. This parameter
  should be changed to \code{link}, since it currently
  allows to interpret a \code{gamma.mixed} model as any
  other}

  \item{...}{parameter to be passed to the \code{lmer}
  function}

  \item{data}{a data.frame}
}
\value{
  a list specifying '.function'
}
\description{
  Interface between the Zelig Model poisson.mixed and the
  Pre-existing Model-fitting Method
}

