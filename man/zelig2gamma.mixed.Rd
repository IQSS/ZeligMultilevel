\name{zelig2gamma.mixed}
\alias{zelig2gamma.mixed}
\title{Interface between the Zelig Model gamma.mixed and
the Pre-existing Model-fitting Method}
\usage{
  zelig2gamma.mixed(formula,
    family = Gamma(link = "identity"), ..., data)
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
  Interface between the Zelig Model gamma.mixed and the
  Pre-existing Model-fitting Method
}
\author{
  Matt Owen and Ferdinand Alimadhi and Delia
}

