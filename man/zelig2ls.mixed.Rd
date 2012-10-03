\name{zelig2ls.mixed}
\alias{zelig2ls.mixed}
\title{Interface between the Zelig Model ls.mixed and
the Pre-existing Model-fitting Method}
\usage{
  zelig2ls.mixed(formula, family, ..., data)
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
  Interface between the Zelig Model ls.mixed and the
  Pre-existing Model-fitting Method
}

