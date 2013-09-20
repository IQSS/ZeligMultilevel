\name{zelig2ls.mixed}
\alias{zelig2ls.mixed}
\title{Interface between the Zelig Model ls.mixed and
the Pre-existing Model-fitting Method}
\usage{
  zelig2ls.mixed(formula, family, ..., data)
}
\arguments{
  \item{formula}{a formula}
  \item{family}{the \code{family} argument is not an allowable parameter, and exists only to
  	warn against accidental usage, since it exist in other models in ZeligMultilevel pacakge.}

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

