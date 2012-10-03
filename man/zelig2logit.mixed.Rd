\name{zelig2logit.mixed}
\alias{zelig2logit.mixed}
\title{Interface between the Zelig Model logit.mixed and
the Pre-existing Model-fitting Method}
\usage{
  zelig2logit.mixed(formula,
    family = binomial(link = "logit"), ..., data)
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
  Interface between the Zelig Model logit.mixed and the
  Pre-existing Model-fitting Method
}
\author{
  Matt Owen and Ferdinand Alimadhi and Delia
}

