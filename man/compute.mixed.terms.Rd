\name{compute.mixed.terms}
\alias{compute.mixed.terms}
\title{Compute Random and Fixed Terms}
\usage{
  compute.mixed.terms(form, x, data)
}
\arguments{
  \item{form}{a \code{formula} whose parameters will be
  categorized as "fixed" or "random"}

  \item{x}{a \code{setx} object}

  \item{data}{a \code{data.frame}}
}
\value{
  a \code{list} containing the entries "f.terms" specifying
  fixed terms and "r.terms" specifying random terms
}
\description{
  Compute Random and Fixed Terms
}
\note{
  This function is used internally by \code{mixed.zelig}
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

