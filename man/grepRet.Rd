\name{grepRet}
\alias{grepRet}
\title{Look for a pattern in a character vector and return the hits}
\usage{
grepRet(pattern, x, index = FALSE, ...)
}
\arguments{
  \item{pattern}{pattern to look for}

  \item{x}{character string to inspect}

  \item{index}{logical; whether to also return indexes of
  hits (defaul FALSE)}

  \item{...}{arguments to be passed to \code{\link{grep}}}
}
\description{
This is a wrapper for \code{\link{grep}} to return not only
the indexes of the hits (optional), but the hits
themselves.
}
\examples{
grepRet(pattern="a", x=names(datasets::mtcars))
}
\author{
Henrik Renlund
}
\seealso{
\code{\link{grep}}, \code{\link{grepl}}
}

