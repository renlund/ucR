\name{varFind}
\alias{varFind}
\title{Look for a pattern in the \code{names} of R objects (typically data frames) and
return the hits}
\usage{
varFind(pattern, dfs, index = FALSE, ...)
}
\arguments{
  \item{pattern}{pattern to look for}

  \item{dfs}{character string to of R objects}

  \item{index}{logical; whether to also return indexes of
  hits (defaul FALSE)}

  \item{...}{arguments to be passed to \code{\link{grep}}}
}
\description{
This function applies \code{\link{grepRet}} to
\code{\link{names}} of a given set of R objects and returns
the results as a list
}
\examples{
require(datasets)
varFind(pattern="a", dfs=c("mtcars", "esoph"), index=TRUE)
}
\author{
Henrik Renlund
}
\seealso{
\code{\link{grepRet}}, \code{\link{grep}}
}

