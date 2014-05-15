\name{cr_group}
\alias{cr_group}
\title{Create \code{cr_group} object}
\usage{
cr_group(x, rgroup, cgroup, colnames)
}
\arguments{
  \item{x}{the object (typically a matrix och data frame)}

  \item{rgroup}{the 'rgroup' attribute with length equal to
  \code{dim(object)[1]}}

  \item{cgroup}{the 'cgroup' attribute with length equal to
  \code{dim(object)[2]}}

  \item{colnames}{use this to set colnames (class
  \code{cr_group} can keep track of these under
  permutations (if object is 'data.frame' colnames are
  dropped under permuation)}
}
\description{
Sets the 'rgroup' and 'cgroup' attributes and returns a
'cr_group' object. These can also be set with \code{attr},
but this functions checks that the dimensionality is OK.
}
\note{
This functions is mainly used because of its \code{latex}
method. There is also an indexing method. However, In
RStudio (version Version 0.98.507) \code{'[.cr_group'}
causes an unexpected message "<simpleError in ..." at
times. It is unclear why, as this does not appear in
RConsole or RTerm.
}
\author{
Henrik Renlund
}

