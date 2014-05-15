\name{[.cr_group}
\alias{[.cr_group}
\title{Index a 'cr_group' object}
\usage{
\method{[}{cr_group}(x, i, j)
}
\arguments{
  \item{x}{an 'cr_group' object}

  \item{i}{first index}

  \item{j}{second index}

  \item{...}{arguments to be passed to \code{'['}}
}
\description{
This method makes sure that attributes 'rgroup', 'cgroup'
and 'colnames' are intact after permutation.
}
\note{
In RStudio (version Version 0.98.507) \code{'[.cr_group'}
causes an unexpected message "<simpleError in ..." at
times. It is unclear why, as this does not appear in
RConsole or RTerm.
}
\author{
Henrik Renlund
}

