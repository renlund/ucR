\name{latex.cr_group}
\alias{latex.cr_group}
\title{Latex method for objects of class \code{cr_group}}
\usage{
\method{latex}{cr_group}(object, r.perm = "as.is", c.perm = "as.is",
  colheads = TRUE, file = "", title = "", ...)
}
\arguments{
  \item{object}{an 'cr_group' object}

  \item{r.perm}{is either (interpreted as) 'as.is',
  'alphabetical' or a permutation of
  \code{sort(unique(attr(object, "rgroup")))}. If 'as.is'
  row groups are ordered as they appear in 'rgroups', if
  'alphabetical' they appear in alphabetical order,
  otherwise in the permutation given.}

  \item{c.perm}{is analogous to \code{r.perm}}

  \item{colheads}{is an argument that if 'character' is
  passed to \code{latex} but if TRUE will set 'colheads'
  (in \code{latex}) to the objects attribute 'colnames' (if
  possibly, else \code{dimnames(object)[[2]]})}

  \item{file}{is an argument passed to \code{latex}
  (default "")}

  \item{title}{is an argument passed to \code{latex}
  (default "")}

  \item{...}{additional arguments passed to \code{latex}}
}
\description{
Objects of class \code{cr_group} have attributes 'rgroup'
and/or 'cgroup'. These are extracted for use in the
\code{latex} function of package \code{Hmics}
}
\examples{
nr <- 7; nc <- 5; M <- matrix(1:(nr*nc), nrow=nr, byrow=TRUE)
rownames(M) <- letters[1:nr]
colnames(M) <- LETTERS[1:nc]
attr(M, "rgroup") <- rep(c("foo", "bar", "baz"), length.out=nr)
attr(M, "cgroup") <- rep(c("Fuzzy", "Busy"), length.out=nc)
class(M) <- "cr_group"
dummy <- latex(M, r.perm='as.is', c.perm='alpha')
# see vignette
}
\author{
Henrik Renlund
}
\seealso{
\code{\link{latex}}, \code{\link{cr_group}}
}

