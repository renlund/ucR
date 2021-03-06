% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ucr_base_tab_methods.r
\name{split.ucr.base.tab}
\alias{split.ucr.base.tab}
\title{Splits a table into several sub-tables.}
\usage{
\method{split}{ucr.base.tab}(object, group.partition, always.n = F)
}
\arguments{
\item{object}{An object of class ucr.base.tab.}

\item{group.partition}{A list specifying how to split the table. Each entry
in the list is an array of group numbers.}

\item{always.n}{If \code{TRUE}, then group size is included in all tables.}
}
\value{
A list of "smaller" \code{ucr.base.tab} objects.
}
\description{
Splits a baseline table into a number of sub-tables, each
containing a subset of the columns.
}
\examples{
# Suppose that u is an object of class ucr.base.tab with 10 groups,
# and one wants to split it into three sub-tables as follows:
#    Sub-table 1: Groups 1, 2, 3.
#    Sub-table 2: Groups 4, 5, 6, 7.
#    Sub-table 3: Groups 8, 9, 10.
# The following call achieves this:
#  split(u, list(1:3, 4:7, 8:10))
}
\seealso{
\code{\link{ucr.base.tab}}
}
\author{
Lars Lindhagen
}
