# Sanity checks for parameters to the ucr.base.tab function.
ucr.internal.model.tab.parameter.check <- function(data) {
}

#' @title Combine two or more model tables.
#'
#' @description Combine two or more model tables. The tables are displayes
#'              in separate columns. The tables are supposed to be identical
#'              except for the regression results.
#'
#' @author Lars Lindhagen
#' @param ... a number of \code{ucr.model.tab} objects.
#' @return Return a new \code{ucr.model.tab} object with more columns.
#' @seealso \code{\link{ucr.model.tab}}
#' @export

# Combines two or more model tables.
cbind.ucr.model.tab <- function(...) {
  obj.list <- list(...)
  if (length(obj.list) < 2) {
    stop("Needs at least two tables for cbind") # Error.
  }
  ret <- obj.list[[1]]
  ncol.basic <- ret$ncol.basic
  for (i in 2:length(obj.list)) {
    obj <- obj.list[[i]]
    if (obj$ncol.basic != ncol.basic) {
      stop("Conflicting number of basic columns") # Error.
    }
    if (!identical(ret$tab[, 1:ncol.basic], obj$tab[, 1:ncol.basic])) {
      stop("Conflicting variable names") # Error.
    }
    ret$tab <- cbind(ret$tab, obj$tab[, -(1:ncol.basic)])
    ret$n.models <- ret$n.models + obj$n.models
    ret$extra.col.heads <- c(ret$extra.col.heads, obj$extra.col.heads[-(1:ncol.basic)])
    if (obj$n.models == 1) {
      colnames(ret$tab)[ncol(ret$tab)] <- colnames(obj$tab)[ncol.basic + 1]
    }
  }
  return (ret)
}

#' @title Produce latex code from a \code{ucr.model.tab} object
#'
#' @description A simple wrapper for the Hmisc latex function,
#' that adds extra column headings.
#'
#' @author Lars Lindhagen
#' @param object a \code{ucr.model.tab} object
#' @param ... arguments to be passed to \code{latex}
#' @seealso \code{\link{ucr.model.tab}}
#' @importFrom Hmisc latex
#' @export

latex.ucr.model.tab <- function(object, ...) {
  # Substitutions of special LaTeX symbols.
  object$tab <- gsub("_", "\\\\_", object$tab) # Change all '_' to '\_'.
  object$tab <- gsub("%", "\\\\%", object$tab) # Change all '%' to '\%'.
  object$extra.col.heads <- gsub("%", "\\\\%", object$extra.col.heads)

  dummy <- Hmisc::latex(object$tab,
    extracolheads=object$extra.col.heads,
    collabel.just=rep("l", ncol(object$tab)),
    col.just=rep("l", ncol(object$tab)),
    ...)
  return (NULL)
}
