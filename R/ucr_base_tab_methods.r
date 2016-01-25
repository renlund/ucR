#' @title Produce latex code from a \code{ucr.base.tab} object
#'
#' @description A simple wrapper for the Hmisc latex function,
#' that adds an explanatory bottom text and extra
#' column headings.
#'
#' @author Lars Lindhagen
#' @param object a \code{ucr.base.tab} object
#' @param ... arguments to be passed to \code{latex}
#' @seealso \code{\link{ucr.base.tab}}
#' @importFrom Hmisc latex
#' @export

latex.ucr.base.tab <- function(object, ...) {
  # --> Add bottom text.
  # Explain notation.
  bot <- "" # Bottom text.
  if (object$exists.numeric) { # Add text explaining numerical variables if any.
    median.eq <- "$m$ ($a$ -- $b$)"
    if (object$median.format == "iqr") {
      median.txt <- "median (Q$_1$ -- Q$_3$)"
    } else if (object$median.format == "range") {
      median.txt <- "median (min -- max)"
    } else {
      # User-specified quantiles.
      median.txt <- sprintf("median (%dth -- %dth percentile)",
        round(object$median.format), round(100 - object$median.format))
    }
    if (object$mean.format == "pm") {
      mean.eq <- "$x$ $\\pm$ $s$"
      mean.txt <- "mean $\\pm$ SD"
    } else {
      mean.eq <- "$x$ ($s$)"
      mean.txt <- "mean (SD)"
    }
    if (object$num.format == "median") {
      num.txt <- sprintf("%s represents %s", median.eq, median.txt)
    } else if (object$num.format == "mean") {
      num.txt <- sprintf("%s represents %s", mean.eq, mean.txt)
    } else {
         num.txt <- sprintf("%s \\{%s\\} represents %s \\{%s\\}", median.eq, mean.eq, median.txt, mean.txt)
    }
    bot <- sprintf("%s \n\n %s.", bot, num.txt)
  }
  if (object$exists.factor.perc) {
    if (object$factor.format == "count.perc") {
      bot <- sprintf("%s \n\n $n$ ($p$%s) represent frequency (percentage).",
        bot, object$perc.sign)
    } else {
      bot <- sprintf("%s \n\n $p$%s ($n$) represent percentage (frequency).",
        bot, object$perc.sign)
    }
  }
  # Explain percentages. If there are no groups, there is not much to explain...
  if (object$exists.factor.perc & object$exists.groups) {
    if (object$perc.method == "group") {
      bot <- sprintf("%s Percentages computed by group.", bot)
    } else if (object$perc.method == "level") {
      bot <- sprintf("%s Percentages computed by level.", bot)
    } else {
      bot <- sprintf("%s Percentages computed by group and level.", bot)
    }
  }
  if (object$exists.factor.noperc) {
    bot <- sprintf("%s \n\n Plain numbers are frequencies.", bot)
  }
  # Explain mising notation.
  if (object$has.missing.in.row) {
    bot <- sprintf("%s \n\n $[M]$ represents number of missings.", bot)
  }
  # Explain the tests used.
  if (any(!is.na(object$test.names))) { # Any test used at all?
    bot <- sprintf("%s \n\n Tests used: ", bot)
    for (i in 1:2) {
      if (!is.na(object$test.names[i])) { # Test 'i' used?
        if (i == 2) {
          bot <- sprintf("%s; ", bot) # Add a semicolon between the test texts.
        }
        bot <- sprintf("%s$^%d$%s", bot, i, object$test.names[i])
      }
    }
    bot <- sprintf("%s.", bot) # Add a final period.
  }
  # Substitutions of special LaTeX symbols.
  object$tab <- gsub("_", "\\\\_", object$tab) # Change all '_' to '\_'.
  object$tab <- gsub("%", "\\\\%", object$tab) # Change all '%' to '\%'.
  bot <- gsub("%", "\\\\%", bot) # Change '%' to '\%' for bottom text too.
   dummy <- Hmisc::latex(object$tab, insert.bottom=bot,
    col.just=rep("l", times=ncol(object$tab)),
    collabel.just=rep("l", times=ncol(object$tab)),
    extracolheads=object$extra.col.heads,
    ...)
  invisible (NULL)
}

#' @title Splits a table into several sub-tables.
#'
#' @description Splits a baseline table into a number of sub-tables, each
#' containing a subset of the columns.
#'
#' @author Lars Lindhagen
#' @param object An object of class ucr.base.tab.
#' @param group.partition A list specifying how to split the table. Each entry
#'                        in the list is an array of group numbers.
#' @param always.n If \code{TRUE}, then group size is included in all tables.
#' @seealso \code{\link{ucr.base.tab}}
#' @examples
#' # Suppose that u is an object of class ucr.base.tab with 10 groups,
#' # and one wants to split it into three sub-tables as follows:
#' #    Sub-table 1: Groups 1, 2, 3.
#' #    Sub-table 2: Groups 4, 5, 6, 7.
#' #    Sub-table 3: Groups 8, 9, 10.
#' # The following call achieves this:
#' #  split(u, list(1:3, 4:7, 8:10))
#' @return A list of "smaller" \code{ucr.base.tab} objects.
#' @export

split.ucr.base.tab <- function(object, group.partition, always.n=F) {
  fgc <- object$first.group.col # Short-hand.
  lgc <- fgc + object$n.groups - 1 # Column of last group.
  nc <- ncol(object$tab) # Total number of columns.
  res <- list()
  for (i in 1:length(group.partition)) {
    cur.obj <- object # Current sub-table.
    if ((i == 1) || always.n) {
      # First sub-table or N requested. Include N column (if any).
      cur.cols <- 1:(fgc-1)
    } else {
      cur.cols <- 1 # Just "Variable" column.
    }
    cur.cols <- c(cur.cols, fgc + group.partition[[i]] - 1) # Selected groups.
    if (i == length(group.partition)) {
      # Last sub-table, include Combined column and P-values (if any).
      if (lgc < nc) {
        cur.cols <- c(cur.cols, ((lgc+1):nc))
      }
    }
    # Keep only correct columns.
    cur.obj$tab <- cur.obj$tab[, cur.cols]
    cur.obj$extra.col.heads <- cur.obj$extra.col.heads[cur.cols]
    res[[i]] <- cur.obj # Append sub-table to result list.
  }
  return (res)
}


# Sanity checks for parameters to the ucr.base.tab function.
ucr.internal.base.tab.parameter.check <- function(data, group.name,
  combined.name, x.names, num.format, median.format, mean.format, factor.format,
  perc.method, print.perc, print.perc.space, omit.ref.level, show.missing, digits, spec.digits,
  include.n, include.p, test.x.names, num.test, factor.test, min.p) {

  # --> Utility functions.
  # Tests if x is a non-negative integer.
  is.posint <- function(x) {
    ret <- is.numeric(x) && (length(x) == 1) && !is.na(x) && (x >= 0) &&
      (x == round(x))
    return (ret)
  }
  # Tests if x is a simple string.
  is.string <- function(x) {
    ret <- is.character(x) && (length(x) == 1) && !is.na(x)
    return (ret)
  }
  # Tests if x is a simple numerical number.
  is.num <- function(x) {
    ret <- is.numeric(x) && (length(x) == 1) && !is.na(x)
    return (ret)
  }
  # Checks that x is a simple logical value. If not, an error is generated.
  assert.logical <- function(x) {
    x.name <- as.character(as.list(sys.call())[[2]]) # Name of x at caller.
    ok <- is.logical(x) && (length(x) == 1) && !is.na(x)
    if (!ok) {
      stop(sprintf("%s must be a single logical value", x.name))
    }
  }
  # Checks that x is a string and belongs to the given set of accepted strings.
  # If not, an error is generated.
  assert.string.in.set <- function(x, acc.set) {
    x.name <- as.character(as.list(sys.call())[[2]]) # Name of x at caller.
    if (!is.string(x)) {
      stop(sprintf("%s must be a string", x.name))
    }
    if (!is.element(x, acc.set)) {
      stop(sprintf("%s must be one of: %s", x.name, paste(acc.set, collapse=", ")))
    }
  }
  # Checks that the numerical value x lies strictly between a and b.
  # If not, an error is generated.
  assert.between <- function(x, a, b) {
    x.name <- as.character(as.list(sys.call())[[2]]) # Name of x at caller.
    if ((x <= a) || (x >= b)) {
      stop(sprintf("%s must lie strictly between %.1f and %.1f", x.name, a, b))
    }
  }

  if (!"data.frame" %in% class(data)) { # with packages like e.g. dplyr a data.frame may have several classes
    stop("data must be a data frame.")
  }
  if (nrow(data) == 0) {
    stop("Empty data frame.")
  }
  n.groups <- 0 # Until further notice. Useful for later checks.
  if (!is.null(group.name)) {
    if (!is.string(group.name)) {
      stop("group.name must be a string or NULL.")
    }
    if (!is.element(group.name, names(data))) {
      stop(sprintf("Group variable '%s' does not exist.", group.name))
    }
    if (any(is.na(data[[group.name]]))) {
      stop(sprintf("There are NA's in the group variable '%s'.", group.name))
    }
    if (!is.element("factor", class(data[[group.name]]))) {
      stop("Group variable must be a factor")
    }
    n.groups <- length(unique(data[[group.name]]))
  }
  if (!is.character(x.names)) {
    stop("x.names must be a string.")
  }
  if (length(x.names) == 0) {
    stop("No x variables supplied.")
  }
  if (!all(is.element(x.names, names(data)))) {
    ix <- which(!is.element(x.names, names(data))) # Non-existing variables.
    ix <- ix[1] # Pick the first non-existing variable for error text.
    stop(sprintf("x variable '%s' does not exist.", x.names[ix]))
  }
  if (max(summary(factor(x.names))) > 1) {
    tmp.sum <- summary(factor(x.names))
    ix <- which.max(tmp.sum)
    stop(sprintf("x variable '%s' included several times.", names(tmp.sum[ix])))
  }
  for (n in x.names) {
    if (!is.numeric(data[[n]]) && !is.factor(data[[n]])) {
      stop(sprintf("Variable %s is neither numeric nor a factor", n))
    }
  }
  if (!is.string(combined.name)) {
    stop("combined.name must be a string.")
  }
  assert.string.in.set(num.format, c("median", "mean", "both"))
  if (is.num(median.format)) {
    assert.between(median.format, 0, 50)
  } else {
    assert.string.in.set(median.format, c("iqr", "range"))
  }
  assert.string.in.set(mean.format, c("pm", "par"))
  assert.string.in.set(factor.format, c("count.perc", "perc.count"))
  assert.string.in.set(perc.method, c("group", "level", "total"))
  assert.logical(print.perc)
  assert.logical(print.perc.space)
  assert.logical(omit.ref.level)
  assert.string.in.set(show.missing, c("none", "in.row", "sep.row"))
  if (!is.posint(digits)) {
    stop("digits must be a positive integer.")
  }
  if (is.list(spec.digits)) {
    if (!all(is.element(names(spec.digits), x.names))) {
      stop("Names of spec.digits must be valid variable names")
    }
    for (ix in length(spec.digits)) {
      if (!is.posint(spec.digits[[ix]])) {
        stop(sprintf("spec.digits is not a non-negative integer for variable '%s'",
          names(spec.digits)[ix]))
      }
    }
  } else if (!is.null(spec.digits)) {
    stop(sprintf("Illegal class '%s' for spec.digits", class(digits)))
  }
  assert.logical(include.n)
  if (!is.character(test.x.names)) {
    stop("test.x.names must be a string.")
  }
  if (!all(is.element(test.x.names, x.names))) {
    stop("test.x.names must be a subset of x.names")
  }
  assert.logical(include.p)
  assert.string.in.set(num.test, c("nonparam", "param", "nonparam.trend",
    "param.trend"))
  assert.string.in.set(factor.test, c("pearson", "fisher", "trend"))
  if (!is.string(min.p) || is.na(as.numeric(min.p))) {
    stop("min.p must be a string containing a number, e.g. '0.001'")
  }
  # Require 3+ groups for trend tests. But allow no groups (tests will be
  # ignored with warning).
  if (is.element(num.test, c("nonparam.trend", "param.trend")) ||
      is.element(factor.test, "trend")) {
    if ((n.groups != 0) && (n.groups < 3)) {
      stop("Need 3+ groups for trend tests.")
    }
  }
  # No problems, just return!
}
