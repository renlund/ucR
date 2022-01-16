#' @title Create a baseline table
#'
#' @description Creates a baseline table from a data frame. The data may
#' be partitioned in two or more groups. A column for the
#' "combined" group (all data) can also be included.
#'
#' @author Lars Lindhagen
#' @param data The data frame.
#' @param group.name Name of the variable in 'data' that defines the groups, or NULL
#'              (default) if there are no groups. The group variable must be a
#'              factor without missing..
#' @param combined.name Column heading for the combined group. Defaults to
#'       "Combined".
#' @param x.names Names and order of the variables in 'data' that shall be
#'                 included in the table. By default, all variables except
#'                 the group variable, if any, are included.
#' @param num.format How to present numerical variables. Allowed values:
#'              \itemize{
#'                \item "median": Median with IQR or range.
#'              \item "mean": Mean with standard deviation.
#'              \item "both": Both. This gives "median stuff (mean stuff)".
#'              }
#' @param median.format What measure of dispersion to give together with medians.
#'                 Allowed values:
#'                 \itemize{
#'                \item "iqr": Interquartile range (Q1 and Q3).
#'                 \item "range": Range (min and max).
#'                 \item A numerical value strictly between 0 and 50.
#'                   Example: If median.format = 10, then the 10th and 90th
#'                   percentiles will be reported.
#'                   }
#' @param mean.format How to present standard deviations. Allowed values:
#'               \itemize{
#'                \item "par": Mean (SD).
#'               \item "pm": Mean plus/minus SD.
#'               }
#' @param factor.format How to present factor variables. Allowed values:
#'                 \itemize{
#'                \item count.perc: Count (percentage).
#'                 \item perc.count: Percentage (count).
#'                 }
#' @param perc.method How to compute percentages for factor variables. Allowed values:
#'               \itemize{
#'                \item "group": Computes n(group, level) / n(group).
#'               \item "level": Computes n(group, level) / n(level).
#'                 For the combined group, this would always give 100\%.
#'                 Hence, percentages are omitted in this case.
#'               \item "total": Percentage  n(group, level) / n.
#'               }
#' @param print.perc TRUE if a percent sign ('\%') is to be printed for percentages.
#' @param print.perc.space TRUE if a space is to be printed between the number and the
#'                    percent sign, i.e. "12.3 \%" rather than "12.3\%".
#'                    N/A if percent sign is disabled.
#' @param omit.perc.decimal TRUE if the decimal is to be omitted for percentages
#'                    above 1\%, e.g. 13\% rather than 13.3\%.
#' @param use.texttt logical; use latex texttt
#' @param omit.ref.level TRUE if the reference (first) level of dichotomous factor
#'                  variables should be omitted from the table.
#' @param separate.factor.row TRUE if a separate row is added (first) for each
#'                  factor, containing the variable name, number of
#'                  inidividuals/missing etc.
#' @param show.missing How should missing values, if any, per group be shown?
#'                \itemize{
#'                \item "none": Don't show missing values.
#'                \item "in.row": Show the number of missing values within square
#'                  brackets after the usual data.
#'                \item "sep.row": Show the number of missing values is a separate
#'                  row.
#'                  }
#' @param digits An integer, giving the number of decimals for numeric
#'          variables, unless otherwise specified through the parameter
#'          'spec.digits'.
#' @param spec.digits A list, whose names form a subset of 'x.names', and whose values
#'               are the number of decimals for those (numeric) variables. The
#'               value NULL causes 'digits' to be used throughout.
#' @param include.combined TRUE if the combined group should be included in a
#'                    separate column.
#' @param include.n TRUE if number of valid (non-NA) observations per variable should
#'             be included in a separate column.
#' @param include.p TRUE if P-values are to be included in the table. They refer to
#'             univariate tests for identical distribution in the different
#'             groups. P-values can only be included if there are groups.
#' @param test.x.names Names of variables to perform tests for.
#' @param num.test Which kind of test to use for numerical variables. Allowed
#'                  values:
#'            \itemize{
#'            \item "nonparam": A non-parametrical test:
#'            \itemize{
#'                \item Wilcoxon (Mann-Whitney) test for two groups.
#'                \item Kruskal-Wallis test for three or more groups.
#'                }
#'            \item "param": A parametrical test:
#'                \itemize{
#'                \item t-test for two groups.
#'                \item ANOVA for three or more groups.
#'                }
#'            \item "nonparam.trend": The non-parametric Jonckheere-Terpstra
#'                   trend test, treating the group variable as ordinal. Only
#'                   for 3+ groups.
#'            \item "param.trend": A parametric trend test (likelihood ratio
#'                   test for simple linear regression), converting the group
#'                   variable to linear scores. Only for 3+ groups.
#'                }
#' @param factor.test Which test to use for factor variables. Allowed values:
#'               \itemize{
#'               \item "fisher": Fisher's exact test.
#'               \item "pearson": Pearson's chi2 test.
#'               \item "trend": Linear-by-linear association test, converting
#'                     both the group variable and the baseline variable
#'                     (possibly dichotomous) to linear scores. Only for 3+
#'                     groups.
#'               }
#' @param min.p Smallest P-value to be displayed. If smaller, then a text like
#'         "< 0.001" is given.
#'         NOTE: This value must be a string, e.g. "0.001" or "1e-3".
#' @return The return value is an S3 object of class ucr.base.tab.
#' @export
ucr.base.tab <- function(data, group.name=NULL, combined.name="Combined",
  x.names=setdiff(names(data), group.name), num.format="median",
  median.format="iqr", mean.format="par", factor.format="count.perc",
  perc.method="group", print.perc=T, print.perc.space=F, omit.perc.decimal=F,
  use.texttt=F, omit.ref.level=F, separate.factor.row=F, show.missing="none",
  digits=1, spec.digits=NULL, include.combined=T, include.n=T, include.p=T,
  test.x.names=x.names,
  num.test="nonparam", factor.test="fisher", min.p="0.001") {

  # --> Check that parameters are sane. If not, execution will be stopped.
  ucr.internal.base.tab.parameter.check(
    data=data, group.name=group.name, combined.name=combined.name,
    x.names=x.names, num.format=num.format, median.format=median.format,
    mean.format=mean.format, factor.format=factor.format,
    perc.method=perc.method, print.perc=print.perc,
    print.perc.space=print.perc.space,
    omit.perc.decimal=omit.perc.decimal, omit.ref.level=omit.ref.level,
    separate.factor.row=separate.factor.row, show.missing=show.missing,
    digits=digits, spec.digits=spec.digits, include.n=include.n,
    include.p=include.p, test.x.names=test.x.names, num.test=num.test,
    factor.test=factor.test, min.p=min.p)
  # Make 'spec.digits' a complete list, replacing missing values by 'digits'.
  if (is.null(spec.digits)) {
    spec.digits <- list()
  }
  for (v in x.names) {
    if (is.null(spec.digits[[v]])) {
      spec.digits[[v]] <- digits # Default value for unspecified variables.
    }
  }
  # Decide how to print percentages.
  if (print.perc) {
    if (print.perc.space) {
      perc.sign <- " %" # E.g. 12.3 %
    } else {
      perc.sign <- "%" # E.g. 12.3%
    }
  } else {
    perc.sign <- "" # E.g. 12.3
  }

  if (!is.null(group.name)) { # Include groups?
    include.groups <- TRUE
    group.sum <- summary(data[[group.name]])
    group.names <- names(group.sum)
    n.groups <- length(group.sum)
  } else { # No groups.
    include.groups <- FALSE
    group.sum <- NA # Set to NA for safety.
    group.names <- NA
    n.groups <- 0
  }
  if (include.p && !include.groups) {
    # Pointless performing tests if no groups. Warn user and skip test.
    include.p <- FALSE
    warning("Skips tests since there are no groups")
  }

  # Define columns. For simplicity, we create the table assuming full number of
  # columns, including P-values. Undesired columns are removed later.
  # The following column is used:
  #    - Variable name.
  #    - Number of observations.
  #    - Groups. One column for each group. Combined group last. If there are
  #              no groups, the combined group is the only one.
  colno.var <- 1 # Column for variable name.
  colno.n <- 2 # Column for number of observations.
  # Column for "group 0". Group number g (= 1, 2, ...) has this number + g.
  colno.group0 <- 2
  if (include.groups) {
    colno.combined <- colno.group0 + n.groups + 1 # Combined group.
  } else {
    colno.combined <- 3
  }
  colno.p <- colno.combined + 1 # P-values.
  n.cols <- colno.p # Number of columnms (also last column).

  # Error handler for tests.
  test.error.handler <- function(x) {
    warning(sprintf("Error in test for variable %s, skips test.\n", cur.x.name))
    return (NA)
  }

  # --> Create large text matrix for the table.
  tab.mat <- matrix("", ncol=n.cols, nrow=0)
  extra.col.heads <- rep("", times=n.cols) # Extra column heads.
  # Create columns headings and subheadings.
  dimnames(tab.mat)[[2]] <- rep("", times=ncol(tab.mat))
  dimnames(tab.mat)[[2]][1] <- "Variable"
  dimnames(tab.mat)[[2]][2] <- "$N$"
  dimnames(tab.mat)[[2]][colno.p] <- "$P$-value"
  for (g in 0:n.groups) { # 0 = Combined, 1 = first group etc.
    if (g == 0) { # Combined (=all)?
      cur.n <- nrow(data) # All data in combined group.
      cur.col <- colno.combined
      cur.name <- combined.name
    } else { # One of the subgroups.
      ix <- which(data[[group.name]] == group.names[g])
      cur.n <- length(ix)
      cur.col <- colno.group0 + g
      cur.name <- group.names[g]
    }
    dimnames(tab.mat)[[2]][cur.col] <- cur.name
    extra.col.heads[cur.col] <- sprintf("$N = %d$", cur.n)
  }

  # Book-keeping variables.
  n.tests.defined <- 0 # No tests yet.
  # 0 = no numeric test, 1 = first test, 2 = second test (factor test was used first).
  numeric.test.ix <- 0
  factor.test.ix <- 0
  test.names <- c(NA, NA)
  exists.numeric <- FALSE # Have we seen any numeric variables yet?
  exists.factor.perc <- FALSE # Have we seen any factor variables with percent yet?
  exists.factor.noperc <- FALSE # Without percent?
  has.missing.in.row <- F # Any missings shown in same row as data?

  # --> Loop over variables.
  for (i in 1:length(x.names)) {
    cur.x.name <- x.names[i] # Name of current x variable.
    cur.x <- data[[cur.x.name]] # Current x variable.
    if (Hmisc::label(cur.x) != "") {
      cur.x.label <- Hmisc::label(cur.x) # Variable label.
    } else {
      # No label, use variable name instead, possibly using typewriter font.
      if (use.texttt) {
        cur.x.label <- sprintf("\\texttt{%s}", cur.x.name)
      } else {
        cur.x.label <- cur.x.name
      }
    }
    if (is.numeric(cur.x)) {
      # -----> Numeric variable.
      # Remove any label in 'cur.x' to avoid some slowness, particularly in
      # kruskal.test.
      cur.x <- as.numeric(cur.x)
      cur.row <- matrix("", ncol=n.cols, nrow=1) # New row to be added to table.
      cur.row[colno.var] <- cur.x.label
      cur.row[colno.n] <- length(which(!is.na(cur.x))) # Number of non-NA's.
      n.miss.per.group <- rep("", times=n.cols) # Strings with number of missings per group.

      for (g in 0:n.groups) { # 0 = Combined, 1 = first group etc.
        if (g == 0) { # Combined (=all)?
          cur.x.group <- cur.x # Posts for group (all data for combined group).
          cur.col <- colno.combined # Column for this group.
        } else { # One of the subgroups.
          ix <- which(data[[group.name]] == group.names[g])
          cur.x.group <- cur.x[ix] # Correct subset.
          cur.col <- colno.group0 + g
        }
        n.miss.per.group[cur.col] <- as.character(length(which(is.na(cur.x.group))))
        if (!all(is.na(cur.x.group))) { # Check that there is some non-NAs.
          if (median.format == "iqr") {
            quant.probs <- c(0.25, 0.75)
          } else if (median.format == "range") {
            quant.probs <- c(0, 1)
          } else {
            # Numerical value, e.g. 10. If so, report 10th and 90th percentiles.
            perc.lo <- median.format / 100
            perc.hi <- 100 - perc.lo
            quant.probs=c(median.format / 100, 1 - median.format / 100)
          }
          x.quant <- stats::quantile(cur.x.group, probs=quant.probs, na.rm=T)
          x.lo <- x.quant[1]
          x.hi <- x.quant[2]
          x.median <- stats::median(cur.x.group, na.rm=T)
          x.mean <- mean(cur.x.group, na.rm=T)
          x.sd <- stats::sd(cur.x.group, na.rm=T)
          cur.digits <- spec.digits[[cur.x.name]] # Decimals for this variable.
          # String for median stuff.
          median.string <- sprintf("%.*f (%.*f -- %.*f)", cur.digits, x.median,
            cur.digits, x.lo, cur.digits, x.hi)
          if (mean.format == "par") { # String for mean stuff.
            # Mean (SD).
            mean.string <- sprintf("%.*f (%.*f)",
              cur.digits, x.mean, cur.digits, x.sd)
          } else {
            # Mean plus/minus SD.
            mean.string <- sprintf("%.*f $\\pm$ %.*f",
              cur.digits, x.mean, cur.digits, x.sd)
          }
          if (num.format == "median") { # Total string.
            cur.row[cur.col] <- median.string
          } else if (num.format == "mean") {
            cur.row[cur.col] <- mean.string
          } else {
            # Both median and mean.
            cur.row[cur.col] <- sprintf("%s \\{%s\\}", median.string, mean.string)
          }
        } else {
          cur.row[cur.col] <- "---" # Only NA's.
        }
        exists.numeric <- TRUE
      } # End of long loop over groups.
      if (include.p && is.element(cur.x.name, test.x.names)) {
        # P-value.
        # First time here? If so, decide which test to use.
        if (numeric.test.ix == 0) {
          n.tests.defined <- n.tests.defined + 1
          numeric.test.ix <- n.tests.defined
          if (num.test == "nonparam") {
            # Non-parametrical test: Wilcoxon/Kruskal-Wallis.
            if (n.groups == 2) {
              test.names[numeric.test.ix] <- "Wilcoxon test"
              num.test.fcn <- function(x, g) stats::wilcox.test(x ~ g)$p.value
            } else {
              test.names[numeric.test.ix] <- "Kruskal-Wallis test"
              num.test.fcn <- function(x, g) stats::kruskal.test(x ~ g)$p.value
            }
          } else if (num.test == "param") {
            # Parametrical test: t-test/ANOVA. Same code, just different names.
            if (n.groups == 2) {
              test.names[numeric.test.ix] <- "$t$-test"
            } else {
              test.names[numeric.test.ix] <- "ANOVA"
            }
            num.test.fcn <- function(x, g) {
              m0 <- stats::lm(x ~ 1)
              m1 <- stats::lm(x ~ g)
              a <- stats::anova(m1, m0)
              p <- a[["Pr(>F)"]][2]
              return (p)
            }
          } else if (num.test == "nonparam.trend") {
            # Non-parametric trend test.
            # Jonckheere-Terpstra test from coin package.
            test.names[numeric.test.ix] <- "Jonckheere--Terpstra trend test"
            num.test.fcn <- function(x, g) {
              g <- as.ordered(g) # Make sure g is ordinal.
              it <- coin::independence_test(x ~ g, ytrafo = rank,
                distribution="asymptotic")
              p <- coin::pvalue(it)
              return (p)
            }
          } else {
            # Parametric trend test. Just linear regression...
            test.names[numeric.test.ix] <- "Linear regression trend test"
            num.test.fcn <- function(x, g) {
              p <- summary(stats::lm(x ~ as.numeric(g)))$coef[2, 4]
              return (p)
            }
          }
        }

        # Test can raise error, e.g. if all non-NA posts belong to the same
        # group. If so, catch the error and let P = NA, giving the string "---".
        p.value <- NA
        p.value <- tryCatch(num.test.fcn(x=cur.x, g=data[[group.name]]),
          error=test.error.handler)
        if (!is.na(p.value)) {
          # P-value exists.
          cur.row[colno.p] <- sprintf("%s$^%d$",
            ucr.format.p(p.value, min.p), numeric.test.ix)
        } else {
          # P-value does not exist.
          cur.row[colno.p] <- "---"
        }
      }
      if (show.missing == "in.row") {
        if (any(!is.element(n.miss.per.group, c("", "0")))) { # Do this only if there are missings.
          n.miss.per.group <- ifelse(n.miss.per.group == "", "",
            sprintf(" [%s]", n.miss.per.group))
          # Keep matrix notation explicit. O/W we end up with an array...
          cur.row[1, ] <- sprintf("%s%s", cur.row[1, ], n.miss.per.group)
          has.missing.in.row <- T
        }
      }
      tab.mat <- rbind(tab.mat, cur.row) # Append new row to table.
    } else { # End of numerical variable block.
      # -----> Factor variable.
      cur.levels <- levels(cur.x)
      cur.n.levels <- length(cur.levels)
      cur.rows <- matrix("", nrow=cur.n.levels, ncol=n.cols)  # New rows to be added to table.
      n.miss.per.group <- rep("", times=n.cols) # Strings with number of missings per group.
      for (j in 1:cur.n.levels) { # Loop over all levels.
        cur.lev <- cur.levels[j] # Current level.
        cur.rows[j, colno.var] <- cur.lev # This will be modified below.
        for (g in 0:n.groups) { # 0 = Combined, 1 = first group etc.
          if (g == 0) { # Combined (= all)?
            cur.x.group <- cur.x # All data.
            cur.col <- colno.combined
          } else { # One of the subgroups.
            ix <- which(data[[group.name]] == group.names[g])
            cur.x.group <- cur.x[ix] # Correct subset.
            cur.col <- colno.group0 + g
          }
          n.miss.per.group[cur.col] <- as.character(length(which(is.na(cur.x.group))))
          count <- length(which(cur.x.group == cur.lev))
          if (perc.method == "group") {
            # Count percentages within each group.
            tot <- length(which(!is.na(cur.x.group))) # Don't count NA's.
          } else if (perc.method == "level") {
            # Count percentages within each factor level.
            tot <- length(which(cur.x == cur.lev))
          } else {
            # Count percentages relative to total population (with well-defined x).
            tot <- length(which(!is.na(cur.x)))
          }
          # Pointless to compute percentages by level for combined group (always 100%).
          if ( (perc.method == "level") & (g == 0) ) {
            cur.rows[j, cur.col] <- sprintf("%d", count)
            exists.factor.noperc <- TRUE
          } else {
            if (tot > 0) { # Avoid division by zero.
              perc <- count / tot * 100 # Percentage.
              perc.digits <- 1 # Until further notice.
              if (omit.perc.decimal && (perc > 1)) {
                perc.digits <- 0
              }
              if (factor.format == "count.perc") {
                cur.rows[j, cur.col] <- sprintf("%d (%.*f%s)", count,
                  perc.digits, perc, perc.sign)
              } else {
                cur.rows[j, cur.col] <- sprintf("%.*f%s (%d)", perc.digits, perc,
                  perc.sign, count)
              }
            } else {
              # Count = total = 0. Percent undefined.
              if (factor.format == "count.perc") {
                cur.rows[j, cur.col] <- "0 (---)"
              } else {
                cur.rows[j, cur.col] <- "--- (0)"
              }
            }
            exists.factor.perc <- TRUE
          }
        } # End of 'g' loop over groups.
      } # End of 'j' loop over factor levels.

      if (omit.ref.level && (cur.n.levels == 2)) {
        cur.rows <- matrix(cur.rows[-1, ], ncol=n.cols) # Remove first row.
      }
      # Add "prefix" to rows: Variable name for first row, and indentation o/w.
      if (separate.factor.row) {
        sep.row <- rep("", ncol(cur.rows))
        sep.row[1] <- sprintf("%s:", cur.x.label)
        for (j in 1:nrow(cur.rows)) {
          prefix <- "\\hspace{1em}"
          cur.rows[j, colno.var] <- sprintf("%s %s", prefix, cur.rows[j, colno.var])
        }
        cur.rows <- rbind(sep.row, cur.rows)
        rownames(cur.rows) <- NULL
      } else {
        for (j in 1:nrow(cur.rows)) {
          if (j == 1) {
            prefix <- sprintf("%s:", cur.x.label)
          } else {
            prefix <- "\\hspace{1em}"
          }
          cur.rows[j, colno.var] <- sprintf("%s %s", prefix, cur.rows[j, colno.var])
        }
      }
      cur.rows[1, colno.n] <- length(which(!is.na(cur.x))) # Number of observations.
      if (include.p && is.element(cur.x.name, test.x.names)) {
        # P-value.
        # First time here? If so, decide which test to use.
        if (factor.test.ix == 0) {
          n.tests.defined <- n.tests.defined + 1
          factor.test.ix <- n.tests.defined
          if (factor.test == "fisher") {
            # Fisher's exact test.
            test.names[factor.test.ix] <- "Fisher's exact test"
            factor.test.fcn <- function(x, g) stats::fisher.test(x, g)$p.value
          } else if (factor.test == "pearson") {
            # Pearson's chi2 test without continuity correction.
            test.names[factor.test.ix] <- "Pearson's $\\chi^2$ test"
            factor.test.fcn <- function(x, g) stats::chisq.test(x, g, correct=F)$p.value
          } else {
            # Trend test. Linear-by-linear association test from coin.
            test.names[factor.test.ix] <- "Linear-by-linear trend test"
            factor.test.fcn <- function(x, g) {
              x <- as.ordered(x) # Make sure both x and g are ordinal.
              g <- as.ordered(g) # Make sure g is ordinal.
              it <- coin::independence_test(x ~ g, teststat="quad",
                distribution="asymptotic")
              p <- coin::pvalue(it)
              return (p)
            }
          }
        }
        # Catch error from test if any, cf. numerical test above.
        p.value <- NA
        p.value <- tryCatch(factor.test.fcn(x=cur.x, g=data[[group.name]]),
          error=test.error.handler)
        if (!is.na(p.value)) {
          # P exists.
          cur.rows[1, colno.p] <- sprintf("%s$^%d$",
            ucr.format.p(p.value, min.p), factor.test.ix)
        } else {
          # P does not exist:
          cur.rows[1, colno.p] <- "---"
        }
      }
      if (show.missing == "in.row") {
        if (any(!is.element(n.miss.per.group, c("", "0")))) { # Do this only if there are missings.
          n.miss.per.group <- ifelse(n.miss.per.group == "", "",
            sprintf(" [%s]", n.miss.per.group))
          cur.rows[1, ] <- sprintf("%s%s", cur.rows[1, ], n.miss.per.group)
          has.missing.in.row <- T
        }
      }
      tab.mat <- rbind(tab.mat, cur.rows) # Append new rows to table.
    } # End of factor variable block.
    if (show.missing == "sep.row") { # Separate row for missings?
      if (any(!is.element(n.miss.per.group, c("", "0")))) { # Do this only if there are missings.
        miss.row <- matrix(n.miss.per.group, nrow=1)
        miss.row[colno.var] <- "\\# missing"
        tab.mat <- rbind(tab.mat, miss.row)
      }
    }
  } # End of looooong 'i' loop over variables.
  # Remove undesire columns. Must start from the right!
  if (!include.p) {
    tab.mat <- tab.mat[, -colno.p] # Remove P-value column.
    extra.col.heads <- extra.col.heads[-colno.p] # Remove subheading.
    test.names <- c(NA, NA) # No tests.
  }
  if (!include.combined) {
    tab.mat <- tab.mat[, -colno.combined] # Remove combined column.
    extra.col.heads <- extra.col.heads[-colno.combined] # Remove subheading.
  }
  if (!include.n) {
    tab.mat <- tab.mat[, -colno.n] # Remove N column.
    extra.col.heads <- extra.col.heads[-colno.n] # Remove subheading.
  }
  # Create return object.
  result <- list()
  result$tab <- tab.mat # The table as a matrix.
  result$extra.col.heads <- extra.col.heads # Group size information.
  result$first.group.col <- ifelse(include.n, 3, 2) # Column for first group.
  result$exists.groups <- include.groups # Are there any groups?
  result$n.groups <- n.groups # Number of groups (zero if no groups).
  result$exists.numeric <- exists.numeric # Is there any numeric variable?
  # Is there any factor variable with percentage?
  result$exists.factor.perc <- exists.factor.perc
  # Is there any factor variable without percentage?
  result$exists.factor.noperc <- exists.factor.noperc
  result$num.format <- num.format # Format of numerical variables.
  result$median.format <- median.format
  result$mean.format <- mean.format
  result$factor.format <- factor.format # Format of factor variables.
  result$perc.method <- perc.method # Percentage method.
  result$perc.sign <- perc.sign # Percent sign.
  result$has.missing.in.row <- has.missing.in.row # Any missings in row?
  result$test.names <- test.names # Names of tests used.
  class(result) <- "ucr.base.tab" # S3 class.
  return (result)
}
