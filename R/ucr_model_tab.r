#' @title Description of analysis models.
#'
#' @description Creates a table to summarize one or several analysis models.
#'
#' @author Lars Lindhagen
#' @param model  The model object, or a list of model objects. The following
#'               classes are recognized: \code{glm}, \code{stripped.glm},
#'               \code{lrm}, \code{cph}, \code{mipo}, \code{glmerMod},
#'               \code{matrix}.
#' @param x.names  Names of variables to be included in the table. Variable
#'                 names (in data frame), not coefficient names.
#' @param data  A data frame, typically the one that the model was built on.
#'              Used to extract coefficient names.
#' @param model.name  The name of the model. Used as column heading.
#' @param rm.intercept  A string specifying the name of the intercept term,
#'                      which will be removed from the tables. If one wants to
#'                      keep the intercept, the parameter should be set to
#'                      \code{NULL}.
#' @param allow.missing.coef  Logical telling whether missing (absent)
#'                            coefficients are allowed. If so, the corresponding
#'                            table entry will be empty. O/W, the program will
#'                            stop with an error.
#' @param allow.duplicated.coef  Logical telling whether duplicated coefficient
#'                               names are allowed in the model. Occasionally,
#'                               this can be handy, e.g. for penalized terms in
#'                               a Cox model.
#' @param factor.sep  Separation between factor names and levels. Empty string
#'                    for classical R models (\code{glm} etc.), but '='
#'                    for rms models, corresponding to typical coefficient
#'                    names like \code{genderMale} or \code{gender=Male}.
#' @param link.fcn  A link function, e.g. the exponential function for logistic
#'                  regression.
#' @param num.scale  A list to specify the scale for numerical variables. The
#'                   list may contain one element for each numerical variable,
#'                   named as the variable itself. These elements are themselves
#'                   lists with the values "type" and (optionally) "digits" and
#'                   "unit". Allowed values for the "type" field:
#'                   \itemize{
#'                   \item "sd": By standard deviation.
#'                   \item "iqr": By IQR.
#'                   \item A numerical value, such as 10.
#'                   }
#'                   If the "unit" field is provided, then this unit will be
#'                   reported in the table. A typical result in the "Level"
#'                   column could be "By IQR: 1.4 mmol/L". (In this example,
#'                   \code{digits} = 1).
#' @param include.ref  A logical specifying whether reference levels should be
#'                    included in the table.
#' @param or.hr  A string providing a name for the estimete, e.g. "OR" for odds
#'               ratios.
#' @param include.p  Logical specifying whether P-values are to be reported.
#' @param n  Number of individuals. Will be reported if non-\code{NULL}.
#' @param max.ucl  Confidence intervals are omitted (replaced by a bar) if the
#'                 upper limit exeeds this value.
#'
#' @return The return value is an S3 object of class \code{ucr.model.tab}.
#' @export

ucr.model.tab <- function(model, x.names=NULL, data, model.name,
  rm.intercept="(Intercept)",
  allow.missing.coef=F, allow.duplicated.coef=F, factor.sep="", link.fcn=exp,
  num.scale=NULL, include.ref=T, or.hr="OR", include.p=T, n=NULL, max.ucl=100) {

  class.contains <- function(x, obj) {
    ret <- any(is.element(x, class(obj)))
    return (ret)
  }
  known.model.class <- function(mod) {
    known.classes <- c("lm", "glm", "stripped.glm", "lrm", "cph", "mipo",
      "glmerMod", "matrix")
    ret <- class.contains(known.classes, mod)
    return (ret)
  }
  # Compute the content of a result cell.
  get.res.txt <- function(b, lcl, ucl, p) {
    if (link.fcn(ucl) < max.ucl) {
      res <- sprintf("%.2f (%.2f, %.2f)", link.fcn(b),
        link.fcn(lcl), link.fcn(ucl))
    } else {
      res <- sprintf("%.2f (---)", link.fcn(b))
    }
    if (include.p) {
      res <- sprintf("%s %s", res, ucr.format.p(p))
    }
    return (res)
  }
  # Make sure 'num.scale' is at least a list.
  if (is.null(num.scale)) {
    num.scale <- list()
  }
  # --> Make sure model(s) are stored in a list (with possibly only one item).
  if (known.model.class(model)) {
    model.list <- list(model) # Single-item list.
  } else if (is.list(model) && all(unlist(lapply(model, known.model.class)))) {
    model.list <- model
  } else {
    stop ("'model' must be a known model or a list of known models")
  }

  # Default: Expect all variables in the data set to be in the model.
  if (is.null(x.names)) {
    x.names <- names(data)
  }

  # --> Create matrix with results from model fit (estimate, SE, CI, P-value).
  result.mat <- matrix(0, nrow=0, ncol=4,
    dimnames=list(NULL, c("est", "lcl", "ucl", "p")))
  for (mod in model.list) {
    if (class.contains(c("lm"), mod)) {
      # Linear regression.
      b <- mod$coefficients
      ci <- confint(mod)
      lcl <- ci[, 1]
      ucl <- ci[, 2]
      sm <- summary(mod)
      p <- sm$coeff[, "Pr(>|t|)"]
      cur.rows <- cbind(b, lcl, ucl, p)
    } else if (class.contains(c("glm"), mod)) {
      # Logistic regression etc.
      b <- mod$coefficients
      ci <- confint(mod)
      lcl <- ci[, 1]
      ucl <- ci[, 2]
      sm <- summary(mod)
      p <- sm$coeff[, "Pr(>|z|)"]
      cur.rows <- cbind(b, lcl, ucl, p)
    } else if (class.contains("stripped.glm", mod)) {
      # Our stripped glm.
      b <- mod$coef
      se <- sqrt(diag(mod$vcov))
      lcl <- b - qnorm(0.975) * se # qnorm(0.975) = 1.96.
      ucl <- b + qnorm(0.975) * se
      p <- 2 * pnorm(abs(b/se), lower.tail=F)
      cur.rows <- cbind(b, lcl, ucl, p)
    } else if (class.contains(c("lrm", "cph"), mod)) {
      # Harrell's logistic regression etc.
      b <- mod$coefficients
      ci <- confint(mod)
      lcl <- ci[, 1]
      ucl <- ci[, 2]
      p <- 2 * pnorm(abs(b/se), lower.tail=F)
      cur.rows <- cbind(b, lcl, ucl, p)
    } else if (class.contains("mipo", mod) || (class(mod) == "matrix")) {
      # Pooled imputation analysis, plain (mipo) or summary'zed (matrix).
      if (class.contains("mipo", mod)) {
        coef.mat <- summary(mod) # Matrix with all necessary data.
      } else {
        coef.mat <- mod
      }
      cur.rows <- coef.mat[, c("est", "lo 95", "hi 95", "Pr(>|t|)")]
      if (nrow(coef.mat) == 1) { # Special handling of single-row matrices.
        cur.rows <- matrix(cur.rows, nrow=1)
        rownames(cur.rows) <- rownames(coef.mat)
      }
    } else if (class.contains("glmerMod", mod)) {
      sm <- summary(mod)$coefficients
      b <- sm[, "Estimate"]
      se <- sm[, "Std. Error"]
      lcl <- b - qnorm(0.975) * se
      ucl <- b + qnorm(0.975) * se
      p <- sm[, "Pr(>|z|)"]
      cur.rows <- cbind(b, lcl, ucl, p)
    } else {
      # TODO: Handle more models: ols, glm, coxph, lm.
      stop(sprintf("Unknown model class %s", class(mod)))
    }
    # Append model results. Coefficient names appear as row names.
    result.mat <- rbind(result.mat, cur.rows)
  }
  # Remove intercept?
  if (!is.null(rm.intercept)) {
    ix.rm <- which(rownames(result.mat) == rm.intercept)
    if (length(ix.rm) > 0) {
      result.mat <- result.mat[-ix.rm, ]
    }
  }
  coef.names <- rownames(result.mat)

  # Any duplicated coefficients? TODO: Handle intercept.
  if (!allow.duplicated.coef) {
    ix.dupl <- which(duplicated(coef.names))
    if (length(ix.dupl) > 0) {
      stop(sprintf("Duplicated coefficients, e.g. %s", coef.names[ix.dupl[1]]))
    }
  }

  # Define columns of resulting table.
  colno.var <- 1
  colno.ref <- 2
  colno.lev <- 3
  colno.res <- 4
  n.cols <- colno.res
  tab.mat <- matrix("", nrow=0, ncol=4)
  extra.col.heads <- rep("", times=n.cols) # Extra column heads.
  if (include.p) {
    extra.col.heads[colno.res] <- sprintf("%s (95%% CI) $P$", or.hr)
  } else {
    extra.col.heads[colno.res] <- sprintf("%s (95%% CI)", or.hr)
  }

  # Create columns headings and subheadings.
  dimnames(tab.mat)[[2]] <- rep("", times=ncol(tab.mat))
  dimnames(tab.mat)[[2]][colno.var] <- "Variable"
  dimnames(tab.mat)[[2]][colno.ref] <- "Reference"
  dimnames(tab.mat)[[2]][colno.lev] <- "Level"
  dimnames(tab.mat)[[2]][colno.res] <- model.name
  if (!is.null(n)) {
    dimnames(tab.mat)[[2]][colno.res] <- sprintf("%s ($N = %d$)",
      dimnames(tab.mat)[[2]][colno.res], n)
  }
  for (cur.x.name in x.names) {
    cur.x <- data[[cur.x.name]] # Current x variable.
    if (is.null(cur.x)) {
      # TODO: Allow for this? Empty cells?
      stop(sprintf("Variable %s not in data", cur.x.name))
    }
    if (Hmisc::label(cur.x) != "") {
      cur.x.label <- Hmisc::label(cur.x) # Variable Hmisc::label.
    } else {
      cur.x.label <- cur.x.name # No label, use variable name instead.
    }
    if (is.numeric(cur.x)) {
      # -----> Numeric variable.
      cur.row <- rep("", ncol=n.cols) # New row to be added to table.
      cur.row[colno.var] <- cur.x.label
      cur.row[colno.ref] <- ""
      scale.item <- num.scale[[cur.x.name]]
      if (is.null(scale.item)) {
        cur.scale <- 1
        cur.row[colno.lev] <- "By 1 unit"
      } else {
        cur.type <- scale.item$type
        cur.unit <- scale.item$unit
        cur.digits <- scale.item$digits
        if (is.null(cur.type)) {
          stop("Numerical type must be provided") # Error.
        }
        if (is.null(cur.unit)) {
          cur.unit <- ""
        } else {
          cur.unit <- sprintf(" %s", cur.unit) # Leading blank.
        }
        if (is.null(cur.digits)) {
          cur.digits <- 1
        }
        if (cur.type == "sd") {
          cur.scale <- sd(cur.x)
          cur.row[colno.lev] <- sprintf("By SD: %.*f%s", cur.digits, cur.scale, cur.unit)
        } else if (cur.type == "iqr") {
          x.quant <- quantile(cur.x, na.rm=T)
          cur.scale <- x.quant[4] - x.quant[2]
          cur.row[colno.lev] <- sprintf("By IQR: %.*f%s", cur.digits, cur.scale, cur.unit)
        } else if (is.numeric(cur.type)) {
          cur.scale <- cur.type
          cur.row[colno.lev] <- sprintf("By %.*f%s", cur.digits, cur.scale, cur.unit)
        } else {
          stop(sprintf("Illegal numerical type for %s", cur.x.name)) # Error.
        }
      }
      b <- result.mat[cur.x.name, "est"] * cur.scale
      lcl <- result.mat[cur.x.name, "lcl"] * cur.scale
      ucl <- result.mat[cur.x.name, "ucl"] * cur.scale
      p <- result.mat[cur.x.name, "p"]
      cur.row[colno.res] <- get.res.txt(b, lcl, ucl, p)
      tab.mat <- rbind(tab.mat, cur.row) # Append new row to table.
    } else { # End of numerical variable block.
      if (!is.factor(cur.x)) stop("Illegal variable type") # TODO.
      # -----> Factor variable.
      cur.levels <- levels(cur.x)
      cur.n.levels <- length(cur.levels)
      cur.rows <- matrix("", nrow=cur.n.levels-1, ncol=n.cols) # New rows to be added to table.
      cur.ref <- cur.levels[1] # The reference level.
      cur.rows[1, colno.var] <- cur.x.label # Variable label/name.
      cur.rows[1, colno.ref] <- cur.ref # Reference level.
      for (j in 1:(cur.n.levels-1)) { # Loop over all levels except the reference.
        cur.lev <- cur.levels[j+1] # Current level.
        cur.rows[j, colno.lev] <- cur.lev
        cur.coef.name <- sprintf("%s%s%s", cur.x.name, factor.sep, cur.lev)
        ix <- which(coef.names == cur.coef.name)
        if (length(ix) == 0) { # Missing coefficient?
          if (allow.missing.coef) {
            # OK with missing coefficients. Just leave the table entry empty.
            cur.rows[j, colno.res] <- ""
          } else {
            # Missing coefficients not allowed. Stop with error.
            stop(sprintf("Coefficient %s not found", cur.coef.name))
          }
          next # Nothing more to do for this variable.
        }
        b <- result.mat[ix, "est"]
        lcl <- result.mat[ix, "lcl"]
        ucl <- result.mat[ix, "ucl"]
        p <- result.mat[ix, "p"]
        if (link.fcn(ucl) < max.ucl) {
          cur.rows[j, colno.res] <- sprintf("%.2f (%.2f, %.2f)",
            link.fcn(b), link.fcn(lcl), link.fcn(ucl))
        } else {
          cur.rows[j, colno.res] <- sprintf("%.2f (---)",
            link.fcn(b))
        }
        if (include.p) {
          cur.rows[j, colno.res] <- sprintf("%s %s", cur.rows[j, colno.res],
            ucr.format.p(p))
        }
      }
      tab.mat <- rbind(tab.mat, cur.rows) # Append new rows to table.
    } # End of factor variable block.
  } # End of looooong loop over variables.
  rownames(tab.mat) <- NULL # For numerical variables.

  # Remove reference column?
  ncol.basic <- 3 # Number of "basic" columns (before the actual results).
  if (!include.ref) {
    tab.mat <- tab.mat[, -colno.ref]
    extra.col.heads <- extra.col.heads[-colno.ref]
    ncol.basic <- 2
  }

  # Create return object.
  result <- list()
  result$tab <- tab.mat # The table as a matrix.
  result$extra.col.heads <- extra.col.heads
  result$n.models <- 1 # Only one model so far. Can increase due to cbind.
  result$ncol.basic <- ncol.basic # Must be identical at cbind.
  class(result) <- "ucr.model.tab" # S3 class.
  return (result)
}
