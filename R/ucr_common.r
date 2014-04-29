#' Format p values for readability
#' 
#' Utility function that formats a P value array to a string array.
#' Smaller values than 'min.p' (a string!) will be replaced by strings like
#' "< 0.001".
#' @author Lars Lindhagen
#' @param p the p value to format
#' @param min.p string indicating minimum value to display explicitly
#' @examples 
#' ucr.format.p(0.0000456, min.p="0.01")
#' @seealso \code{\link{format}}, \code{\link{sprintf}}
#' @export

ucr.format.p <- function(p, min.p="0.001") {
  ret <- ifelse(p < as.numeric(min.p), sprintf("$<%s$", min.p),
    # Above 0.1: Decimal notation, two decimals.
    ifelse(p >= 0.1, sprintf("%.2f", p),
    # Above 0.001: Decimal notation, three decimals.
    ifelse(p >= 0.001, sprintf("%.3f", p),
    # Small. Scientific notation, two significant digits.
    sprintf("%.1e", p))))
  return (ret)
}
