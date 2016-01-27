##' @title Slight formatting for exporting a base tab object
##' @description Replace LaTeX code with something more readable. This
##'     is a simple convenience function that gsub:s the latex code
##'     from the first row of the ucr base tab 'tab' component.
##' @param object A ucr.base.tab object or the 'tab' component of that list.
##' @param replacer Replace the LaTeX in first column with this
##' @examples
##' ## Apply this to the base tab object to make it more readable
##' ## bt <- ucr.base.tab(my_data)
##' ## write.csv(ucr_tab_exp(bt), file = "path/to/file.csv")
##' @author Henrik Renlund
##' @export

base_tab_exp <- function(object, replacer = " -:- "){
    x <- if("ucr.base.tab" %in% class(object)) object$tab else object
    x[, 1] <- gsub("\\hspace{1em}", replacer, x[, 1], fixed = TRUE)
    x
}
