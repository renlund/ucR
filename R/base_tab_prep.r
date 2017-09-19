#' @title Prepare a data frame for base tabulating
#' @description Remove difficult variables: dates, factors with too many unique
#'   values, etc.
#' @param X a data frame
#' @param max.unique maximum number of unique values to tolerate (for
#'   non-numeric variables), default 10
#' @param elim.names set of names to eliminate
#' @param force.factor \code{TRUE} (default) if caracter vectors should be
#'   factors
#' @param binary.code convert a 0/1 variable into a factor with these labels (if
#'   \code{TRUE} then 0 = "No" and 1 = "Yes").
#' @param elim.class a set of classes to eliminate in addition to \code{Date},
#'   \code{POSIXct} and \code{POSIXt}
#' @author Henrik Renlund
#' @export


base_tab_prep <- function(X, max.unique = 10, elim.names = NULL, force.factor = TRUE, binary.code = TRUE, elim.class = NULL){
   if(!"data.frame" %in% class(X)) warning("[base_tab_prep] X does not have the property 'data.frame'")
   elim_class <- c(elim.class,  c("Date", "POSIXct", "POSIXt"))
   if(is.logical(binary.code)){
      if(binary.code) {
         b_fix <- TRUE
         blist <- list("0" = "No", "1" = "Yes")
      } else {
         b_fix <- FALSE
      }
   } else if(is.character(binary.code)){
       if(length(binary.code) != 2){
           stop("[base_tab_prep] wrong length of binary code")
       }
      b_fix <- TRUE
      blist <- list("0" = binary.code[1], "1" = binary.code[2])
   } else {
      stop("[base_tab_prep] weird binary code")
   }
   for(K in names(X)){ ## K = "bl_hyper"
      if(K %in% elim.names){
         X[[K]] <- NULL
         next
      }
      the_class <- class(X[[K]])
      if(any(the_class %in% elim_class)){
         X[[K]] <- NULL
         next
      }
      n_unique <- length(stats::na.omit(unique(X[[K]])))
      if(all(!the_class %in% c("numeric", "integer")) & n_unique > max.unique) {
         X[[K]] <- NULL
         next
      }
      if(n_unique == 2){
         unique_set <- stats::na.omit(unique(X[[K]]))
         if(setequal(unique_set, 0:1)){
            X[[K]] <- reFactor(x = X[[K]], L = blist)
            next
         }
      }
      if(force.factor & is.character(X[[K]])){
         X[[K]]  <- factor(X[[K]])
      }
   }
   X
}
