#' @title This function manipulates factor levels
#'
#' @description This functions enables relabeling, reordering and merging of factor levels.
#' It is also possible to add and exclude levels, and turn missing values into
#' a separate level.
#'
#' @author Henrik Renlund
#' @param x factor or character vector to be relabelled or reordered
#' @param L a list specifying the relabeling/reordering
#' @param na.level if not \code{NULL} all \code{NA}'s will be a level with this label
#' @param exclude a value to make into NA's
#' @param new.last if \code{TRUE} then added levels will be placed last in the list of levels (default \code{FALSE})
#' @param warn do you want warnings when doing stupid things?
#' @examples
#' x <- LETTERS[1:5] # x <- factor(x)
#' L <- list(B=c("nyB", "C"),E=NULL, D="nyD")
#' reFactor(x,L)
#' @seealso \code{\link{factor}}, \code{\link{relevel}}
#' @note This function will be \code{refactor} in the 'dataman' package.
#' (It will be eventually be removed so as to not clutter the 'ucR' package too
#' much, but I don't want to break to much code in current use!)
#' @export

reFactor <- function(x,L, na.level=NULL, exclude=NULL, new.last=FALSE, warn=TRUE){
   if( !(is.numeric(x) | is.character(x) | is.factor(x)) ){
      stop("[reFactor] 'x' is neither factor, character nor numeric")
   }
   char_x <- as.character(x)
   fact_x <- if(is.factor(x)) x else factor(char_x)
   lev <- levels(fact_x)
   if(!is.null(na.level)){
      if(!is.character(na.level)) na.level <- "missing"
      char_x[is.na(char_x)] <- na.level
      lev <- unique(c(lev, na.level))
      fact_x <- factor(char_x, levels=lev)
   }
   if(is.character(exclude)){
      char_x[char_x %in% exclude] <- NA
      lev <- setdiff(lev, exclude)
      fact_x <- factor(char_x, levels=lev)
   }
   if(missing(L)) return(fact_x)
   if(!is.list(L))
      stop("[reFactor] 'L' needs to be a list.")
   nL <- names(L)
   levels_cleared <- c()
   new_levels <- c()
   new_names <- c()
   last <- c()
   for(key in nL){
      if(key %in% levels_cleared){
         if(warn) warning("[reFactor] Key '",key,"' has been used previously and will be skipped.")
         next
      }
      if(key == ""){
         empty_indx <- min(which(nL == key))
         a <- L[[empty_indx]]
      } else {
         a <- L[[key]] # before
      }
      if( !(key %in% lev) ){
         if( !is.null(a) ) if(warn) warning(paste0("[reFactor] Key ", key,
                                          " is a new value and will not be renamed nor will it absorb any other value"))
         if(new.last){
            last <- c(last, key)
         } else {
            new_levels <- c(new_levels, key)
         }
         levels_cleared <- c(levels_cleared, key)
         next
      }
      filt <- a %in% lev
      vs <- a[filt]
      if(any(vs %in% levels_cleared)){
         if(warn) warning(paste0("[reFactor] Key '",key,
                        "' has entries used previously. These will be skipped "))
         vs <- setdiff(a, levels_cleared)
         if(length(vs)==0) next
      }

      if(sum(!filt)>1){
         if(warn) warning(paste0("[reFactor] Multiple new names for level '",
            key,"'. Only the first one will be used."))
      }
      s <- a[!filt][1]
      if( is.null(s) || is.na(s) ) {
         s <- key
      } else {
         if(s %in% lev) stop(paste0("[reFactor] Key '",key,
                                    "' has new name'",s,
            "' which exists as a level of 'x'.")) # THIS CAN NEVER HAPPEN!
         if(s %in% new_names) stop(paste0("[reFactor] Key '",key,"'
            has new name'",s,
            "' which is occupied."))
      }
      vs <- c(vs, key)
      char_x[char_x %in% vs] <- s
      new_levels <- c(new_levels, s)
      levels_cleared <- c(levels_cleared, vs)
      new_names <- c(new_names, s)
   }
   ALL <- if(new.last) c(new_levels, last) else new_levels
   new_levels <- c(new_levels, setdiff(unique(char_x),ALL), last)
   factor(char_x, levels=new_levels)
}
