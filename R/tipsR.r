#' @title View tipsR presentations
#' @description If you have access to 'P:/Programming/tipsR' this function
#' can let you open the tipsR presentations
#' @author Henrik Renlund
#' @param types What kind of files to look for in 'P:/Programming/tipsR'. Defaults
#' to hmtl and pdf.
#' @export

tipsR <- function(types=c(".html", ".pdf")){
   wd <- getwd()
   path <- file.path("P:", "Programming", "tipsR")
   tryCatch(
      setwd(path),
      error = function(e) stop(paste("[tipsR]",path,"is not available."))
   )
   setwd(wd)
   look <- paste(paste0("(",types,")"), collapse="|")
   L <- list.files(path=path, pattern=look, recursive=TRUE)
   L_full <- list.files(path=path, pattern=look, recursive=TRUE, full.names=TRUE)
   showL <- paste(paste0(1:length(L),": ",L), collapse="\n")
   cat(paste0("PRESENTATIONS:\n", showL, "\n"))
   x <- readline("\nChoose index (anything else to quit)  ")
   if(x %in% as.character(1:length(L))){
      shell.exec(L_full[as.numeric(x)])
   }
   cat("")
   invisible(NULL)
}