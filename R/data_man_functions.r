#' @title Data Management Tool (interactive-ish)
#' @description ...
#' @param name (new) name of variable
#' @param var database entry
#' @param where database (or data frame)
#' @param recode ucR::reFactor L argument for recoding
#' @param date function for transformation into date format
#' @param rg rgroup label
#' @param comment variable comment
#' @param container name of container of information in \code{.GlobalEnv}.
#'   Defaults to "data_man_container"
#' @param label if TRUE then the data base entry will be stored as a label for
#'   the new variable. Another label can be specified with a character string.
#' @param check if TRUE this checks the variable
#' @author Henrik Renlund
#' @export

data_man <- function(name,
                   var = NA,
                   where = NA,
                   recode = NULL,
                   date = NULL,
                   rg = NULL,
                   comment = NULL,
                   container = "data_man_container",
                   label = FALSE,
                   check = TRUE
){
   if(is.na(where)){
      if(check) message("[data_man] cannot 'check' if arg 'where' is missing")
      check <- FALSE
   }
   if(check){
       if(!exists(var, get(where))) warning("[data_man] data base entry appears to be non-existing")
       tmp_var <- get(where)[[var]]
       class_var <- class(tmp_var)
       cat(paste(rep("-", options("width")[[1]]-3), collapse=""), "\n")
       cat("Adding data base '", where,"' entry '", var, "' as variable '", name,"'\n", sep = "")
       cat("A variable of class: ", class_var, "\n")
       #if(!any(class_var %in% c("Date", "POSIXct", "POSIXt"))){
       if(class_var %in% c("numeric", "integer")){
         cat("\nSummary of numeric variable:")
         cat("\n    min:", min(tmp_var, na.rm=T), "\n    max:", max(tmp_var, na.rm=TRUE), "\n    mean:",mean(tmp_var, na.rm=TRUE), "\n")
      }
      if(class_var %in% c("integer", "numeric", "factor", "character")){
         n_unique <- length(unique(tmp_var))
         if(n_unique<=10){
            cat("\nThere are only", n_unique,"unique values. \nTabulated: \n\n")
            print(table(tmp_var, useNA="always", dnn = NULL))
         } else {
            x <- tmp_var[!is.na(tmp_var)]
            if(!is.numeric(tmp_var)){
               x <- x[!grepl("^ *$", x)]
            }
            x <- x[1:min(length(x), 20)]
            if(length(x) & !class_var %in% c("numeric", "integer") ){
               cat("\nThe first (at most 20) non-NA or non-empty values are:\n   ", paste0(x, collapse =", "), "\n")
            } else if(!length(x)){
               cat("There are only NA or empty values!\n")
            }
         }
      }
      if(!is.null(recode)){
         cat("\nCross-tabulating the recoding: \n\n")
         print(
            recode_table <- table(
               tmp_var,
               reFactor(x = tmp_var, L = recode),
               dnn = c(var, name),
               useNA="ifany"
            )
         )
      }
   }

   if(!is.character(container)) stop("[ucR::data_man] wrong 'container' argument")
   if(
      length(container)>1 |
         nchar(container)==0 |
         grepl(" ", container) |
         grepl("^[0-9]", container)
      ) stop("[ucR::data_man] wrong 'container' argument")
   L <- list(
      name = name,
      var = var,
      where = where,
      recode = recode,
      date = date,
      rg = rg,
      comment = comment,
      label = if(is.character(label)) label else if(label) var else NULL,
      recode_table = if(exists("recode_table", envir = environment(), inherits = FALSE)) recode_table else NULL
   )

   if(!exists(container, envir=.GlobalEnv)) {
      foo <- as.list(NULL)
      class(foo) <- "data_man"
   } else {
      foo <- get(container, envir = .GlobalEnv)
      if(
         !is.list(foo) | !"data_man" %in% class(foo) # |
            # length(names(foo)) != length(foo) |
            # any(unlist(lapply(foo, function(x) names(x)[1] != "name")))
      ){
         cat(paste0("A variable '", container, "' already exists and does not appear to be correct. Would you like to overwrite it?\n"))
         if(readline("Press 'x' to abort, anything else to proceed\n  ") == "x"){
            return(invisible(NULL))
         }
         foo <- as.list(NULL)
         class(foo) <- "data_man"
      }
   }
   foo[[name]] <- L
   assign(container, foo, envir = .GlobalEnv)
}

#' @title Get data from \code{data_man_container} info
#' @description ...
#' @param id vector of id's
#' @param container name of \code{data_man_container} object
#' @param id.name identifier across data bases or a list of such
#' @param na.where if entry \code{where} is \code{NA} in
#' \code{data_man_container}, this object is used in its place.
#' @export

data_man_create <- function(id,
                              container,
                              id.name,
                              na.where = NULL
){
   if(missing(id.name)) id.name <- "id"
   if(missing(container)){
      tryCatch(expr = container <- get("data_man_container", envir = .GlobalEnv),
               error = function(e) stop("[data_man_create] no default container available"))
   }
   identicalid <- length(id.name) == 1
   the_id <- id.name[[1]]
   if(!is.null(na.where)){
      tryCatch(expr = NAW <- get(na.where, envir = .GlobalEnv),
               error = function(e) stop("[data_man_create] can't find 'na.where'"))
      if(!identicalid) id.name[["NAW"]] <- id.name[[na.where]]
   }
   DF <- data.frame(id)
   names(DF) <- the_id

   for(indx in seq_along(container)){ # indx <- 17
      cat(paste0("Fixing variable no.", indx, ": ", names(container)[indx], "\n"))
      X <- container[[indx]]
      var <- X$var
      name <- X$name
      if( is.na(X$where) & is.null(na.where)){
         next
      } else {
         df <- if(!is.na(X$where)) X$where else "NAW"
         tmp <- get(df)[[var]]
      }
      if(!is.null(f <- X$date)){
            tmp <- get(f)(tmp)
      }
      if(!is.null(recode <- X$recode)){
         tryCatch(expr = {
            tmp <- reFactor(x=tmp, L=recode)
         },
         warning=function(w) {
            print(var)
            print(w)
            # break
         }
         )
      }
      loc.df <- data.frame(
         tmp,
         get(df)[[if(identicalid) the_id else id.name[df][[1]]]]
      )
      names(loc.df) <- c(name, the_id)
      DF <- merge(DF, loc.df, by.x = the_id)
      if(!is.null(X$label)) attr(DF[[name]], "label") <- X$label
   }
   DF
}

#' @title \code{makeDF} method for \code{data_man_container} object
#' @description ...
#' @param object a \code{data_man_container} object
#' @param env useless here (included for S3 method consistency.)
#' @param ... useless here (included for S3 method consistency.)
#' @param where if \code{TRUE} then 'where' info is included. If \code{NULL}
#'   (default) then the function will guess that you want it if there is such
#'   information.
#' @param comment if \code{TRUE}  then 'comment' info is included. If
#'   \code{NULL} (default) then the function will guess that you want.
#' @param rgroup if \code{TRUE}  then a \code{cr_group} object is returned with
#'   attribute \code{rgroup} from 'rgroup' info. If \code{NULL} (default) then
#'   the function will guess that you want
#' @param null.rgroup the \code{rgroup} attribute for \code{is.na(rgroup)}.
#' @export

makeDF.data_man <- function(object, env, ..., where = NULL, comment = NULL, rgroup = NULL, null.rgroup = "Other"){
   if(is.null(rgroup)) {
      rgroup <- if(any(unlist(lapply(object, FUN = function(x) !is.null(x$rg))))) TRUE else FALSE
   }
   if(is.null(comment)) {
      comment <- if(any(unlist(lapply(object, FUN = function(x) !is.null(x$comment))))) TRUE else FALSE
   }
   if(is.null(where)) {
      where <- if(any(unlist(lapply(object, FUN = function(x) !is.null(x$where))))) TRUE else FALSE
   }
   if(!missing(env)) {
      warning("[makeDF.data_man] argument 'env' does not do anything...\n (Included for S3 method consistency.)")
   }
   n_col = 2 + where + comment
   M <- matrix(NA_character_, nrow=length(object), ncol=n_col)
   colnames(M) <- c(
      "variable",
      "database entry",
      if(where) "from" else NULL,
      if(comment) "comment" else NULL
      )
   rgr <- NULL
   for(k in seq_along(object)){ # k  = 1
      M[k, 1] <- object[[k]]$name
      M[k, 2] <- if(is.na(object[[k]]$var)) "?" else object[[k]]$var
      if(where) M[k, 3] <- if(is.na(object[[k]]$where)) "?" else object[[k]]$where
      if(comment) {
         the_comment <- object[[k]]$comment
         M[k, 3 + where] <- if(is.null(the_comment)) "" else the_comment
      }
      if(rgroup){
         the_rg <-  object[[k]]$rg
         rgr <- c(rgr, if(is.null(the_rg)) null.rgroup else the_rg)
      }
   }
   M <- as.data.frame(M)
   if(rgroup) cr_group(M, rgroup = rgr) else M
}

#' @title Show \code{data_man} recodings
#' @description ...
#' @param container the 'data_man' container
#' @param file 'file' argument for \code{Hmisc::latex} (default = "")
#' @param lab.prefix prefix for labels for LaTeX tables (default = "tab:recode_") which concatenates with the variable name
#' @param ... arguments passed to \code{Hmisc::latex}.
#' @author Henrik Renlund
#' @export

data_man_get_recode <- function(container, file = "", lab.prefix = "tab:recode_", ...){
   if(missing(container)){
      tryCatch(expr = container <- get("data_man_container", envir = .GlobalEnv),
               error = function(e) stop("[data_man_get_recode] no default container available"))
   }
   ttify <- function(s) if(length(s)) gsub("_", "\\_", paste0("\\texttt{", s, "}"), fixed=TRUE ) else ""
   for(k in seq_along(container)){
      X <- container[[k]]

      if(is.null(recode <- X$recode)){
         next
      } else {
         if(is.null(L <- X$recode_table)){
            tmp_var <- get(X$where)[[X$var]]
            L <- table(
               tmp_var,
               reFactor(x = tmp_var, L = recode),
               dnn = c(X$var, X$name),
               useNA="ifany"
            )
         }
      }
      var <- ttify(X$where)
      Hmisc::latex(object = L,
                   file = file,
                   append = TRUE,
                   caption = paste0("Recoding of data base ",ttify(X$where), " entry ",ttify(X$var)," into ",ttify(X$name),"."),
                   label = paste0(lab.prefix, X$name),
                   ...)
   }
   invisible(NULL)
}