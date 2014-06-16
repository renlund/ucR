#' @title Turn objects into data frames 
#' @param object an object
#' @param env an environment
#' @param ... passed arguments
#' @author Henrik Renlund
#' @export

makeDF <- function(object, env, ...) UseMethod("makeDF")

#' @title Turn objects into data frames 
#' @param object an object
#' @param env an environment
#' @param ... passed arguments
#' @author Henrik Renlund
#' @export

makeDF.default <- function(object, env, ...){
   message("The default method of makeDF doesn't do much")
   as.data.frame(NULL)
}

#' @title Turn variables into a data frame
#' @param object a character vector of variables contained in \code{env}
#' @param env the environment, data frame or list that contains the variables
#' @param ... for future needs?
#' @author Henrik Renlund
#' @export

makeDF.character <- function(object, env=.GlobalEnv, ...){
#    if( class(env) %in% c("data.frame", "list")){
#       env <- get(as.character(substitute(env)), inherits=TRUE)
#    } 
   for(K in object){
      if(!exists(K, env)){
         stop(paste0("'",K,"' does not exist in the specified place (environment, data frame, or list)."))
      } else {
         klass <- class(get(K, env))
         if(!exists("klasser", inherits=FALSE)) klasser <- klass
         klasser <- intersect(klass, klasser)
      }
   }
   curr_extensions <- setdiff(gsub("makeDF.", "", methods("makeDF")), c("default", "character"))
   if(length(klasser)>0){
      if(klasser %in% curr_extensions){
         for(k in seq_along(object)){
            X <- makeDF(get(object[k],env))
            if(is.null(d <- nrow(X))) d <- 1
            X <- cbind(X, "object" =rep(k, d))
            if(!exists("rX",inherits=FALSE)) rX <- X else rX <- rbind(rX, X)
         }
         return(rX)
      }
   }
   code <- paste0("data.frame(", paste0("'", object, "' = get('",object,"', env)", collapse=", "), ")")
   eval(parse(text=code))
}

# makeDF(1)
# x <- 1:5; y <- LETTERS[1:5]; z <- factor(rep(letters[2:1], len=5))
# (df <- makeDF(object=c("x", "y", "z")))
# makeDF(c("x", "z"), df)

#' @title Turn survfit object into a data frame
#' @param object a survfit object
#' @param env environment must currently be set to \code{.GlobalEnv}
#' @param ... for future needs?
#' @author Henrik Renlund
#' @export
 
makeDF.survfit <- function(object, env=.GlobalEnv, ...){
   if(environmentName(env)!="R_GlobalEnv"){
      stop("[makeDF.survfit] environments (other than the global) are currently unsupported")
   }
   use <- c("time", "n.risk", "surv", "upper", "lower")
   if(!is.null(strata <- object[['strata']])){
      namn <- names(strata)
      object[['strata']] <- factor(rep(namn, strata), levels=namn)
      use <- c(use, "strata")
   }
   makeDF(object=use, env=object)
}

# library(survival)
# S <- Surv(time=rexp(50,1), event=rbinom(50,1,0.2))
# sf <- survfit(S~1)
# sf2 <- sf
# 
# makeDF(object=  sf)
# makeDF(object = "sf")
# makeDF(object = c("sf", "sf2"))

#' @title Turn elements of a list into a data frame
#' @param object a list of objects for which a makeDF method exist
#' @param env environment must currently be set to \code{.GlobalEnv}
#' @param ... for future needs?
#' @author Henrik Renlund
#' @export
 
makeDF.list <- function(object, env=.GlobalEnv, ...){
   if(environmentName(env)!="R_GlobalEnv"){
      stop("[makeDF.list] environments (other than the global) are currently unsupported")
   }
   use <- names(object)
   makeDF(object=use, env=object)
}

# library(survival)
# S <- Surv(time=rexp(50,1), event=rbinom(50,1,0.2))
# sf <- survfit(S~1)
# sf2 <- sf
# L <- list(a=sf, b=sf2)
# makeDF(object=L)
# makeDF.list(object=L)

