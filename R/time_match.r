##' find matches on variables in a given time period
##'
##' in a dataset with one or more variables (typically containing text or codes)
##'     associated with a date, find matches on those variables within specifed
##'     time frames
##' @title find matches
##' @param data a data frame
##' @param s a search string (regular expression)
##' @param search names of variables to search in (given in order of importance)
##' @param id name of id variable
##' @param date name of associated date variable
##' @param begin date (or vector of dates) or the name of a date variable
##' @param end date (or vector of dates) or the name of a date variable
##' @param matches specification of what to return; 'all' for all matches,
##'     'first.date' to filter on the first occurence for any individual and
##'     date, and 'first.id' to filter on the first occurence of any individual
##' @param non.matches if \code{TRUE} also return data on the individuals with
##'     no matches
##' @param keep vector of names to also keep from data. Note what the names in
##'     the reuturn objects are, if keep conflicts with these, some variables
##'     will be renamed. Also, the id, date and possibly begin and end variables
##'     will always be kept (but renamed)
##' @param verbose if \code{TRUE} the function will give helpful and/or annoying
##'     messages sometimes
##' @return a data frame with \itemize{
##'  \item id id variable
##'  \item event 1 of match found (else 0 if any and \code{non.matches = TRUE})
##'  \item t days from 'begin' to 'date'
##'  \item match the match found
##'  \item variable match found in this variable
##'  \item t2 days from 'date' to end
##'  \item first.id indicator for first occurence of associated id
##'  \item first.date indicator for first occurence of associated id and date
##'  \item last.id indicator for last occurence of associated id
##'  \item last.date indicator for last occurence of associated id and date
##'  \item begin the begin variable
##'  \item date the date variable
##'  \item end the end variable
##' }
##' @author Henrik Renlund
##' @export
time_match <- function(data, s, search, id = 'id', date = 'date',
                       begin = NULL, end = NULL,
                       matches = "all", non.matches = TRUE,
                       keep = NULL, verbose = FALSE){
    if(verbose) cat("\n[Function ucR::time_match set to verbose.]\n",
                    "Checking arguments and preparing data\n")
    ## -- some sanity checks --
    ## vars is the names of variables we keep or create by default
    vars <- .time_match_vars()
    ## make sure there's no name conflicts between variables keep by default and
    ## created, versus want the user wants to keep (as specified by 'keep')
    begin.chr <- if(!is.null(begin) & is.character(begin)) begin else NULL
    end.chr <- if(!is.null(end) & is.character(end)) end else NULL
    keep <- setdiff(keep, c(id, date, begin.chr, end.chr))
    if(!is.null(keep)){
        updates <- .update_names(keep = keep,
                                 data.names = names(data),
                                 vars = vars,
                                 verbose = verbose)
        keep <- updates$keep
        names(data) <- updates$data.names
        vars <- updates$vars
    }
    if(length(s) > 1){
        warning("s of length > 1, only first entry used")
        s <- s[1]
    }
    .time_match_check_match(matches)
    nm <- c(id, date, search, keep)
    badname.indx <- which(!nm %in% names(data))
    .time_match_check_data_names(names(data), c(nm, begin.chr, end.chr))
    ## -- get relevant data and fix 'begin' and 'end' if not provided
    if(!is.null(begin.chr)){
        begin <- data[[begin.chr]]
    } else if(!is.null(begin)){
        if(any(is.na(begin))) stop("no missing in 'begin' please")
        if(verbose) if(min(begin) < max(data[[date]])){
            message(" (!) FYI: (earliest) 'begin' earlier than earliest 'date'")
        }
    }
    if(!is.null(end.chr)){
        end <- data[[end.chr]]
    } else if(!is.null(end)){
        if(any(is.na(end))) stop("no missing in 'end' please")
        if(verbose) if(max(end) > max(data[[date]])){
            message(" (!) FYI: (latest) 'end' larger than latest 'date'")
        }
    }
    data <- data[, nm]
    names(data) <- c("id", "date", search, keep)
    if(is.null(end)) end <- max(data$date, na.rm = TRUE)
    if(is.null(begin)) begin <- min(data$date, na.rm = TRUE)
    data$begin <- begin
    data$end <- end
    ## -- hard to deal with missing in 'date', so if so throw
    ## --    error and let user fix this
    na.indx <- which(is.null(data$date))
    if(length(na.indx) > 0){
        warning("Missing 'date' at rows:", paste0(na.indx, collapse = ", "),
                "\nThese will be removed\n")
        data <- data[!na.indx, ]
    }
    ## -- calculate 't', time from beginning, and 't2', time to end
    data$t  <- as.numeric(difftime(data$date, data$begin, units = "days"))
    data$t2 <- as.numeric(difftime(data$end, data$date, units = "days"))
    ## -- filter down to relevant time period but keep copy of data for later
    data.copy <- subset(data, TRUE, select = c("id", "begin", "end"))
    data <- data[data$date >= data$begin & data$date <= end, ]
    ## -- look for patters 's' in each search variable
    if(verbose) cat(" Searching through variables:\n")
    R <- NULL
    for(K in search){ ##  K <- search[1] ## for testing
        if(verbose) cat("  - Searching variable:", K, "\n")
        g <- grepl(pattern = s, x = data[[K]])
        if(sum(g) == 0) next
        tmp <- data[g, ]
        tmp$variable <- factor(K, levels = search)
        tmp$match <- tmp[[K]]
        tmp$event <- 1L
        R <- if(is.null(R)) tmp else rbind(R, tmp)
    }
    if(verbose) cat(" Search complete\n Fixing output data\n")
    ## -- order matches and create indicators for first id and first date
    ## -- if there are no matches, object R is still NULL (treat separately)
    if(is.null(R)){
        S <- as.data.frame(matrix(vector(), nrow = 0, ncol = length(vars),
                           dimnames = list(c(), vars)))
    } else {
        S <- R[order(R$id, R$date, R$variable), ]
        n <- nrow(S)
        ## -- create indicators for first instance of each id and first instance
        ## -- of each id + date combination. This code must treat the case of
        ## -- length 1 vectors separately
        if(n > 1){
            S$first.id <- as.integer(c(TRUE, !(S$id[2:n] == S$id[1:(n-1)])))
            S$first.date <- as.integer(
                c(TRUE, !(S$id[2:n] == S$id[1:(n-1)] &
                          S$date[2:n] == S$date[1:(n-1)]))
                )
            S$last.id <- as.integer(c(!(S$id[1:(n-1)] == S$id[2:n]), TRUE))
            S$last.date <- as.integer(
                c(!(S$id[1:(n-1)] == S$id[2:n] &
                    S$date[1:(n-1)] == S$date[2:n]), TRUE)
                )
        } else {
            S$first.id <- 1L
            S$first.date <- 1L
            S$last.id <- 1L
            S$last.date <- 1L
        }
    }
    ## -- look at individuals without any matches
    TMP <- subset(data.copy, !id %in% S$id)
    ## -- create the object to return based on arguments given
    RET <- if(non.matches & nrow(TMP) > 0){
               n <- nrow(TMP)
               TMP <- TMP[order(TMP$id), ]
               ## -- keep only one line for each id
               if(n > 1){
                   B <- TMP[c(TRUE, !TMP$id[2:n] == TMP$id[1:(n-1)]), ]
               } else {
                   B <- TMP
               }
               B$event <- 0L
               B$t <- as.numeric(difftime(B$end, B$begin, units = "days"))
               B$match <- NA_character_
               B$variable <- NA_character_
               B$t2 <- 0
               B$first.id <- 1L
               B$first.date <- 1L
               B$last.id <- 1L
               B$last.date <- 1L
               B$date <- B$end
               for(K in keep){
                   B[[K]] <- NA
               }
               rbind(S[, vars], B[, vars])
           } else {
               S[, vars]
           }
    rownames(RET) <- NULL
    ## if(!is.null(s.name)){ ## is naming output after names(s) a good idea?
    ##     names(RET)[names(RET) == "event"] <- paste0("event.", s.name)
    ##     names(RET)[names(RET) == "t"] <- paste0("t.", s.name)
    ## }
    if(verbose) cat(" Done!\n\n")
    if(matches == "all"){
        RET
    } else if(matches == "first.date"){
        RET[RET$first.date == 1,]
    } else if(matches == "first.id"){
        RET[RET$first.id == 1,]
    } else if(matches == "last.date"){
        RET[RET$last.date == 1,]
    } else if(matches == "last.id"){
        RET[RET$last.id == 1,]
    } else {
        message(" One should never see this message, somethings very wrong!")
        invisible(NULL)
    }
}

## check that nm are elements of data.names
.time_match_check_data_names <- function(data.names, nm){
    badname.indx <- which(!nm %in% data.names)
    if(length(badname.indx) > 0){
        stop("Some variable names specified (i.e.: ",
             paste(nm[badname.indx], collapse = ", "),
             ") does not exist in data.")
    }
    invisible(NULL)
}


## default names of variables used/created by time_match
.time_match_vars <- function(){
    c("id", "event", "t", "match", "variable", "t2",
      "first.id", "first.date", "last.id", "last.date",
      "begin", "date", "end")
}

##
.time_match_check_match <- function(matches){
    ok.matches <- c("first.id", "first.date", "last.id", "last.date", "all")
    if(!matches %in% ok.matches){
        stop("'matches' must be one of: ", paste(ok.matches, collapse = ", "))
    }
    invisible(NULL)
}

##  helper for time_match
##
##  make sure there are no name conflicts
##  @param keep
##  @param data.names
##  @param vars
##  @author Henrik Renlund
.update_names <- function(keep, data.names, vars, verbose = TRUE){
    if(!is.null(keep)){
        ## Throw error if not all keep-names are in data
        notindata <- which(!keep %in% data.names)
        if(length(notindata) > 0){
            b <- paste0("\nSome variable names given by 'keep' are not",
                        " in data:\n   ",
                        paste0(data.names[notindata], collapse = ", "),
                        ".\n")
            stop(b)
        }
        ## Rename variable names that are overlapping and not kept anyway
        invars <- which(keep %in% vars)
        if(length(invars) > 0){
            keep.org <- keep
            keep[invars] <- paste0("data_", keep[invars])
            data.names[data.names == keep.org[invars]] <- keep[invars]
            b <- paste0(" (!) FYI: Reserved names for this calculation are:\n",
                        "        ",
                        paste0(vars, collapse = ", "), ".\n",
                        "     Some variables (named: ",
                        paste0(keep.org[invars], collapse = ","),
                        ") will be renamed (renamed: ",
                        paste0(keep[invars], collapse = ", ",")"))
            if(verbose) message(b)
        }
        vars <- c(vars, keep)
    }
    list("keep" = keep, "data.names" = data.names, "vars" = vars)
}

##' @describeIn time_match wrapper for find_match to look at only those units within a given set
##' @title find matches for specified units
##' @param set a vector of id's, or a data frame containing id's as well as (but
##'     optionally) 'begin' and 'end' variables
##' @param set.id variable name in 'set' to use as id
##' @param set.begin variable name in 'set' to use as begin, will be set to smallest date in data
##' @param set.end variable name in 'set' to use as end, will be set to smallest date in data
##' @export
time_match_set <- function(data, s, search, id = "id", date = "date",
                           set, set.id = id,
                           set.begin = "begin", set.end = "end",
                           matches = "all", non.matches = TRUE,
                           keep = NULL, verbose = FALSE){
    if(verbose) cat("\n[Function ucR::time_match_set set to verbose.]\n",
                    "Checking arguments and preparing data before calling",
                    "time_match...\n")
    vars <- .time_match_vars()
    .time_match_check_match(matches)
    keep <- setdiff(keep, c(id, date))
    if(!is.null(keep)){
        updates <- .update_names(keep = keep,
                                 data.names = names(data),
                                 vars = vars,
                                 verbose = verbose)
        keep <- updates$keep
        names(data) <- updates$data.names
        vars <- updates$vars
    }
    nm <- c(id, date, search, keep)
    .time_match_check_data_names(names(data), nm)
    begin.chr <- if(!is.null(set.begin) & is.character(set.begin)){
                     if(set.begin %in% names(set)) set.begin else NULL
                 } else NULL
    end.chr <- if(!is.null(set.end) & is.character(set.end)){
                   if(set.end %in% names(set)) set.end else NULL
                   } else NULL
    if(is.null(dim(set))){
        set <- data.frame(id = set)
        if(verbose) if(!any(set$id %in% data[[id]])){
                        message(" (!) FYI: no element of set in data id")
                    }
    } else {
        nm_set <- intersect(c(set.id, begin.chr, end.chr), names(set))
        set <- set[, nm_set, drop = FALSE]
        if(!set.id %in% names(set)){
            stop("An id-variable ('", set.id, "') is needed in set.\n",
                 "Set this separately with 'set.id'")
        }
        names(set) <- c("id",
                        if(!is.null(begin.chr)) "begin",
                        if(!is.null(end.chr))"end")
    }
    if(is.null(set$begin)){
        set$begin <- if(inherits(set.begin, "Date")){
                         set.begin
                     } else {
                         min(data[[date]], na.rm = TRUE)
                     }
    }
    if(is.null(set$end)){
        set$end <- if(inherits(set.end, "Date")){
                       set.end
                       } else {
                           max(data[[date]], na.rm = TRUE)
                       }
    }
    ## set <- set[, c("id", "begin", "end")] ## not needed
    ## -- fix data
    data <- data[, nm]
    names(data) <- c("id", "date", search, keep)
    ## -- missing date in data will be problematic, throw error
    na.indx <- which(is.null(data$date))
    if(length(na.indx) > 0){
        warning("Missing 'date' at rows:", paste0(na.indx, collapse = ", "),
                "\nThese will be removed\n")
        data <- data[!na.indx, ]
    }
    DATA <- merge(set, data, by = "id", all.x = TRUE)
    DATA$date[is.na(DATA$date)] <- max(DATA$end, na.rm = TRUE)
    if(verbose) cat(" ...done!\n")
    time_match(data = DATA, s = s, search = search, id = "id",
               date = "date", begin = "begin", end = "end",
               matches = matches, non.matches = non.matches,
               keep = keep, verbose = verbose)
}

if(FALSE){ ## -- for testing ---

    df <- data.frame(
        foo = rep(1:5, c(4, 3, 1, 1, 1)),
        bar = as.Date("2001-01-01") + c(-371,1,1,2, 2,3,371, 0, 372, 4),
        baz =  c("b","a","a","b", "a","b","b", "a", "b", "b"),
        quuz = c("a","b","a","b", "b","a","a", "b", "a", "b"),
        xtra = sprintf("extra%d", 1:10),
        date = sprintf("date%d", 1:10)
    )
    df <- df[sample(1:nrow(df)), ]
    Set <- data.frame(
        ID = c(2:4, 6:7),
        arrival = as.Date("2000-06-06") + c(0,10,365,366,367),
        death = c(1,1,1,0,0),
        death.date = as.Date("2001-06-06") + c(0,100,200,720,720)
    )
    Set <- Set[sample(1:nrow(Set)), ]

    time_match_set(data = df, s = "a", search = "baz", id = "foo",
                   date = "bar", set = Set, set.id = "ID",
                   set.begin = "arrival", set.end = "death.date",
                   matches = "all", non.matches = TRUE, keep = "xtra",
                   verbose = TRUE)

    ## data = df
    ## s = "a"
    ## search = "baz"
    ## id = "foo"
    ## date = "bar"
    ## set = Set[, c("ID", "arrival")]
    ## set.id = "ID"
    ## set.begin = "arrival"
    ## ## set.end = "death.date"
    ## set.end = "end"
    ## matches = "all"
    ## non.matches = TRUE
    ## keep = "xtra"
    ## verbose = TRUE

    time_match_set(data = df, s = "a", search = "baz", id = "foo",
                   date = "bar", set = Set[, c("ID", "arrival")],
                   set.id = "ID", set.begin = "arrival",
                   matches = "all", non.matches = TRUE, keep = "xtra",
                   verbose = TRUE)

    time_match(data = df, s = "a", search = c("quuz", "baz"), id = "foo",
               date = "bar", begin = as.Date("2001-01-01"),
               end = as.Date("2001-12-31"), matches = "all", non.matches = TRUE,
               verbose = TRUE)

    time_match(data = df, s = "a", search = c("quuz", "baz"), id = "foo",
               date = "bar", begin = as.Date("2001-01-01"),
               end = as.Date("2001-12-31"), matches = "all", non.matches = TRUE,
               keep = c("xtra", "date" , "foo"),
               verbose = TRUE)

    time_match(data = df, s = "a", search = c("quuz", "baz"), id = "foo",
               date = "bar", begin = as.Date("2010-01-01"),
               end = as.Date("2011-12-31"), matches = "all", non.matches = TRUE,
               verbose = TRUE)

    time_match(data = df, s = "a", search = c("quuz", "baz"), id = "foo",
               date = "bar", begin = as.Date("2010-01-01"),
               end = as.Date("2011-12-31"), matches = "all", non.matches = TRUE,
               keep = c("xtra"), verbose = TRUE)

    time_match(data = df, s = "c", search = c("quuz", "baz"), id = "foo",
               date = "bar", begin = as.Date("2001-01-01"),
               end = as.Date("2001-12-31"), matches = "all", non.matches = TRUE,
               verbose = TRUE)

    time_match(data = df, s = ".*", search = c("quuz", "baz"), id = "foo",
               date = "bar", matches = "all", non.matches = TRUE,
               verbose = TRUE)

    time_match(data = df, s = ".*", search = c("quuz", "baz"), id = "foo",
               date = "bar", matches = "all", non.matches = TRUE,
               keep = c("xtra", "date"),
               verbose = TRUE)

    time_match(data = df, s = setNames("a", "foo"), search = c("baz", "quuz"), id = "foo",
               begin = as.Date("1990-01-01"), date = "bar",
               matches = "first.id", non.matches = FALSE,
               verbose = TRUE)

    time_match_set(data = df, s = "a", search = c("quuz", "baz"), id = "foo",
                   date = "bar",
                   set = c(2:4, 7:8), matches = "all", non.matches = TRUE,
                   keep = c("xtra", "date"),
                   verbose = TRUE)

    time_match_set(data = df, s = "a", search = c("quuz", "baz"), id = "foo",
                   date = "bar",
                   set = c(7:8), matches = "all", non.matches = TRUE,
                   keep = c("xtra", "date"),
                   verbose = TRUE)


    ## this function might be useful at some point, but maybe not
    df_first_instance <- function(data, ..., name = ".first_instance"){
        if((n <- nrow(data)) == 1) {
            data[[name]] <- 1L
            return(data)
        }
        nm <- as.character(eval(substitute(alist(...))))
        bad.names <- which(!nm %in% names(data))
        if(length(bad.names) > 0){
            stop("variable(s)", paste(nm[bad.names], collapse = ", "),
                 "is/are not in data")
        }
        code <- paste0("data[order(",
                       paste(paste0("data[['", nm, "']]"), collapse = ", "),
                       "), ]")
        data.ord <- eval(parse(text = code))
        a <- "2:n"
        b <- "1:(n-1)"
        code2 <- paste0("!(", paste0(paste0("data.ord[['", nm, "']][", a, "] == data.ord[['",
                                            nm, "']][", b, "]"),
                                     collapse = " & "), ")")
        f <- eval(parse(text = code2))
        data.ord[[name]] <- as.integer(c(TRUE, f))
        data.ord
    }

}

