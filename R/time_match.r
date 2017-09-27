time_match_rigid <- function(data, s, search = NULL, long = TRUE, verbose = TRUE){
    ## -- sanity checks
    if(verbose) cat("\n [Function ucR::time_match_rigid is verbose]\n Checking arguments and data\n")
    used_data_names <-  c("id", "date", "begin", "end")
    internal_names <- c("event", "time", "match", "match.in", "time2",
                        "first.id", "first.id_date")
    ## -- require that 'id', 'date' and all search variables are in the data set
    .required_data_names(data.names = names(data),
                         required = c(used_data_names[1:2], search))
    ## -- if search = NULL, search in all variables (except 'id' and 'date')
    if(is.null(search)) search <- setdiff(names(data), used_data_names)
    ## -- do not allow the name of search variables to coincide with used data names
    .not_allowed_names(nm = search, no = used_data_names)
    search.new <- .rename_if_in(nm = search, compare = internal_names,
                                prefix = '.', verbose = verbose)
    if(!is.null(search.new)){
        names(data)[names(data) %in% search] <- search.new
        search <- search.new
    }
    ## -- keep all variables (if long = TRUE) not explicitly searched in ...
    keep <- if(long) setdiff(names(data), c(used_data_names, search)) else NULL
    keep.new <- .rename_if_in(nm = keep, compare = internal_names,
                              prefix = '.', verbose = verbose)
    ## -- ... but rename them if they coincide with internal names
    if(!is.null(keep.new)){
        names(data)[names(data) %in% keep] <- keep.new
        keep <- keep.new
    }
    output_vars <- c(used_data_names, internal_names, keep)
    ## -- 'date' is key, so throw error if missing, let user sort this out
    if(any(is.na(data$date))){
        warning("Missing values in date, these are removed.")
        data <- data[!is.na(data$date), ]
    }
    ## -- if 'begin'/'end' missing, set to earliest/lates dates available
    if(is.null(data$begin)) data$begin <- min(data$date)
    if(is.null(data$end)) data$end <- max(data$date)
    ## -- keep copy of data, one row per id, for later
    data.copy <- subset(data, !duplicated(id), select = c("id", "date", "begin", "end"))
    data.copy$date <- as.Date(NA_character_)
    missing_in_nonmatches <- setdiff(output_vars, names(data.copy))
    ## -- derive time variables
    data$time  <- as.numeric(difftime(data$date, data$begin, units = "days"))
    data$time2 <- as.numeric(difftime(data$end, data$date, units = "days"))
    for(K in setdiff(output_vars, names(data))){
        data[[K]] <- NA
    }
    data <- data[data$date >= data$begin & data$date <= data$end, ]
    R <- NULL
    if(verbose){
        cat(" Searching through variable:\n   ")
        if(length(search) == 0) cat("(no variabes to search in!)")
    }
    for(K in search){ ##  K <- search[1] ## for testing
        if(verbose) cat(K, ", ", sep = "")
        g <- grepl(pattern = s, x = data[[K]])
        if(sum(g) == 0) next
        tmp <- data[g, ]
        tmp$match <- tmp[[K]]
        tmp$match.in <- factor(K, levels = search)
        tmp$event <- 1L
        R <- if(is.null(R)) tmp else rbind(R, tmp)
    }
    if(verbose) cat("and search complete.\n Fixing output data\n\n")
    ## -- order matches and create indicators for first id and first date
    ## -- if there are no matches, object R is still NULL (treat separately)
    if(is.null(R)){
        S <- as.data.frame(matrix(vector(),
                                  nrow = 0,
                                  ncol = length(output_vars),
                                  dimnames = list(c(), output_vars)))
    } else {
        S <- R[order(R$id, R$date, R$match.in), output_vars]
        n <- nrow(S)
        ## -- create indicators for first instance of each id and first instance
        ## -- of each id + date combination. This code must treat the case of
        ## -- length 1 vectors separately
        if(n > 1){
            S$first.id <- as.integer(c(TRUE, !(S$id[2:n] == S$id[1:(n-1)])))
            S$first.id_date <- as.integer(
                c(TRUE, !(S$id[2:n] == S$id[1:(n-1)] &
                          S$date[2:n] == S$date[1:(n-1)]))
                )
        } else {
            S$first.id <- 1L
            S$first.id_date <- 1L
        }
    }
    ## -- create the object to return
    RET <- if(nrow(B <- subset(data.copy, !id %in% S$id)) > 0){
               ## -- create all output variables
               for(K in missing_in_nonmatches){
                   B[[K]] <- NA
               }
               B$date <- B$end
               for(K in c("event", "time2")) B[[K]] <- 0
               for(K in c("first.id", "first.id_date")){
                   B[[K]] <- 1L
               }
               B$time <- as.numeric(difftime(B$end, B$begin, units = "days"))
               rbind(S, B)
           } else {
               S
           }
    rownames(RET) <- NULL
    if(long){
        RET
    } else {
        foo <- function(X){
            n <- nrow(X)
            X$events <- if(sum(X$event) > 0) n else 0
            x <- X$match
            y <- X$match.in
            dont <- all(is.na(x))
            X$matches <- if(dont) NA else paste0(unique(x), collapse = " ")
            z <- paste0(unique(paste0(x, ":", y, ":", X$date)), collapse = " ")
            X$matches.info <- if(dont) NA else z
            X[X$first.id == 1, c("id",  "begin", "end", "date",
                                 "event", "time", "match", "match.in",
                                 "events", "matches", "matches.info")]
        }
        do.call(rbind, lapply(split(RET, RET$id), foo))
    }
}

##' find matches on variables in a given time period
##'
##' in a dataset with one or more variables (typically containing text or codes)
##'     associated with a date, find matches on those variables within specifed
##'     time frames
##' @title find matches
##' @param data a data frame
##' @param s a search string (regular expression), or vector thereof (in the
##'     latter case it is a good idea to have entries named)
##' @param search names of variables to search in (given in order of
##'     importance), if missing all variables except id and date are chosen
##' @param id name of id variable
##' @param date name of associated date variable
##' @param units a vector of id's, or a data frame containing id's as well as (but
##'     optionally) 'begin' and 'end' variables
##' @param units.id variable name in 'set' to use as id
##' @param begin variable name in 'set' to use as begin, if missing will be set
##'     to earliest date in data
##' @param end variable name in 'set' to use as end, if missing will be set to
##'     latest date in data
##' @param long if \code{TRUE} all matches will get a row, else first match gets
##'     details and information on all other matches is condensed
##' @param stack if \code{TRUE} results are stacked with a 'search.term'
##'     variable. Note that if 'long' is \code{TRUE}, then stack will be set to \code{TRUE}
##' @param verbose if \code{TRUE} the function will give helpful and/or annoying
##'     messages sometimes
##' @return The basic 'long' output is a data frame with \itemize{
##'  \item id the id variable
##'  \item begin the begin date (could be individual)
##'  \item end the end date (could be individual)
##'  \item date the date variable of XK
##'  \item event 1 is if match found, else 0.
##'  \item t days from 'begin' to 'date'
##'  \item match the match found
##'  \item match.in match found in this variable
##'  \item t2 days from 'date' to end
##'  \item first.id indicator for first occurence of associated id
##'  \item first.date indicator for first occurence of associated id and date
##'  \item ... all variables in data that are not id, date or search
##'     variables. These will be renamed if they are in conflict with output
##'     names. These will only be included in output when \code{long = TRUE}.
##' }
##' Note that any individual can have more than one match.
##'
##' The basic output when \code{long = FALSE} is one line per individual where
##'     the first match (by date and order of search variables) is specified
##'     with some detail and information on all subsequent matches is condensed.
##' \itemize{ XK
##'  \item id, begin, end, date, event, time, match, match.in as before, but
##'     only relevant for the first match.
##'  \item events the total number of matches found
##'  \item matches all (unique) matches found, separated by a space
##'  \item matches.info all (unique) match:mathing variable:date-cominations
##'     separated by a space
##' }
##'
##' If s is a vector, the results can be stacked or not. If they are not
##'     stacked, the format must necessarily be one line per individual
##'     (i.e. \code{long = FALSE}). If the result is stacked, another variable
##'     'search' will either give the pattern searched for, or, if the vector of
##'     pattern has names, the corresponding name. If the output is unstacked,
##'     all pattern specific output (i.e. all but id, begin and end) variables
##'     will get a suffix, either the names of the pattern vector, or a created
##'     one.
##'
##' @author Henrik Renlund
##' @export
time_match <- function(data, s, search = NULL, id = 'id', date = 'date',
                       units = NULL, units.id = id, begin = 'begin',
                       end = 'end', long = TRUE, stack = TRUE, verbose = TRUE){
    .required_properties(verbose, class = "logical", length = 1, nm = "verbose")
    if(verbose) cat("\n [Function ucR::time_match_set set to verbose.]\n",
                    "Checking arguments and preparing data before calling",
                    "time_match...\n")
    data.begin <- if(class(begin) == "Date"){
                      begin
                  } else {
                      min(data[[date]], na.rm = TRUE)
                  }
    data.end   <- if(class(end) == "Date"){
                      end
                  } else {
                      max(data[[date]], na.rm = TRUE)
                  }
    if(is.null(begin)) begin <- data.begin
    if(is.null(end))   end   <- data.end
    .required_properties(id, class = "character", length = 1, nm = "id")
    .required_properties(date, class = "character", length = 1, nm = "date")
    .required_properties(begin, class = c("Date", "character"), length = 1, nm = "begin")
    .required_properties(end,   class = c("Date", "character"), length = 1, nm = "end")
    .required_data_names(data.names = names(data),
                         required = c(id, date, search))
    .required_properties(long, class = "logical", length = 1, nm = "long")
    .required_properties(stack, class = "logical", length = 1, nm = "stack")
    if(long){
        if(!stack) warning("long format must be stacked")
        stack <- TRUE
    }
    used_data_names <-  c("id", "begin", "end", "date")
    internal_names <- c("event", "time", "match", "match.in", "time2",
                        "first.id", "first.id_date", if(stack) "s")
    ## -- make sure there are no name conflicts
    .required_properties(search, class = c("character", "NULL"))
    if(is.null(search)) search <- setdiff(names(data), used_data_names[1:2])
    search.new <- .rename_if_in(nm = search,
                                compare = c(used_data_names, internal_names))
    if(!is.null(search.new)){
        names(data)[names(data) %in% search] <- search.new
        search <- search.new
    }
    keep <- if(long) setdiff(names(data), c(used_data_names, search)) else NULL
    keep.new <- .rename_if_in(nm = keep, compare = internal_names,
                              prefix = '.', verbose = verbose)
    ## -- ... but rename them if they coincide with internal names
    if(!is.null(keep.new)){
        names(data)[names(data) %in% keep] <- keep.new
        keep <- keep.new
    }
    ## -- if 'units' is just a vector of id:s, create a data frame
    if(is.null(units)){
        units <- unique(data[[id]])
    }
    if(is.null(dim(units))){
        units <- data.frame('id' = units)
        units[['begin']] <- data.begin
        units[['end']]   <- data.end
    } else { ## -- else select (and/or create) 'id', 'begin' and 'end'
        if(is.null(units[[units.id]])) stop("Need an 'id' variable in units")
        if(is.character(begin)){
            if(is.null(units[[begin]])) units[[begin]] <- data.begin
        } else {
            units[['begin']] <- begin
            begin <- 'begin'
        }
        if(is.character(end)){
            if(is.null(units[[end]])) units[[end]] <- data.end
        } else {
            units[['end']] <- end
            end <- 'end'
        }
        units <- units[, c(units.id, begin, end)]
        names(units) <- c('id', 'begin', 'end')
    }
    ## -- missing date in data will be problematic, throw error
    na.indx <- is.na(data[[date]])
    if(any(na.indx)){
        warning("Missing 'date' at rows:", paste0(which(na.indx), collapse = ", "),
                "\nThese will be removed\n")
    }
    ## -- fix data
    data <- data[!na.indx, c(id, date, search, keep)] ## -keep?
    names(data) <- c("id", "date", search, keep) ## -keep?
    DATA <- merge(units, data, by = 'id', all.x = TRUE)
    DATA$date[is.na(DATA$date)] <- max(DATA$end, na.rm = TRUE)
    ## START--
    if(length(s) == 1){
        time_match_rigid(data = DATA, s = s, search = search,
                          long = long, verbose = verbose)
    } else {
        wide.names <- names(s)
        long.names <- wide.names
        if(is.null(wide.names)){
            if(verbose & !stack) cat(" (!) FYI: you might want to have 's' named.",
                                     "Prefix '.s1', '.s2', etc will be added\n")
            wide.names <- sprintf("s%d", 1:length(s))
            long.names <- s
        }
        R <- NULL
        N <- length(s)
        for(i in 1:N){ ## i = 3
            if(verbose) cat("\n Searching for regular expression:", s[i],
                            "\n", paste(rep("=", options("width")$width-2),
                                        collapse = ""))
            tm <- time_match_rigid(data = DATA, s = s[i], search = search,
                                    long = long, verbose = verbose)
            if(stack){
                tm[['s']] <- long.names[i]
                R <- if(is.null(R)) tm else rbind(R, tm)
            } else {
                names(tm)[4:11] <- paste0(names(tm)[4:11], ".", wide.names[i])
                R <- if(is.null(R)) tm else merge(R, tm, by = c("id", "begin", "end"))
            }
        }
        R
    }
}


#################################################################

if(FALSE){

    df <- data.frame(
        foo = rep(1:5, c(4, 3, 1, 1, 1)),
        bar = as.Date("2001-01-01") + c(-371,1,1,2, 2,3,371, 0, 372, 4),
        baz =  c("b","a","a","b", "a","b","b", "a", "b", "b"),
        quuz = c("a","b","a","b", "b","a","a", "b", "a", "b"),
        xtra = sprintf("extra%d", 1:10),
        time = sprintf("date%d", 1:10)
    )
    df <- df[sample(1:nrow(df)), ]
    Set <- data.frame(
        ID = c(2:4, 6:7),
        arrival = as.Date("2000-06-06") + c(0,10,365,366,367),
        death = c(1,1,1,0,0),
        death.date = as.Date("2001-06-06") + c(0,100,200,720,720)
    )
    Set <- Set[sample(1:nrow(Set)), ]

    ## match all
    time_match(data = df, s = '.', search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = FALSE, verbose = TRUE)

    time_match(data = df, s = '.', search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = TRUE, stack = TRUE, verbose = TRUE)

    time_match(data = df, s = '.', search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = TRUE, verbose = TRUE)

    ## match all, twice
    time_match(data = df, s = c('.', '.*'),
               search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = FALSE, verbose = TRUE)

    time_match(data = df, s = c('.', '.*'),
               search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = TRUE, stack = TRUE, verbose = TRUE)

    time_match(data = df, s = c('.', '.*'),
               search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = TRUE, verbose = TRUE)

    ## match some
    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = FALSE, verbose = TRUE)

    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = TRUE, stack = TRUE, verbose = TRUE)

    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = TRUE, verbose = TRUE)

    ## use set dates
    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID',
               begin = as.Date('2000-01-01'), end = as.Date('2001-12-31'),
               long = FALSE, stack = FALSE, verbose = FALSE)

    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID',
               begin = as.Date('2000-01-01'), end = as.Date('2001-12-31'),
               long = TRUE, stack = TRUE, verbose = FALSE)

    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID',
               begin = as.Date('2000-01-01'), end = as.Date('2001-12-31'),
               long = FALSE, stack = TRUE, verbose = FALSE)

    ## use set dates, vector of id's
    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo', date = 'bar',
               units = Set$ID, begin = as.Date('2000-01-01'),
               end = as.Date('2001-12-31'), verbose = FALSE)

    ## vector of id's
    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo', date = 'bar',
               units = Set$ID, verbose = FALSE)

    ## vector of id's
    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo', date = 'bar',
               units = Set$ID, verbose = FALSE)

    ## 'incomplete' units
    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo', date = 'bar',
               units = Set[, c('ID', 'arrival')], units.id = 'ID',
               begin = 'arrival', end = 'death.date', verbose = FALSE)

    ## 'incomplete' units
    time_match(data = df, s = setNames(c('a', 'b'), c("Foo", "Bar")),
               search = c('baz', 'quuz'), id = 'foo', date = 'bar',
               units = Set[, c('ID', 'death.date')], units.id = 'ID',
               begin = 'arrival', end = 'death.date', verbose = FALSE)


    ## data = df
    ## s = setNames(c('a', 'b'), c("Foo", "Bar"))
    ## search = c('baz', 'quuz')
    ## id = 'foo'
    ## date = 'bar'
    ## units = Set$ID
    ## begin = as.Date('2000-01-01')
    ## end = as.Date('2001-12-31')
    ## verbose = FALSE

    ## -----------------------------------------------------------------

    df <- data.frame(
        id = rep(1:5, c(4, 3, 1, 1, 1)),
        date = as.Date("2001-01-01") + c(-371,1,1,2, 2,3,371, 0, 372, 4),
        baz =  c("b","a","a","b", "a","b","b", "a", "b", "b"),
        quuz = c("a","b","a","b", "b","a","a", "b", "a", "b"),
        xtra = sprintf("extra%d", 1:10),
        time = sprintf("date%d", 1:10)
    )
    df <- df[sample(1:nrow(df)), ]

    ## all matches
    time_match_rigid(data = df, s = '.', search = c('baz', 'quuz'),
                      long = TRUE, verbose = TRUE)
    time_match_rigid(data = df, s = '.', search = c('baz', 'quuz'),
                      long = FALSE, verbose = TRUE)

    ## non matches
    time_match_rigid(data = df, s = 'q', search = c('baz', 'quuz'),
                      long = TRUE, verbose = TRUE)
    time_match_rigid(data = df, s = 'q', search = c('baz', 'quuz'),
                      long = FALSE, verbose = TRUE)

    ## some matches
    time_match_rigid(data = df, s = 'a', search = c('baz'),
                      long = TRUE, verbose = TRUE)
    time_match_rigid(data = df, s = 'a', search = c('baz'),
                      long = FALSE, verbose = TRUE)

    ## test
    time_match_rigid(data = df, s = '1', search = c('time'),
                      long = TRUE, verbose = TRUE)
    time_match_rigid(data = df, s = 'x', search = c('time'),
                      long = FALSE, verbose = TRUE)


    ## data = df
    ## s = 'a'
    ## search = c('baz', 'quuz')
    ## verbose = TRUE


}


#################################################################


.required_properties <- function(x, class = NULL, length = NULL, nm = NULL){
    if(!is.null(class)){
        s <- paste0(nm, " argument fails to be in class:", paste0(class, collapse = ", or"))
        if(!any(class(x) %in% class)) stop(s)
    }
    if(!is.null(length)){
        s <- paste0(nm, " argument fails to be have length:", paste0(length, collapse = ", or"))
        if(!length(x) %in% length) stop()
    }
}

.required_data_names <- function(data.names, required){
    badname.indx <- which(!required %in% data.names)
    if(length(badname.indx) > 0){
        stop("Some variable names required (specifically: ",
             paste(required[badname.indx], collapse = ", "),
             ") does not exist in data.")
    }
    invisible(NULL)
}


.not_allowed_names <- function(nm, no){
    badname.indx <- which(nm %in% no)
    if(length(badname.indx) > 0){
        stop("Name conflict, don't want ",
             paste0(nm, collapse = "; "), " and ",
             paste0(no, collapse = "; "), " to intersect.")
    }
    invisible(NULL)
}


.rename_if_in <- function(nm, compare, prefix = '.', suffix = NULL,
                          all = FALSE, limit = 10, verbose = TRUE){
    if(length(prefix) > 1 | length(suffix) > 1) stop("bad prefix or suffix")
    nm.org <- nm
    rename <- which(nm %in% compare)
    if(length(rename) == 0){
        NULL
    } else {
        if(verbose) message(" (!) I will try to alter some variable names")
        dummy <- 0
        while(length(rename) > 0 & dummy < limit){
            if(all){
                nm <- paste0(prefix, nm, suffix)
            } else {
                nm[rename] <- paste0(prefix, nm[rename], suffix)
            }
            rename <- which(nm %in% compare)
            dummy <- dummy + 1
        }
        if(length(rename) > 0) {
            stop("Renaming of conflicting variables failed.")
        }
        if(verbose){
            i <- which(nm.org != nm)
            x <- paste(paste0(nm.org[i], " -> ", nm[i]), collapse = ", ")
            message("     Name changes: ", x)
        }
        nm
    }
}

#################################################################

if(FALSE){

    .not_allowed_names(letters[1:3], letters[3:5])
    .required_data_names(letters[1:3], letters[2:5])
    .rename_if_in(letters[1:3], letters[3:5])
    .rename_if_in("a", c("a", ".a", "..a"))
    .rename_if_in("a", c("a", ".a", "..a", "...a"), limit = 3)

}
