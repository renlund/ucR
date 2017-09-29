##' find matches on variables in a given time period
##'
##' this is essentially a helper function for \code{time_match}, see the
##'     documentation
##'
##' @param pattern a search string to pass to \code{grepl}
##' @param x names of variables to search in (in order of importance, if
##'     applicable), if \code{NULL} all variables except 'id' and 'date' (which
##'     must exist) and 'begin' and 'end' (which might exist) are used as search
##'     variables
##' @param data a data set that must contain 'id' and 'date', and
##'     optionally 'begin' and 'end'. If the latter are missing, the earliest
##'     and latest dates will be used, respectively.
##' @param ... arguments (beyond 'pattern' and 'x') passed to \code{grepl}
##' @param long output format, if \code{TRUE} all hits have separate row, else
##'     the emphasis is on the first hit for each 'id' (by 'date' and 'x')
##' @param verbose if \code{TRUE} the function will give helpful and/or annoying
##'     messages sometimes
##' @param paste.alias if \code{long = FALSE} and if 'x' has a names attribute,
##'     this will be pasted onto some output variable names, unless you set
##'     override this behavour by setting this argument to zero
##' @return See \code{\link{time_match}} for details on output
##' @seealso \code{\link{time_match}}, \code{\link[base]{grepl}}
##' @author Henrik Renlund
time_match_rigid <- function(pattern, x = NULL, data, ...,
                             long = TRUE, verbose = TRUE, paste.alias = TRUE){
    ## -- sanity checks
    .required_properties(x = verbose, class = "logical", length = 1)
    if(verbose) cat("\n [Function ucR::time_match_rigid is verbose]\n Checking arguments and data\n")
    .required_properties(x = long, class = "logical", length = 1)
    .required_properties(x = paste.alias, class = "logical", length = 1)
    .required_properties(x = pattern, class = "character", length = 1)
    .required_properties(x = data, class = "data.frame")
    .required_properties(x = x, class = c("character", "NULL"))
    used_data_names <-  c("id", "begin", "end", "date")
    internal_names <- c("event", "time",
                        "match", "match.in",
                        "pattern", "alias",
                        "first.id", "first.id_date")
    ## -- require that 'id', 'date' and all x variables are in the data set
    .required_data_names(data.names = names(data),
                         required = c("id", "date", x))
    ## -- if x = NULL, search in all variables (except used_data_names)
    if(is.null(x)) x <- setdiff(names(data), used_data_names)
    ## -- do not allow the name of search variables to coincide with used_data_names
    .not_allowed_names(nm = x, no = used_data_names)
    ## -- rename if they coincide with internal names
    x.new <- .rename_if_in(nm = x, compare = internal_names, prefix = '.',
                           suffix = NULL, all = FALSE, limit = 10, verbose = verbose)
    if(!is.null(x.new)){
        names(data)[names(data) %in% x] <- x.new
        x <- x.new
    }
    ## -- keep all variables (if long = TRUE) not explicitly searched in ...
    keep <- if(long) setdiff(names(data), c(used_data_names, x)) else NULL
    ## -- ... but rename them if they coincide with internal names
    keep.new <- .rename_if_in(nm = keep, compare = internal_names, prefix = '.',
                           suffix = NULL, all = FALSE, limit = 10, verbose = verbose)
    if(!is.null(keep.new)){
        names(data)[names(data) %in% keep] <- keep.new
        keep <- keep.new
    }
    output_vars <- c(used_data_names, internal_names, keep)
    ## -- 'date' is key, so throw error if missing, let user sort this out
    if(any(is.na(data$date))){
        stop("Missing values in date.")
    }
    ## -- if 'begin'/'end' missing, set to earliest/lates dates available
    if(is.null(data$begin)) data$begin <- min(data$date)
    if(is.null(data$end)) data$end <- max(data$date)
    ## -- keep copy of data, one row per id, for later
    data.copy <- subset(data, !duplicated(id), select = used_data_names)
    data.copy$date <- as.Date(NA_character_)
    missing_in_nonmatches <- setdiff(output_vars, names(data.copy))
    ## -- derive time variables
    data$time  <- as.numeric(difftime(data$date, data$begin, units = "days"))
    for(K in setdiff(output_vars, names(data))){
        data[[K]] <- NA
    }
    ## -- filter data to relevant time period
    data <- data[data$date >= data$begin & data$date <= data$end, ]
    pattern.name <- names(pattern)
    alias <- if(is.null(pattern.name)) "p1" else pattern.name
    .alias <- if(is.null(pattern.name)) NULL else paste0(".", alias)
    .dots <- list(...) ## .dots <- as.list(NULL)
    R <- NULL
    if(verbose){
        cat(" Searching through variable:\n   ")
        if(length(x) == 0) cat("(no variabes to search in!)")
    }
    for(K in x){ ##  K <- x[1] ## for testing
        if(verbose) cat(K, ", ", sep = "")
        ## g <- grepl(pattern = pattern, x = data[[K]], ...)
        g <- do.call(grepl, args = c(list('pattern' = pattern,
                                          'x' = data[[K]]),
                                     .dots))
        if(sum(g) == 0) next
        tmp <- data[g, ]
        tmp$match <- tmp[[K]]
        tmp$match.in <- factor(K, levels = x)
        tmp$event <- 1L
        tmp$pattern <- pattern
        tmp$alias <- alias
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
               ## -- set relevant values
               B$date <- B$end
               B$event <- 0
               for(K in c("first.id", "first.id_date")){
                   B[[K]] <- 1L
               }
               B$time <- as.numeric(difftime(B$end, B$begin, units = "days"))
               B$pattern <- pattern
               B$alias <- alias
               rbind(S, B)
           } else {
               S
           }
    rownames(RET) <- NULL
    if(long){
        RET
    } else {
        foo <- function(E){
            n <- nrow(E)
            E$events <- if(sum(E$event) > 0) n else 0
            a <- E$match
            b <- E$match.in
            dont <- all(is.na(a))
            E$matches <- if(dont) NA else paste0(unique(a), collapse = " ")
            z <- paste0(unique(paste0(a, ":", b, ":", E$date)), collapse = " ")
            E$matches.info <- if(dont) NA else z
            D <- E[E$first.id == 1, c("id",  "begin", "end", "date",
                                 "event", "time", "match", "match.in",
                                 "pattern", "alias",
                                 "events", "matches", "matches.info")]
            ## -- variables are renamed only if x had a name attribute AND
            ## --   paste.alias is TRUE
            if(paste.alias) names(D)[4:13] <- paste0(names(D)[4:13], .alias)
            D
        }
        do.call(rbind, lapply(split(RET, RET$id), foo))
    }
}

##' find matches for specific units on variables in a given time period
##'
##' in a dataset with one or more variables (typically containing text)
##'     associated with a date, find matches on those variables for specific
##'     individuals within specifed time frames
##'
##' @param pattern a vector of search strings (regular expressions) (the names
##'     attribute will be used if it exists)
##' @param x names of variables to search in (given in order of importance), if
##'     missing all variables except id and date are chosen
##' @param data a data frame
##' @param id name of id variable (in 'data')
##' @param date name of associated date variable (in 'data')
##' @param units a vector of id's, or a data frame containing id's as well as
##'     (but optionally) 'begin' and 'end' variables
##' @param units.id variable name in 'set' to use as id (by default the same is
##'     'id')
##' @param begin variable name in 'set' to use as begin, if missing will be set
##'     to earliest date in data
##' @param end variable name in 'set' to use as end, if missing will be set to
##'     latest date in data
##' @param long if \code{TRUE} all matches will get a row, else first match gets
##'     details and information on all other matches is condensed
##' @param stack if \code{TRUE} results are stacked. Not stacking is only
##'     possible when \code{long = FALSE}.
##' @param ... arguments passed to \code{grepl}
##' @param verbose if \code{TRUE} the function will give helpful and/or annoying
##'     messages sometimes
##' @return The basic 'long' output is a data frame with
##'
##'  \itemize{\item id the id variable
##'
##'  \item begin the begin date (could be individual)
##'
##'  \item end the end date (could be individual)
##'
##'  \item date the date of assicated match
##'
##'  \item event indicator for a match
##'
##'  \item time days from 'begin' to 'date'
##'
##'  \item match the match found
##'
##'  \item match.in the variable the match was found in
##'
##'  \item pattern the pattern searched for
##'
##'  \item alias the name of pattern searched for (else p1, p2, etc)
##'
##'  \item first.id indicator for first occurence of associated id
##'
##'  \item first.id_date indicator for first occurence of associated id and date
##'
##'  \item ... all variables in data that are not id, date or search
##'     variables. These will be renamed if they are in conflict with output
##'     names. These will only be included in output when \code{long = TRUE}.}
##'
##' Note that any individual can have more than one match.
##'
##' The basic output when \code{long = FALSE} is one line per individual where
##'     the first match (by date and order of search variables, i.e. filtered on
##'     \code{first.id == 1}) is specified with some detail, and information on
##'     all subsequent matches is condensed.
##'
##'  \itemize{ \item id, begin, end, date, event, time, match, match.in, pattern, alias
##'     as before, but only relevant for the first match.
##'
##'  \item events the total number of matches found
##'
##'  \item matches all (unique) matches found, separated by a space
##'
##'  \item matches.info all (unique) match/mathing-variable/date-combinations
##'     separated by a space }
##'
##' If pattern is a vector, the results can be stacked or not. If they are not
##'     stacked, the format must necessarily be one line per individual
##'     (i.e. \code{long = FALSE}).  If the output is unstacked, all pattern
##'     specific output (i.e. all but id, begin and end) variables will get a
##'     suffix, either the names of the pattern vector, or a created one.
##'
##' @author Henrik Renlund
##' @export
time_match <- function(pattern, x = NULL, data, id = 'id', date = 'date',
                       units = NULL, units.id = id, begin = 'begin',
                       end = 'end', long = TRUE, stack = TRUE, ...,
                       verbose = TRUE){
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
    .required_properties(pattern, class = "character")
    .required_properties(x, class = c("character", "NULL"))
    .required_properties(data, class = "data.frame")
    .required_properties(id, class = "character", length = 1)
    .required_properties(date, class = "character", length = 1)
    .required_properties(units.id, class = "character")
    .required_properties(begin, class = c("Date", "character"), length = 1)
    .required_properties(end,   class = c("Date", "character"), length = 1)
    .required_properties(long, class = "logical", length = 1)
    .required_properties(stack, class = "logical", length = 1)
    .required_data_names(data.names = names(data),
                         required = c(id, date, x))
    if(long){
        if(!stack) warning("long format must be stacked")
        stack <- TRUE
    }
    used_data_names <-  c("id", "begin", "end", "date")
    ## comment: begin/end are not actually names in data that we use
    internal_names <- c("event", "time", "match", "match.in",
                        "first.id", "first.id_date", "pattern", "alias")
    ## -- make sure there are no name conflicts
    if(is.null(x)) x <- setdiff(names(data), used_data_names[c(1,4)])
    x.new <- .rename_if_in(nm = x,
                           compare = c(used_data_names, internal_names))
    if(!is.null(x.new)){
        names(data)[names(data) %in% x] <- x.new
        x <- x.new
    }
    keep <- if(long) setdiff(names(data), c(used_data_names, x)) else NULL
    keep.new <- .rename_if_in(nm = keep, compare = internal_names,
                              prefix = '.', verbose = verbose)
    ## -- ... but rename them if they coincide with internal names
    if(!is.null(keep.new)){
        names(data)[names(data) %in% keep] <- keep.new
        keep <- keep.new
    }
    ## -- if 'units' is NULL, use all 'id' from data
    if(is.null(units)){
        units <- unique(data[[id]])
    }
    ## -- if 'units' is just a vector of id:s, create a data frame
    if(is.null(dim(units))){
        units <- data.frame('id' = units)
        units[['begin']] <- data.begin
        units[['end']]   <- data.end
    } else { ## -- else select (and/or create) 'id', 'begin' and 'end'
        if(is.null(units[[units.id]])) stop("Need variable ", units.id," in 'units'")
        if(is.character(begin)){ ## -- if no 'begin' variable exists, create it
            if(is.null(units[[begin]])) units[[begin]] <- data.begin
        } else {
            units[['begin']] <- begin
            begin <- 'begin'
        }
        if(is.character(end)){ ## -- if no 'end' variable exists, create it
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
    data <- data[!na.indx, c(id, date, x, keep)]
    names(data) <- c("id", "date", x, keep)
    DATA <- merge(units, data, by = 'id', all.x = TRUE)
    DATA$date[is.na(DATA$date)] <- max(DATA$end, na.rm = TRUE)
    if(length(pattern) == 1){
        time_match_rigid(pattern = pattern, x = x, data = data,
                         ..., long = long, verbose = verbose,
                         paste.alias = TRUE)
    } else {
        pattern.names <- names(pattern)
        set.pattern.names <- sprintf("p%d", 1:length(pattern))
        paste.alias <- if(stack) FALSE else TRUE
        if(is.null(pattern.names)){
            names(pattern) <- set.pattern.names
            if(!stack & !long & verbose){
                cat(" (!) FYI: you might want to have 'pattern' named.",
                    "'p1', 'p2', etc will be used\n")
            }
        }
        R <- NULL
        N <- length(pattern)
        for(i in 1:N){ ## i = 3
            if(verbose) cat("\n Searching for regular expression:", pattern[i],
                            "\n", paste(rep("=", options("width")$width-2),
                                        collapse = ""))
            tm <- time_match_rigid(pattern = pattern[i], x = x, data = DATA,
                                   ..., long = long, verbose = verbose,
                                   paste.alias = paste.alias)
            if(stack){
                R <- if(is.null(R)) tm else rbind(R, tm)
            } else {
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
    time_match(data = df, pattern = '.', x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = FALSE, verbose = TRUE)

    time_match(data = df, pattern = '.', x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = TRUE, stack = TRUE, verbose = TRUE)

    time_match(data = df, pattern = '.', x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = TRUE, verbose = TRUE)

    ## match all, twice
    time_match(data = df, pattern = c('.', '.*'),
               x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = FALSE, verbose = TRUE)

    time_match(data = df, pattern = c('.', '.*'),
               x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = TRUE, stack = TRUE, verbose = TRUE)

    time_match(data = df, pattern = c('.', '.*'),
               x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = TRUE, verbose = TRUE)

    ## match some
    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = FALSE, verbose = TRUE)

    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = TRUE, stack = TRUE, verbose = TRUE)

    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID', begin = 'arrival',
               end = 'death.date', long = FALSE, stack = TRUE, verbose = TRUE)

    ## use set dates
    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID',
               begin = as.Date('2000-01-01'), end = as.Date('2001-12-31'),
               long = FALSE, stack = FALSE, verbose = FALSE)

    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID',
               begin = as.Date('2000-01-01'), end = as.Date('2001-12-31'),
               long = TRUE, stack = TRUE, verbose = FALSE)

    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo',
               date = 'bar', units = Set, units.id = 'ID',
               begin = as.Date('2000-01-01'), end = as.Date('2001-12-31'),
               long = FALSE, stack = TRUE, verbose = FALSE)

    ## use set dates, vector of id's
    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo', date = 'bar',
               units = Set$ID, begin = as.Date('2000-01-01'),
               end = as.Date('2001-12-31'), verbose = FALSE)

    ## vector of id's
    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo', date = 'bar',
               units = Set$ID, verbose = FALSE)

    ## vector of id's
    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo', date = 'bar',
               units = Set$ID, verbose = FALSE)

    ## 'incomplete' units
    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo', date = 'bar',
               units = Set[, c('ID', 'arrival')], units.id = 'ID',
               begin = 'arrival', end = 'death.date', verbose = FALSE)

    ## 'incomplete' units
    time_match(data = df, pattern = setNames(c('a', 'b'), c("Foo", "Bar")),
               x = c('baz', 'quuz'), id = 'foo', date = 'bar',
               units = Set[, c('ID', 'death.date')], units.id = 'ID',
               begin = 'arrival', end = 'death.date', verbose = FALSE)

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
    time_match_rigid(data = df, pattern = '.', x = c('baz', 'quuz'),
                      long = TRUE, verbose = TRUE)
    time_match_rigid(data = df, pattern = '.', x = c('baz', 'quuz'),
                     long = FALSE, verbose = TRUE)
    time_match_rigid(data = df, pattern = setNames('.', "ALL"), x = c('baz', 'quuz'),
                      long = FALSE, verbose = TRUE)
    time_match_rigid(data = df, pattern = setNames('.', "ALL"), x = c('baz', 'quuz'),
                      long = FALSE, verbose = TRUE, paste.alias = FALSE)

    ## non matches
    time_match_rigid(data = df, pattern = 'q', x = c('baz', 'quuz'),
                      long = TRUE, verbose = TRUE)
    time_match_rigid(data = df, pattern = 'q', x = c('baz', 'quuz'),
                      long = FALSE, verbose = TRUE)
    time_match_rigid(data = df, pattern = setNames('q', "NONE"), x = c('baz', 'quuz'),
                     long = FALSE, verbose = TRUE)
    time_match_rigid(data = df, pattern = setNames('q', "NONE"), x = c('baz', 'quuz'),
                      long = FALSE, verbose = TRUE, paste.alias = FALSE)

    ## some matches
    time_match_rigid(data = df, pattern = 'a', x = c('baz'),
                      long = TRUE, verbose = TRUE)
    time_match_rigid(data = df, pattern = 'a', x = c('baz'),
                      long = FALSE, verbose = TRUE)
    time_match_rigid(data = df, pattern = setNames('a', "TheA"), x = c('baz'),
                      long = FALSE, verbose = TRUE)

    ## wrong case, but ignored
    time_match_rigid(data = df, pattern = 'A', x = c('baz'),
                      long = TRUE, ignore.case = TRUE, verbose = TRUE)
    time_match_rigid(data = df, pattern = 'A', x = c('baz'),
                      long = FALSE, ignore.case = TRUE, verbose = TRUE)
    time_match_rigid(data = df, pattern = setNames('A', "TheA"), x = c('baz'),
                      long = FALSE, ignore.case = TRUE, verbose = TRUE)

    ## test
    time_match_rigid(data = df, pattern = '1', x = c('time'),
                      long = TRUE, verbose = TRUE)
    time_match_rigid(data = df, pattern = 'x', x = c('time'),
                      long = FALSE, verbose = TRUE)
    time_match_rigid(data = df, pattern = setNames('x', "dieX"), x = c('time'),
                      long = FALSE, verbose = TRUE)


}


#################################################################


.required_properties <- function(x, class = NULL, length = NULL, nm = NULL){
    if(is.null(nm)) nm <- as.character(substitute(x))
    if(!is.null(class)){
        s <- paste0("Argument '", nm, "' fails to be in class: ",
                    paste0(class, collapse = ", or"))
        if(!any(class(x) %in% class)) stop(s)
    }
    if(!is.null(length)){
        s <- paste0("Argument '", nm, "' fails to be have length: ",
                    paste0(length, collapse = ", or"))
        if(!length(x) %in% length) stop(s)
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

    fooz <- function(pattern, x, ...){
        L <- list(...)
        cat("names L:", paste0(names(L), collapse = ", "), "\n")
        L2 <- c(list('pattern'=pattern, 'x' = x), L)
        cat("names L2:", paste0(names(L2), collapse = ", "), "\n")
        do.call(what = grepl, args = L2)
    }
    fooz(pattern = "a", x = letters)
    fooz(pattern = "a", x = c(letters, LETTERS), ignore.case = TRUE)
    grepl("a", c(letters, LETTERS), ignore.case = TRUE)


}
