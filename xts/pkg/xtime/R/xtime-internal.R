.MATCH.xtime <-
function(x, table, no.match=NA, ...) {
    i <- table
    if (missing(i)) {
        i <- 1:NROW(x)
    }
    else if (is.xtime(i)) {
        i <- match(i, x)
    }
    else if (!missing(i) && is.numeric(i)) {
        if (.Call("any_negative", i, PACKAGE = "xts")) {
            if (!all(i <= 0)) 
                stop("only zeros may be mixed with negative subscripts")
            i <- (1:length(x))[i]
        }
        if (max(i) > length(x)) 
            stop("subscript out of bounds")
    }
    else if (inherits(i, "AsIs") && is.character(i)) {
        i <- match(i, format(x))
    }
    else if (timeBased(i)) {
        if (inherits(i, "POSIXct")) {
            i <- which(!is.na(match(x, i)))
        }
        else {
            i <- which(!is.na(match(x, as.POSIXct(as.character(i)))))
        }
        i[is.na(i)] <- 0
    }
    else if (is.logical(i)) {
        i <- which(i)
    }
    else if (is.character(i)) {
        if (length(i) == 1 && !identical(integer(), grep("^T.*?/T", 
            i[1]))) {
            i <- gsub("T|:", "", i)
            i <- strsplit(i, "/")[[1]]
            i <- .makeISO8601TT(x, i[1], i[2])
        }
        i.tmp <- NULL
        tz <- as.character(indexTZ(x))
        for (ii in i) {
            adjusted.times <- .parseISO8601(ii, x[1], 
                x[NROW(x)], tz = tz)
            if (length(adjusted.times) > 1) {
                firstlast <- c(seq.int(xts:::binsearch(adjusted.times$first.time, 
                  x, TRUE), xts:::binsearch(adjusted.times$last.time, 
                  x, FALSE)))
                if (isOrdered(firstlast, strict = FALSE)) 
                  i.tmp <- c(i.tmp, firstlast)
            }
        }
        i <- i.tmp
    }
    if (!isOrdered(i, strictly = FALSE)) {
        i <- sort(i)
    }
    zero.index <- xts:::binsearch(0, i, NULL)
    if (!is.na(zero.index)) 
        i <- i[-zero.index]
    i
    tmp <- rep(no.match,length(x))
    tmp[i] <- 1:length(i)
    tmp
}

