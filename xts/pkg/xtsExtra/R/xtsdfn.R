##' Function for creating xtsdf object (2013 edition prototype)
##'
##' .. content for \details{} ..
##' @title
##' @param num an \code{xts} object containing numeric part
##' @param char an \code{xts} object containing character part
##' @param index logical vector of length \eqn{ncol(num) + ncol(char))} with \eqn{ncol(num)}
##' TRUE's and \eqn{ncol(char)} FALSE's. TRUE indicates numeric column
##' @return an \code{xtsdf} object
##' @author EvilTosha
as.xtsdfn <- function(num, char, index) {
  x <- list(numeric = num, character = char, index = index)
  class(x) <- "xtsdfn"
  x
}

index.xtsdfn <- function(x) index(x$numeric)

## aux index maps each column index in xtsdfn object to indices in numeric and character parts
## aux index is only useful in conjuction with original index
get.aux.index <- function(index) {
  index.num <- 0
  index.char <- 0
  index.aux <- vector("numeric", length(index))
  for (i in 1:length(index)) {
    if (index[i]) {
      index.num <- index.num + 1
      index.aux[i] <- index.num
    }
    else {
      index.char <- index.char + 1
      index.aux[i] <- index.char
    }
  }
  index.aux
}


## works extremely slow
as.data.frame.xtsdfn <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (is.null(row.names))
    row.names <- index(x)
  ## warning: ugly code
  ## for some reason in R changes in variable made inside closure don't change actual variable outside
  index.aux <- get.aux.index(x$index)
  xts.list <- lapply(1:length(x$index),
                     function(i) {
                       if (x$index[i]) x$numeric[, index.aux[i]]
                       else            x$character[, index.aux[i]]
                     })
  class(xts.list) <- "xtsdf"
  as.data.frame(xts.list)
}

print.xtsdfn <- function(x, ...) {
  print(as.data.frame(x))
}

`[.xtsdfn` <- function(x, i, j, drop = FALSE, which.i = FALSE, ...) {
  j.num <- intersect(which(x$index), j)
  j.char <- intersect(which(x$index == FALSE), j)
  ind <- x$index[j]
  index.aux <- get.aux.index(x$index)
  as.xtsdfn(x$numeric[i, index.aux[j.num]], x$character[i, index.aux[j.char]], ind)
}
