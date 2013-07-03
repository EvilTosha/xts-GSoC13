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

## works extremely slow
as.data.frame.xtsdfn <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (is.null(row.names))
    row.names <- index(x)
  index.num <- 0
  index.char <- 0
  ## doesn't keep colnames
  xts.list <- lapply(x$index,
                     function(i) {
                       if (i) {
                         index.num <- index.num + 1
                         x$numeric[, index.num]
                       }
                       else {
                         index.char <- index.char + 1
                         x$character[, index.char]
                       }
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
  as.xtsdfn(x$numeric[i, j.num], x$character[i, j.char], ind)
}
