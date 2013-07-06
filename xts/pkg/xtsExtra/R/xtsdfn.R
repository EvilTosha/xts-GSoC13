xtsdfn <- function(..., column.classes = NULL, index = NULL){
  if (is.null(index) && !is.xts(..1))
    stop("First column needs to be an xts-series if index is not provided")

  if(is.null(index)) index <- index(..1)

  dots <- list(...)

  smodes <- sapply(dots, storage.mode)

  x <- list()
  x$index <- index
  x$smodes <- unique(smodes)

  recycle.columns <- FALSE
  if (is.null(column.classes))
    recycle.columns <- TRUE

  for(smode in unique(smodes)) {
    x[[smode]] <- do.call(cbind, dots[smode == smodes])
    if (recycle.columns)
      column.classes <- c(column.classes, rep(smode, ncol(x[[smode]])))
  }

  x$column.classes <- column.classes
  class(x) <- "xtsdfn"

  x
}

as.xtsdfn.data.frame <- function(df, index = NULL) {
  if (is.null(index)) index <- rownames(df)
  column.classes <- vector("numeric", ncol(df))

  df.column.classes <- sapply(df, storage.mode)
  smodes <- unique(df.column.classes)

  x <- list()
  x$index <- index
  x$smodes <- smodes

  for (smode in smodes) {
    x[[smode]] <- as.xts(df[, df.column.classes == smode])
    column.classes[df.column.classes == smode] <- smode
  }

  x$column.classes <- column.classes
  class(x) <- "xtsdfn"

  x
}

index.xtsdfn <- function(x) x$index

dim.xtsdfn <- function(x) c(length(index(x)), length(x$column.classes))

## aux index maps each column index in xtsdfn object to indices in numeric and character parts
## aux index is only useful in conjuction with original index
get.aux.index <- function(x) {
  class.indexes <- list()
  for (smode in x$smodes)
    class.indexes[[smode]] <- 0

  ncol <- length(x$column.classes)
  index.aux <- vector("numeric", ncol)
  col.classes <- x$column.classes
  for (i in 1:ncol) {
    class.indexes[[col.classes[i]]] <- class.indexes[[col.classes[i]]] + 1
    index.aux[i] <- class.indexes[[col.classes[i]]]
  }
  index.aux
}


## works extremely slow
as.data.frame.xtsdfn <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (is.null(row.names))
    row.names <- index(x)
  index.aux <- get.aux.index(x)
  xts.list <- lapply(1:length(x$column.classes), function(i) x[[x$column.classes[i]]][, index.aux[i]])
  class(xts.list) <- "xtsdf"
  as.data.frame(xts.list)
}

print.xtsdfn <- function(x, ...) {
  print(as.data.frame(x))
}

`[.xtsdfn` <- function(x, i, j, drop = FALSE, which.i = FALSE, ...) {
  ## some magic with indices
  index.aux <- get.aux.index(x)
  class.xts <- list()
  ## FIXME: dirty hack
  if (missing(j)) j <- 1:ncol(x)
  for (smode in x$smodes)
    class.xts[[smode]] <- x[[smode]][i, index.aux[intersect(which(x$column.classes == smode), j)]]

  do.call(xtsdfn, append(class.xts, list(index = x$index, column.classes = x$column.classes[j])))
}
