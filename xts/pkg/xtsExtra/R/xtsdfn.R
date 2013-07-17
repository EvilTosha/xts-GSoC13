## Implementation model:
## 1) An xtsdfn object is a list of xts object with some auxiliary parameters.
## 2) There is one xts object for any storage mode ("character", "double", ...).
## 3) Vector of all storage modes (smodes) is contained in parameter x$smodes
## 4) Parameter x$column.smodes contains vector of smodes, which length is sum of lengths of all xts objects.
##    i'th element in x$column.smodes represents storage mode of i'th column

xtsdfn <- function(..., column.smodes = NULL, index = NULL){
  if (is.null(index) && !is.xts(..1))
    stop("First column needs to be an xts-series if index is not provided")

  if (is.null(index)) index <- index(..1)

  x <- list()
  x$index <- index

  dots <- list(...)
  smodes <- sapply(dots, storage.mode)
  x$smodes <- unique(smodes)

  recycle.columns <- FALSE
  if (is.null(column.smodes))
    recycle.columns <- TRUE

  for(smode in unique(smodes)) {
    columns <- dots[smode == smodes]
    ## we need this check, because after subsetting there is only one xts object for each smode
    ## but cbind of list of one element is time-consuming operation
    if (length(columns) == 1)
      x[[smode]] <- columns[[1]]
    else
      x[[smode]] <- do.call(cbind, columns)
    if (recycle.columns)
      column.smodes <- c(column.smodes, rep(smode, ncol(x[[smode]])))
  }
  x$column.smodes <- column.smodes
  class(x) <- "xtsdfn"

  x
}

as.xtsdfn <- function(x, ...) UseMethod("as.xtsdfn")

as.xtsdfn.data.frame <- function(df, index = NULL) {
  if (is.null(index)) index <- rownames(df)
  column.smodes <- vector("numeric", ncol(df))

  df.column.smodes <- sapply(df, storage.mode)
  smodes <- unique(df.column.classes)

  x <- list()
  x$index <- index
  x$smodes <- smodes

  for (smode in smodes) {
    x[[smode]] <- as.xts(df[, df.column.smodes == smode])
    column.smodes[df.column.smodes == smode] <- smode
  }

  x$column.smodes <- column.smodes
  class(x) <- "xtsdfn"

  x
}

index.xtsdfn <- function(x) x$index

dim.xtsdfn <- function(x) c(length(index(x)), length(x$column.smodes))

## aux index maps each column index in xtsdfn object to indices in numeric and character parts
## aux index is only useful in conjuction with original index
get.aux.index <- function(x) {
  class.indexes <- list()
  for (smode in x$smodes)
    class.indexes[[smode]] <- 0

  index.aux <- vector("numeric", ncol(x))
  col.smodes <- x$column.smodes
  for (i in 1:ncol(x)) {
    class.indexes[[col.smodes[i]]] <- class.indexes[[col.smodes[i]]] + 1
    index.aux[i] <- class.indexes[[col.smodes[i]]]
  }
  index.aux
}


as.data.frame.xtsdfn <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (is.null(row.names))
    row.names <- index(x)
  index.aux <- get.aux.index(x)
  xts.list <- lapply(1:length(x$column.smodes), function(i) x[[x$column.smodes[i]]][, index.aux[i]])
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
    class.xts[[smode]] <- x[[smode]][i, index.aux[intersect(which(x$column.smodes == smode), j)]]
  do.call(xtsdfn, append(class.xts, list(index = x$index, column.smodes = x$column.smodes[j])))
}