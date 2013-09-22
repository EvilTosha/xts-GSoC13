## Implementation model:
## 1) An xtsdfn object is a list of xts objects with some auxiliary parameters.
## 2) There is one xts object for each class ("character", "double", ...).
## 3) Vector of all storage modes (smodes) is contained in parameter x$smodes
## 4) Parameter x$column.smodes contains vector of smodes, which length is sum of lengths of all xts objects.
##    i'th element in x$column.smodes represents storage mode of i'th column

## constructor only accepts xts objects as input
## this is the constructor for internal usage, not public
.xtsdfn <- function(..., column.smodes = NULL, column.classes = NULL, order.by = NULL, class.info = NULL){
  dots <- list(...)
  if (!all(sapply(dots, is.xts)))
    stop("All provided objects need to be an xts objects")

  if (is.null(order.by)) order.by <- index(..1)

  x <- list()
  x$index <- order.by

  if (is.null(column.classes)) {
    column.classes <- list()
    for (obj in dots) {
      ## TODO: use better way to determine a class of an xts object
      class <- class(coredata(obj[1, 1, drop = TRUE]))
      column.classes <- append(column.classes, rep(list(class), ncol(obj)))
    }
  }
  if (!is.list(column.classes)) column.classes <- as.list(column.classes)
  x$column.classes <- column.classes

  smodes <- unique(sapply(dots, storage.mode))
  x$smodes <- smodes

  recycle.column.smodes <- FALSE
  if (is.null(column.smodes)) {
    column.smodes <- c()
    recycle.column.smodes <- TRUE
  }

  for (smode in smodes) {
    columns <- dots[smode == smodes]
    ## we need this check, because after subsetting there is only one xts object for each smode
    ## but cbind of the list of one element is quite time-consuming operation
    if (length(columns) == 1)
      x[[smode]] <- columns[[1]]
    else
      x[[smode]] <- do.call(cbind, columns)
    if (recycle.column.smodes)
      column.smodes <- c(column.smodes, rep(smode, ncol(x[[smode]])))
  }
  x$column.smodes <- column.smodes
  x$class.info <- class.info
  class(x) <- "xtsdfn"

  ## make.unique.colnames(x)
  x
}

is.xtsdfn <- function(x) inherits(x, "xtsdfn")

## aux index maps each column index in xtsdfn object to indices in corresponding smode xts object
## i.e. if x$column.smodes = c("double", "character", "double", "logical", "character", "character"),
## get.aux.index = c(1, 1, 2, 1, 2, 3)
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

as.xtsdfn <- function(x, ...) UseMethod("as.xtsdfn")

as.xtsdfn.data.frame <- function(x, order.by = "rownames", ...) {
  if (!is.timeBased(order.by)) {
    if(order.by == "rownames")
      order.by <- rownames(x)
    order.by <- as.POSIXct(order.by, ...)
  }

  res <- list()
  res$index <- order.by
  res$column.smodes <- sapply(x, storage.mode)
  res$smodes <- unique(res$column.smodes)
  res$column.classes <- lapply(x, class)

  for (smode in res$smodes) {
    sub.df <- x[, res$column.smodes == smode, drop = FALSE]
    ## preprocessing for types such as POSIXct or factor
    sub.matrix <- do.call(cbind, lapply(sub.df,
                         function(col) {
                           if ("factor" %in% class(col))
                             as.integer(col)
                           else
                             as.vector(col)
                         }))
    res[[smode]] <- as.xts(sub.matrix, order.by = order.by)
  }

  res$class.info <- list()
  for (i in seq_len(ncol(x))) {
    if ("factor" %in% res$column.classes[[i]])
      res$class.info[[i]] <- levels(x[, i])
  }

  class(res) <- "xtsdfn"

  make.unique.colnames(res)
}

as.xtsdfn.matrix <- function(x, ...) {
  as.xtsdfn.data.frame(as.data.frame(x), ... = ...)
}

as.xtsdfn.xts <- function(x, ...) .xtsdfn(x, ...)

as.xtsdfn.xtsdfn <- function(x, ...) x

index.xtsdfn <- function(x, ...) x$index

dim.xtsdfn <- function(x) c(length(index(x)), length(x$column.smodes))

coredata.xtsdfn <- function(x, ...) as.data.frame(x)

lag.xtsdfn <- function(x, k = 1, na.pad = TRUE, ...) {
  ans <- x
  for (smode in x$smodes)
    ans[[smode]] <- lag(ans[[smode]], k, na.pad, ...)
  ans
}

## xts can't have same name for different columns, so xtsdfn should replicate this behavior
make.unique.colnames <- function(x) {
  colnames <- make.unique(colnames(x))
  for (smode in x$smodes)
    colnames(x[[smode]]) <- colnames[x$column.smodes == smode]
  x
}

dimnames.xtsdfn <- function(x) {
  if (length(x$smodes) == 0)
    NULL
  else {
    colnames <- vector("character", ncol(x))
    for (smode in x$smodes)
      colnames[x$column.smodes == smode] <- colnames(x[[smode]])
    list(rownames(x[[x$smodes[1]]]), colnames)
  }
}

head.xtsdfn <- function(x, n = 6, ...) {
  stopifnot(length(n) == 1L)
	xlen <- nrow(x)
  n <- if (n < 0L)
    max(xlen + n, 0L)
  else min(n, xlen)

	x[seq_len(n),, drop = FALSE]
}

tail.xtsdfn <- function(x, n = 6, ...) {
  stopifnot(length(n) == 1L)
	xlen <- nrow(x)
  n <- if (n < 0L)
    max(xlen + n, 0L)
  else min(n, xlen)

	x[seq.int(to = xlen, length.out = n),, drop = FALSE]
}


as.xts.xtsdfn <- function(x, ...) {
  if (length(x$column.smodes) == 0)
    xts(NULL)
  ## if there is only one smode available - simply return corresponding xts
  else if (length(x$smodes) == 1)
    x[[x$smodes[1]]]
  ## else, if there exist column of non-standard smode, convert all to character xts
  else {
    top.smode <- "logical"
    if (!all(x$smodes %in% c("logical", "integer", "double", "complex")))
      top.smode <- "character"
    else if (any(x$smodes %in% "complex"))
      top.smode <- "complex"
    else if (any(x$smodes %in% "double"))
      top.smode <- "double"
    else if (any(x$smodes %in% "integer"))
      top.smode <- "integer"

    index.aux <- get.aux.index(x)
    ## TODO: find a way without converting each column to character explicitly
    res <- x[[x$column.smodes[1]]][, index.aux[1]]
    storage.mode(res) <- top.smode
    for (i in 2:ncol(x)) {
      col <- x[[x$column.smodes[i]]][, index.aux[i]]
      storage.mode(col) <- top.smode
      res <- cbind(res, col)
    }
    res
  }
}

as.matrix.xtsdfn <- function(x, ...) {
  as.matrix(as.xts(x), ... = ...)
}

intersects <- function(a, b) length(intersect(a, b)) > 0

## TODO: first version, needs improvements
restore.column.class <- function(column, class, class.info = NULL) {
  if (intersects(class, c("numeric", "logical", "character", "integer")))
    column
  else {
    if (intersects(class, c("Date")))
      as.Date(column)
    else if (intersects(class, c("POSIXct")))
      ## is it the right way to restore POSIXct?
      .POSIXct(column)
    else if (intersects(class, c("factor")))
      ## TODO: this construction is ugly and slow: find a better way
      factor(as.numeric(column), labels = class.info[sort(unique(as.numeric(column)))])
  }
}

as.data.frame.xtsdfn <- function(x, row.names = NULL, optional = FALSE, stringsAsFactors = FALSE, ...) {
  if (is.null(row.names))
    row.names <- make.unique(as.character(index(x)))
  index.aux <- get.aux.index(x)
  df <- cbind.data.frame(lapply(seq(ncol(x)),
                                function(i)
                                data.frame(restore.column.class(x[[x$column.smodes[i]]][, index.aux[i], drop = TRUE],
                                                                x$column.classes[[i]],
                                                                x$class.info[[i]]),
                                           stringsAsFactors = stringsAsFactors)))
  colnames(df) <- colnames(x)
  rownames(df) <- row.names
  df
}


## returns list of xts objects: one for each column
as.list.xtsdfn <- function(x, ...) {
  res <- list()
  index.aux <- get.aux.index(x)
  for (i in seq(ncol(x))) {
    ## FIXME: find better way of appending xts objects
    res <- append(res, list(x[[x$column.smodes[i]]][, index.aux[i]]))
  }
  names(res) <- colnames(x)
  res
}

print.xtsdfn <- function(x, ...) {
  print(as.data.frame(x))
}
