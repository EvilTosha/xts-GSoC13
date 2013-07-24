## Implementation model:
## 1) An xtsdfn object is a list of xts object with some auxiliary parameters.
## 2) There is one xts object for any storage mode ("character", "double", ...).
## 3) Vector of all storage modes (smodes) is contained in parameter x$smodes
## 4) Parameter x$column.smodes contains vector of smodes, which length is sum of lengths of all xts objects.
##    i'th element in x$column.smodes represents storage mode of i'th column

## constructor only accepts xts objects as input
xtsdfn <- function(..., column.smodes = NULL, order.by = NULL){
  dots <- list(...)
  if (!all(sapply(dots, is.xts)))
    stop("All provided objects need to be a xts objects")

  if (is.null(order.by)) order.by <- index(..1)

  x <- list()
  x$index <- order.by

  if (!is.null(column.smodes)) {
    smodes <- unique(column.smodes)
    recycle.columns <- FALSE
  }
  else {
    smodes <- unique(sapply(dots, storage.mode))
    recycle.columns <- TRUE
  }
  x$smodes <- smodes

  for (smode in smodes) {
    columns <- dots[smode == smodes]
    ## we need this check, because after subsetting there is only one xts object for each smode
    ## but cbind of the list of one element is quite time-consuming operation
    if (length(columns) == 1)
      x[[smode]] <- columns[[1]]
    else
      x[[smode]] <- do.call(cbind, columns)
    if (recycle.columns)
      column.smodes <- c(column.smodes, rep(smode, ncol(x[[smode]])))
  }
  x$column.smodes <- column.smodes
  class(x) <- "xtsdfn"

  x <- make.unique.colnames(x)

  x
}

is.xtsdfn <- function(x) inherits(x, "xtsdfn")

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

as.xtsdfn <- function(x, ...) UseMethod("as.xtsdfn")

as.xtsdfn.data.frame <- function(df, order.by = "rownames", ...) {
  if (!is.timeBased(order.by)) {
    if(order.by == "rownames")
      order.by <- rownames(df)
    order.by <- as.POSIXct(order.by, ...)
  }
  column.smodes <- vector("numeric", ncol(df))

  df.column.smodes <- sapply(df, storage.mode)
  smodes <- unique(df.column.smodes)

  x <- list()
  x$index <- order.by
  x$smodes <- smodes

  for (smode in smodes) {
    x[[smode]] <- as.xts(df[, df.column.smodes == smode, drop = FALSE], order.by = order.by)
    column.smodes[df.column.smodes == smode] <- smode
  }

  x$column.smodes <- column.smodes
  class(x) <- "xtsdfn"

  x
}

as.xtsdfn.xts <- function(x, ...) xtsdfn(x, ...)

index.xtsdfn <- function(x) x$index

dim.xtsdfn <- function(x) c(length(index(x)), length(x$column.smodes))

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
    index.aux <- get.aux.index(x)
    colnames <- vector("character", ncol(x))
    for (smode in x$smodes)
      colnames[x$column.smodes == smode] <- colnames(x[[smode]])
    list(rownames(x[[x$smodes[1]]]), colnames)
  }
}


as.xts.xtsdfn <- function(x) {
  if (length(x$column.smodes) == 0)
    xts(NULL)
  ## if there is only one smode available - simply return corresponding xts
  else if (length(x$smodes) == 1)
    x[[x$smodes[1]]]
  ## else convert all to character xts
  else {
    index.aux <- get.aux.index(x)
    ## TODO: find a way without converting each column to character explicitly
    res <- x[[x$column.smodes[1]]][, index.aux[1]]
    storage.mode(res) <- "character"
    for (i in 2:ncol(x)) {
      col <- x[[x$column.smodes[i]]][, index.aux[i]]
      storage.mode(col) <- "character"
      res <- cbind(res, col)
    }
    res
  }
}

as.data.frame.xtsdfn <- function(x, row.names = NULL, ...) {
  if (is.null(row.names))
    row.names <- index(x)
  index.aux <- get.aux.index(x)
  xts.list <- lapply(1:length(x$column.smodes), function(i) x[[x$column.smodes[i]]][, index.aux[i]])
  do.call(data.frame, append(xts.list, list(row.names = row.names, ...)))
}

print.xtsdfn <- function(x, ...) {
  print(as.data.frame(x))
}

`[.xtsdfn` <- function(x, i, j, drop = FALSE, which.i = FALSE, ...) {
  ## smode.xts - a list, which contains for each storage mode a corresponding xts object
  smode.xts <- list()
  if (missing(j)) j      <- 1:ncol(x)
  if (is.character(j)) j <- which(colnames(x) %in% j)
  if (is.logical(j)) j   <- which(j)
  for (smode in x$smodes) {
    ## set of columns of current smode
    smode.columns <- which(x$column.smodes == smode)
    smode.xts <- x[[smode]][i, which(smode.columns %in% j)]
    if (length(smode.xts) > 0)
      smode.xts[[smode]] <- smode.xts
    if (!missing(i))
      index <- index(x[[smode]])[x[[smode]][i, , which.i = TRUE]]
    else
      index <- index(x[[smode]])
  }
  do.call(xtsdfn, append(smode.xts, list(order.by = index, column.smodes = x$column.smodes[j])))
}

`[<-.xtsdfn` <- function(x, i, j, value) {
  if (missing(j)) j      <- 1:ncol(x)
  if (is.character(j)) j <- which(colnames(x) %in% j)
  if (is.logical(j)) j   <- which(j)

  ## TODO: this won't work with vector to column assignment
  if (is.atomic(value) || is.vector(value) || is.list(value))
    value <- as.data.frame(value, stringsAsFactors = FALSE)

  for (smode in x$smodes) {
    smode.columns <- which(x$column.smodes == smode)
    smode.j <- which(smode.columns %in% j)
    value.j <- which(j %in% smode.columns)
    if (length(smode.j) > 0)
      x[[smode]][i, smode.j] <- value[, value.j]
  }
  x
}

cbind.xtsdfn <- function(..., deparse.level = 1) {
  dots <- list(...)
  column.smodes <- do.call(append, lapply(dots, function(x) x$column.smodes))

  class.xts <- list()
  for (obj in dots) {
    for (smode in obj$smodes) {
      if (is.null(class.xts[[smode]]))
        class.xts[[smode]] <- obj[[smode]]
      else
        class.xts[[smode]] <- cbind(class.xts[[smode]], obj[[smode]])
    }
  }

  do.call(xtsdfn, append(class.xts, list(column.smodes = column.smodes)))
}


rbind.xtsdfn <- function(..., deparse.level = 1) {
  ## only works with perfectly matching (in rbind sense) objects
  dots <- list(...)

  if (length(dots) == 0)
    NULL
  else {
    class.xts <- list()
    ref.obj <- dots[[1]]
    for (smode in ref.obj$smodes)
      class.xts[[smode]] <- ref.obj[[smode]]
    for (obj in dots[-1]) {
      for (smode in ref.obj$smodes)
        class.xts[[smode]] <- rbind(class.xts[[smode]], obj[[smode]])
    }
    do.call(xtsdfn, append(class.xts, list(column.smodes = ref.obj$column.smodes)))
  }
}
