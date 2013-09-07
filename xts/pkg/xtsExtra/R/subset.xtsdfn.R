`[.xtsdfn` <- function(x, i, j, drop = FALSE, which.i = FALSE, ...) {
  ## smode.xts - a list, which contains for each storage mode a corresponding xts object
  smode.xts <- list()

  if (missing(j))      j <- seq_len(ncol(x))
  if (is.character(j)) j <- which(colnames(x) %in% j)
  if (is.logical(j))   j <- which(j)

  ## drop with single column case
  if (isTRUE(drop) && length(j) == 1) {
    smode <- x$column.smodes[j]
    smode.columns <- which(x$column.smodes == smode)
    sub.j <- which(smode.columns %in% j)
    if (missing(i))
      return(restore.column.class(x[[smode]][, sub.j, drop = TRUE], x$column.classes[[j]], x$class.info[[j]]))
    else
      return(restore.column.class(x[[smode]][i, sub.j, drop = TRUE], x$column.classes[[j]], x$class.info[[j]]))
  }

  ## indexes in all smode objects must be consistent, so we can afford to calculate result index like this
  index <- NULL
  first.smode <- first(x$smodes)
  if (!missing(i)) {
    i <- x[[first.smode]][i, , which.i = TRUE]
    index <- index(x[[first.smode]])[i]
  }
  else
    index <- index(x[[first.smode]])

  for (smode in x$smodes) {
    ## set of columns of current smode
    smode.columns <- which(x$column.smodes == smode)
    sub.j <- which(smode.columns %in% j)
    if (length(sub.j) > 0) {
      sub.xts <- x[[smode]][i, sub.j, drop = drop]
      smode.xts[[smode]] <- sub.xts

    }
  }
  ## remove empty parts if the whole object is not empty
  if (any(sapply(smode.xts, length)) > 0) {
    smode.xts <- smode.xts[sapply(smode.xts, length) > 0]
  }

  ## drop with single row case
  ## NOTE: case with single element is handled in the single column case
  if (isTRUE(drop) && length(i) == 1) {
    res <- list()
    index.aux <- get.aux.index(x)
    for (j.ind in seq_along(j)) {
      sub.j <- j[j.ind]
      res <- append(res, list(restore.column.class(smode.xts[[j.ind]], x$column.classes[[sub.j]], x$class.info[[sub.j]])))
    }
    names(res) <- colnames(x)[j]
    return(res)
  }

  do.call(xtsdfn, append(smode.xts, list(order.by = index,
                                         column.smodes = x$column.smodes[j],
                                         column.classes = x$column.classes[j],
                                         class.info = x$class.info[j])))
}

`[<-.xtsdfn` <- function(x, i, j, value) {
  if (missing(j))      j <- seq_len(ncol(x))
  if (is.character(j)) j <- which(colnames(x) %in% j)
  if (is.logical(j))   j <- which(j)

  if (missing(i)) i <- seq_len(nrow(x))

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
