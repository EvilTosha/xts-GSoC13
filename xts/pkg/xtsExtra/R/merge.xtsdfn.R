cbind.xtsdfn <- function(..., deparse.level = 1) {
  dots <- list(...)
  column.smodes <- do.call(append, lapply(dots, function(x) x$column.smodes))

  smode.xts <- list()
  for (obj in dots) {
    for (smode in obj$smodes) {
      if (is.null(smode.xts[[smode]]))
        smode.xts[[smode]] <- obj[[smode]]
      else
        smode.xts[[smode]] <- cbind(smode.xts[[smode]], obj[[smode]])
    }
  }

  do.call(xtsdfn, append(smode.xts, list(column.smodes = column.smodes)))
}


rbind.xtsdfn <- function(..., deparse.level = 1) {
  ## only works with perfectly matching (in rbind sense) objects
  dots <- list(...)

  if (length(dots) == 0)
    NULL
  else {
    smode.xts <- list()
    ref.obj <- dots[[1]]
    for (smode in ref.obj$smodes)
      smode.xts[[smode]] <- ref.obj[[smode]]
    for (obj in dots[-1]) {
      for (smode in ref.obj$smodes)
        smode.xts[[smode]] <- rbind(smode.xts[[smode]], obj[[smode]])
    }
    do.call(xtsdfn, append(smode.xts, list(column.smodes = ref.obj$column.smodes)))
  }
}

merge.xtsdfn <- function(..., all = TRUE, fill = NA) {
  dots <- list(...)

  ## check indexes
  index.list <- lapply(dots, index)
  ## 1. for non-unique entries
  index_duplicates <- function(x) length(unique(MATCH(x, x))) < length(x)
  if (any(sapply(index.list, index_duplicates)))
    stop("Series cannot be merged with non-unique index entries in a series")
  ## 2. for differing classes
  index.classes <- sapply(index.list, function(x) class(x)[1])
  if (!all(index.classes == index.classes[1]))
    stop("Only indexes with similar classes are allowed")

  index <- unique(sort(Reduce(append, index.list)))
  index.xts <- xts(order.by = index)

  column.smodes <- do.call(append, lapply(dots, function(x) x$column.smodes))
  smode.xts <- list()
  for (obj in dots) {
    for (smode in obj$smodes) {
      if (is.null(smode.xts[[smode]]))
        smode.xts[[smode]] <- merge(obj[[smode]], index.xts, all = all, fill = fill)
      else
        smode.xts[[smode]] <- merge(smode.xts[[smode]], obj[[smode]], all = all, fill = fill)
    }
  }

  do.call(xtsdfn, append(smode.xts, list(column.smodes = column.smodes)))
}
