cumsum.xtsdfn <- function(x) {
  smode.xts <- list()
  for (smode in x$smodes)
    smode.xts[[smode]] <- cumsum(x[[smode]])
  do.call(xtsdfn, append(smode.xts, list(order.by = x$index,
                                         column.smodes = x$column.smodes,
                                         column.classes = x$column.classes,
                                         class.info = x$class.info)))
}

cumprod.xtsdfn <- function(x) {
  smode.xts <- list()
  for (smode in x$smodes)
    smode.xts[[smode]] <- cumprod(x[[smode]])
  do.call(xtsdfn, append(smode.xts, list(order.by = x$index,
                                         column.smodes = x$column.smodes,
                                         column.classes = x$column.classes,
                                         class.info = x$class.info)))
}

cummin.xtsdfn <- function(x) {
  smode.xts <- list()
  for (smode in x$smodes)
    smode.xts[[smode]] <- cummin(x[[smode]])
  do.call(xtsdfn, append(smode.xts, list(order.by = x$index,
                                         column.smodes = x$column.smodes,
                                         column.classes = x$column.classes,
                                         class.info = x$class.info)))
}

cummax.xtsdfn <- function(x) {
  smode.xts <- list()
  for (smode in x$smodes)
    smode.xts[[smode]] <- cummax(x[[smode]])
  do.call(xtsdfn, append(smode.xts, list(order.by = x$index,
                                         column.smodes = x$column.smodes,
                                         column.classes = x$column.classes,
                                         class.info = x$class.info)))
}


mean.xtsdfn <- function(x, ...) {
  means <- NULL
  for (smode in x$smodes)
    means <- c(means, mean(x[[smode]], ... = ...))
  mean(means, ... = ...)
}
