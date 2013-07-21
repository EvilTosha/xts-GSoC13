cumsum.xtsdfn <- function(x) {
  class.xts <- list()
  for (smode in x$smodes) {
    class.xts[[smode]] <- cumsum(x[[smode]])
  }
  do.call(xtsdfn, append(class.xts, list(index = x$index, column.smodes = x$column.smodes)))
}

cumprod.xtsdfn <- function(x) {
  class.xts <- list()
  for (smode in x$smodes) {
    class.xts[[smode]] <- cumprod(x[[smode]])
  }
  do.call(xtsdfn, append(class.xts, list(index = x$index, column.smodes = x$column.smodes)))
}

cummin.xtsdfn <- function(x) {
  class.xts <- list()
  for (smode in x$smodes) {
    class.xts[[smode]] <- cummin(x[[smode]])
  }
  do.call(xtsdfn, append(class.xts, list(index = x$index, column.smodes = x$column.smodes)))
}

cummax.xtsdfn <- function(x) {
  class.xts <- list()
  for (smode in x$smodes) {
    class.xts[[smode]] <- cummax(x[[smode]])
  }
  do.call(xtsdfn, append(class.xts, list(index = x$index, column.smodes = x$column.smodes)))
}
