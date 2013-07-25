cumsum.xtsdfn <- function(x) {
  smode.xts <- list()
  for (smode in x$smodes)
    smode.xts[[smode]] <- cumsum(x[[smode]])
  do.call(xtsdfn, append(smode.xts, list(order.by = x$index, column.smodes = x$column.smodes)))
}

cumprod.xtsdfn <- function(x) {
  smode.xts <- list()
  for (smode in x$smodes)
    smode.xts[[smode]] <- cumprod(x[[smode]])
  do.call(xtsdfn, append(smode.xts, list(order.by = x$index, column.smodes = x$column.smodes)))
}

cummin.xtsdfn <- function(x) {
  smode.xts <- list()
  for (smode in x$smodes)
    smode.xts[[smode]] <- cummin(x[[smode]])
  do.call(xtsdfn, append(smode.xts, list(order.by = x$index, column.smodes = x$column.smodes)))
}

cummax.xtsdfn <- function(x) {
  smode.xts <- list()
  for (smode in x$smodes)
    smode.xts[[smode]] <- cummax(x[[smode]])
  do.call(xtsdfn, append(smode.xts, list(order.by = x$index, column.smodes = x$column.smodes)))
}
