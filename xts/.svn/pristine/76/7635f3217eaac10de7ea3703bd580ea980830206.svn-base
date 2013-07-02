tzone <-
function(x) {
  UseMethod("tzone")
}

tzone.xtime <-
function(x) {
  attr(x, "tzone")
}

tzone.xts <-
function(x) {
  attr(.index(x), "tzone")
}

`tzone<-` <-
function(x,value) {
  UseMethod("tzone<-")
}

`tzone<-.xtime` <-
function(x,value) {
  attr(x, "tzone") <- structure(value, .Names="TZ")
  x
}

`tzone<-.xts` <-
function(x,value) {
  attr(.index(x), "tzone") <- structure(value, .Names="TZ")
  x
}

