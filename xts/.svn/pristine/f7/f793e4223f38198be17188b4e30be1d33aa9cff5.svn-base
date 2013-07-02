tclass <-
function(x) {
  UseMethod("tclass")
}

tclass.xtime <-
function(x) {
  attr(x, "tclass")
}

tclass.xts <-
function(x) {
  attr(attr(x,"index"), "tclass")
}

`tclass<-` <-
function(x, value) {
  UseMethod("tclass<-")
}

`tclass<-.xtime` <-
function(x, value) {
  attr(x, "tclass") <- value
  x
}

`tclass<-.xts` <-
function(x, value) {
  attr(attr(x,"index"), "tclass") <- value
  x
}

