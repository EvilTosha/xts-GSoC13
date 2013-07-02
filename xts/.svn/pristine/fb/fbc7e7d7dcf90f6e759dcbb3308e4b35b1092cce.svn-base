# as.** and as.xtime.** functions

# Date
as.Date.xtime <- function(x, ...) {
  as.Date(as.character(structure(unclass(x), class=c("POSIXct","POSIXt"))))
}

as.xtime.Date <- function(x, ...) {
  tzone <- attr(x, "tzone")
  if(is.null(tzone))
    tzone <- Sys.getenv("TZ")
  tm <- structure(unclass(as.POSIXct(as.character(x))), tzone=tzone, tclass="Date", class="xtime")
  tm
}

# POSIXt
as.POSIXct.xtime <-
function(x, ...) {
  structure(unclass(x), class=c("POSIXct","POSIXt"))
}
as.POSIXlt.xtime <-
function(x, tz="", ...) {
  as.POSIXlt(structure(unclass(x), class=c("POSIXt","POSIXct")), tz=tz)
}

as.xtime.POSIXt <-
function(x, ...) {
  tzone <- attr(x, "tzone")
  if(is.null(tzone))
    tzone <- Sys.getenv("TZ")
  if(inherits(x, "POSIXct"))
    tm <- structure(unclass(x), tzone=tzone, tclass="POSIXct", class="xtime")
  if(inherits(x, "POSIXlt"))
    tm <- structure(unclass(x), tzone=tzone, tclass="POSIXlt", class="xtime")
  tm
}

# chron dates times
as.chron.xtime <- function() {} #FIXME

as.xtime.dates <-
function(x, ...) {
    structure(as.numeric(as.POSIXct(strptime(as.character(x),"(%m/%d/%y %H:%M:%S)"))),
              tzone=Sys.getenv("TZ"), tclass=class(x), class="xtime")
}

