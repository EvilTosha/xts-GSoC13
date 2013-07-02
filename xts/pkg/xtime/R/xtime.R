`[.xtime` <-
function(x, i, which.i=FALSE, ...) {
  if(missing(i)) i <- 1:length(x)
  if(is.numeric(i) && !is.xtime(i)) {
    structure(.subset(x,i), 
              tzone=tzone(x), tclass=tclass(x), class="xtime")
  } else {
    #structure(.subset(x, which(MATCH.xtime(x,i,0)>0)), 
    structure(.subset(x, .ISO8601subset(x,i)), 
              tzone=tzone(x), tclass=tclass(x), class="xtime")
  }
}

.xtime <- function(x) {
  unclass(x)
}

is.xtime <-
function(x) {
  inherits(x, "xtime")
}

un.xtime <-
function(x) { 
  tc <- tclass(x)
  if(is.null(tc))
    return(x)

  tzone <- attr(x, "tzone")
  if("Date" %in% tc)
    as.Date(as.character(structure(x, class=c("POSIXct","POSIXt"), tzone=tzone)))
  else
  if("POSIXct" %in% tc)
    structure(x, class=c("POSIXct","POSIXt"), tzone=tzone)
  else
  if("POSIXlt" %in% tc)
    as.POSIXlt(structure(x, class=c("POSIXct","POSIXt"), tzone=tzone))
  else
  if("yearmon" %in% tc)
    as.yearmon(structure(x, class=c("POSIXct","POSIXt"), tzone=tzone))
  else
  if("yearqtr" %in% tc)
    as.yearqtr(structure(x, class=c("POSIXct","POSIXt"), tzone=tzone))
  else
  if("chron" %in% tc) {
    as.chron(format(structure(x, class=c("POSIXct","POSIXt"), tzone="")))
  }
}

Sys.xtime <-
function(tzone=Sys.getenv("TZ")) {
  structure(Sys.time(), tzone=tzone, tclass=c("POSIXct","POSIXt"), class="xtime")
}

