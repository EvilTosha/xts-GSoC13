#
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

endpoints <- function(x, on="months", k=1) {
  UseMethod("endpoints")
}

#  endpoints.xts <-

endpoints.default <- function(x,on='months',k=1) {
  # this is only applicable to timeBased classes, and is now
  # a generic function. The xts method will be available in
  # the xts package

  if(!timeBased(x))
    stop(paste("no 'endpoints' method for class",class(x)))

  if(!is.xtime(x)) {
    x <- as.xtime(x)
  }

  NR <- length(x)

  # posixltindex is costly in memory (9x length of time)
  # make sure we really need it
  if(on %in% c('years','quarters','months','weeks','days'))
    #posixltindex <- as.POSIXlt(structure( .index(x), class=c("POSIXt","POSIXct")))
    posixltindex <- as.POSIXlt(x)

  if(on == 'years') {
    #as.integer(c(0, which(diff(as.POSIXlt(index(x))$year %/% k + 1) != 0), NR) )
    as.integer(c(0, which(diff(posixltindex$year %/% k + 1) != 0), NR))
  } else

  if(on == 'quarters') {
    xi <- (posixltindex$mon%/%3) + 1
    as.integer(c(0,which(diff(xi) != 0),NR))
  } else 

  if(on == 'months') {
    #as.integer(c(0, which(diff(posixltindex$mon %/% k + 1) != 0), NR) )
    .Call("endpoints", posixltindex$mon, 1L, k)
  } else 
  if(on == 'weeks') {
    #as.integer(c(0, which(diff( (.index(x) + (3L * 86400L)) %/% 604800L %/% k + 1) != 0), NR) )
    .Call("endpoints", .xtime(x)+3L*86400L, 604800L, k)
  } else
  if(on == 'days') {
    #as.integer(c(0, which(diff(.xtime(x) %/% 86400L %/% k + 1) != 0), NR))
    #as.integer(c(0, which(diff(posixl.xtime$yday %/% k + 1) != 0), NR))
    .Call("endpoints", posixl.xtime$yday, 1L, k)
  } else
  # non-date slicing should be indifferent to TZ and DST, so use math instead
  if(on == 'hours') {
    #c(0, which(diff(as.POSIXlt.xtime(x))$hour %/% k + 1) != 0), NR) 
    #as.integer(c(0, which(diff(.xtime(x) %/% 3600L %/% k + 1) != 0), NR))
    #as.integer(c(0, which(diff(posixl.xtime$hour %/% k + 1) != 0), NR))
    .Call("endpoints", x, 3600L, k)
  } else
  if(on == 'minutes' || on == 'mins') {
    #c(0, which(diff(as.POSIXlt.xtime(x))$min %/% k + 1) != 0), NR) 
    #as.integer(c(0, which(diff(.xtime(x) %/% 60L %/% k + 1) != 0), NR))
    #as.integer(c(0, which(diff(posixl.xtime$min %/% k + 1) != 0), NR))
    .Call("endpoints", x, 60L, k)
  } else
  if(on == 'seconds' || on == 'secs') {
    #c(0, which(diff(as.POSIXlt.xtime(x))$sec %/% k + 1) != 0), NR) 
    #as.integer(c(0, which(diff(.xtime(x) %/%  k + 1) != 0), NR))
    .Call("endpoints", x, 1L, k)
  } else
  if(on == 'milliseconds' || on == 'ms') {
    #as.integer(c(0, which(diff(.xtime(x)%/%.001%/%k + 1) != 0), NR))
    .Call("endpoints", .xtime(x)%/%.001, 1L, k)
  } else
  if(on == 'microseconds' || on == 'us') {
    #as.integer(c(0, which(diff(.xtime(x)%/%.000001%/%k + 1) != 0), NR))
    .Call("endpoints", .xtime(x)%/%.000001, 1L, k)
  } else {
    stop('unsupported "on" argument')
  }
}

`startof` <-
function(x,by='months', k=1) {
  ep <- endpoints(x,on=by, k=k)
  (ep+1)[-length(ep)]
}

`endof` <-
function(x,by='months', k=1) {
  endpoints(x,on=by, k=k)[-1]
}

`firstof` <-
function(year=1970,month=1,day=1,hour=0,min=0,sec=0,tz="") {
  ISOdatetime(year,month,day,hour,min,sec,tz)
}

lastof <-
function (year = 1970,
          month = 12,
          day = 31,
          hour = 23, 
          min = 59,
          sec = 59,
          subsec=.99999, tz = "") 
{
    if(!missing(sec) && sec %% 1 != 0)
      subsec <- 0
    sec <- ifelse(year < 1970, sec, sec+subsec) # <1970 asPOSIXct bug workaround
    #sec <- sec + subsec
    mon.lengths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 
        30, 31)
    if (missing(day)) {
        day <- ifelse(month %in% 2, ifelse(((year%%4 %in% 0 & 
            !year%%100 %in% 0) | (year%%400 %in% 0)), 29, 28), 
            mon.lengths[month])
    }
    if (length(c(year, month, day, hour, min, sec)) == 6 && c(year, 
        month, day, hour, min, sec) == c(1969, 12, 31, 23, 59, 
        59) && Sys.getenv("TZ") %in% c("", "GMT", "UTC")) 
        sec <- sec-1
    ISOdatetime(year, month, day, hour, min, sec, tz)
}
