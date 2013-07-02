as.character.xtime <-
function(x) {
  ux <- un.xtime(x)
  if(is.xtime(ux))
    as.character(as.POSIXct(ux))
  else
  as.character(ux)
}

format.xtime <- function(x, format="", tz="", ...) {
  ux <- un.xtime(x)
  if(is.xtime(ux))
    format(as.POSIXct(ux))
  else
  format(ux)
}

