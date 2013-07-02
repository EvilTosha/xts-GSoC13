timeBased <- function (x) 
{
    if (!any(sapply(c("Date", "POSIXct", "chron", "dates", "times", 
        "timeDate", "yearmon", "yearqtr", "xtime"), function(xx) inherits(x, 
        xx)))) {
        FALSE
    }
    else TRUE
}

