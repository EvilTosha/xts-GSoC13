Ops.xtime <-
function(e1, e2) {
  e <- NextMethod(.Generic)
  .Call("add_xtime_attributes", e, class(e1), tzone(e1), tclass(e1))
}

