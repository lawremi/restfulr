### =========================================================================
### Low-level utilities
### -------------------------------------------------------------------------

raggedListToDF <- function(x, ...) {
  nms <- unlist(lapply(x, names))
  uniq.nms <- unique(nms)
  ind <- match(nms, uniq.nms)
  as.data.frame(.Call(R_raggedListToDF, x, uniq.nms, ind), ...)
}

setMethod("unstrsplit", "AsIs", function(x, sep = "") {
              unstrsplit(unclass(x), sep=sep)
          })
