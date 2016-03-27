### =========================================================================
### Low-level utilities
### -------------------------------------------------------------------------

raggedListToDF <- function(x, keepAlwaysNULL = TRUE, ...) {
  nms <- unlist(lapply(x, names))
  uniq.nms <- unique(nms)
  ind <- match(nms, uniq.nms)
  cols <- .Call(R_raggedListToDF, x, uniq.nms, ind)
  nulls <- vapply(cols, is.null, logical(1L))
  if (keepAlwaysNULL) {
      cols[nulls] <- list(rep(NA, length(x)))
  } else {
      cols[nulls] <- NULL
  }
  as.data.frame(cols, ...)
}

setMethod("unstrsplit", "AsIs", function(x, sep = "") {
              unstrsplit(unclass(x), sep=sep)
          })
