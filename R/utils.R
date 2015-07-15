### =========================================================================
### Low-level utilities
### -------------------------------------------------------------------------

isSingleString <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

isTRUEorFALSE <- function(x) {
  identical(x, TRUE) || identical(x, FALSE)
}

recycleVector <- function(x, length.out)
{
  if (length(x) == length.out) {
    x
  } else {
    ans <- vector(storage.mode(x), length.out)
    ans[] <- x
    ans
  }
}

raggedListToDF <- function(x, ...) {
  nms <- unlist(lapply(x, names))
  uniq.nms <- unique(nms)
  ind <- match(nms, uniq.nms)
  as.data.frame(.Call(R_raggedListToDF, x, uniq.nms, ind), ...)
}

unstrsplit <- function(x, sep = "") {
    if (!isSingleString(sep)) {
        stop("'sep' must be a single, non-NA string")
    }
    .Call(R_unstrsplit_list, as.list(x), sep)
}
