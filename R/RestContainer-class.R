### =========================================================================
### RestContainer objects
### -------------------------------------------------------------------------
###
### A means for accessing a URI using vector-like syntax.
###

setClass("RestContainer", representation(uri="RestUri"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

RestContainer <- function(...) {
    container(RestUri(...))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### CREATE
###

## How about: service$objects[] <- new.objects.

## The syntax service$objects[] means extract everything, because the
## default index is the wildcard. Intuitively, one would expect the
## same for []<-, i.e., replace-all; however, we could take the
## default to be the IDs of the elements being added. This is a
## primary difference from a DB API and R vectors: with databases, the
## IDs tend to be inherent in the objects.

setMethod("[<-", "RestContainer", function(x, i, j, ..., value) {
  if (!missing(j))
    warning("argument 'j' is ignored")
  if (missing(i)) {
    create(x@uri, value, ...)
  } else {
    value <- recycleArg(value, "value", length(i))
    for (ii in i)
      x[[ii, ...]] <- value[[ii]]
  }
  x
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### READ
###

setMethod("$", "RestContainer", function(x, name) {
  x[[name]]
})

setMethod("[[", "RestContainer", function(x, i, ...) {
  if (!is.character(i))
    stop("'i' must be a character vector")
  read(x@uri[[i]], ...)
})

setMethod("[", "RestContainer", function(x, i, j, ..., drop = TRUE) {
  if (!missing(j))
    warning("argument 'j' is ignored")
  if (!isTRUE(drop))
    warning("argument 'drop' must be TRUE")
  if (missing(i)) {
    read(x, ...)
  }
  else {
    if (!is.character(i))
      stop("'i' must be a character vector")
    sapply(i, function(ii, ...) x[[ii, ...]], ..., simplify=FALSE)
  }
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### UPDATE/DELETE
###

setMethod("$<-", "RestContainer", function(x, name, value) {
  x[[name]] <- value
  x
})

setMethod("[[<-", "RestContainer", function(x, i, j, ..., value) {
  if (missing(i))
    stop("argument 'i' cannot be missing")
  if (!missing(j))
    warning("argument 'j' is ignored")
  if (is.null(value))
    delete(x@uri[[i]], ...)
  else update(x@uri[[i]], ..., value=value)
  x
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "RestContainer", function(object) {
  cat("RestContainer object\n")
  cat("uri:", as.character(object@uri), "\n")
})
