### =========================================================================
### MediaCache objects
### -------------------------------------------------------------------------

### Just environments that only store Media objects

setClass("MediaCache", contains = "environment")

MediaCache <- function() {
  new("MediaCache", new.env(parent = emptyenv()))
}

setMethod("$<-", "MediaCache", function(x, name, value) {
  x[[name]] <- value
  x
})

setMethod("[[<-", "MediaCache", function(x, i, j, ..., value) {
  if (!missing(j))
    warning("argument 'j' is ignored")
  assign(i, value, x)
  x
})

setGeneric("assign",
           function (x, value, pos = -1, envir = as.environment(pos),
                     inherits = FALSE, immediate = TRUE)
           standardGeneric("assign"),
           signature = c("x", "value"))

setMethod("assign", "MediaCache",
          function (x, value, pos = -1, envir = as.environment(pos),
                    inherits = FALSE, immediate = TRUE)
          {
            if (!is(value, "Media"))
              stop("MediaCache only permits Media storage")
            callNextMethod()
          })

purge <- function(x) {
  rm(list=ls(x), envir=x)
  x
}
