### =========================================================================
### RestUri objects
### -------------------------------------------------------------------------
##
## A URI constructor with a $/[[-based syntax for path concatenation.
##

setClass("RestUri",
         representation(cache = "MediaCache",
                        protocol = "CRUDProtocol"),
         contains = "character")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

globalRestClientCache <- local({
  cache <- MediaCache()
  function() cache
})

RestUri <- function(base.uri, protocol = CRUDProtocol(base.uri, ...),
                    cache = globalRestClientCache(),
                    ...)
{
  if (!isSingleString(base.uri))
    stop("'base.uri' must be a single, non-NA string")
  if (!missing(protocol) && length(list(...)) > 0L)
    warning("arguments in '...' are ignored when 'protocol' is non-missing")
  base.uri <- sub("/$", "", base.uri)
  new("RestUri", base.uri, protocol = protocol, cache = cache)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### URI building methods
###

### Two choices for syntax:
### 1) Constructive: c(uri, "foo", "bar")
### 2) Accessive: uri$foo$bar
### The latter is preferable -- we are accessing a resource / api

setMethod("$", "RestUri", function(x, name) {
  x[[name]]
})

setMethod("[[", "RestUri", function(x, i, j, ...) {
  if (!missing(j) || length(list(...)) > 0L)
    warning("argument 'j' and arguments in '...' are ignored")
  tokens <- as.character(lapply(i, URLencode))
  initialize(x, paste0(x, "/", paste(tokens, collapse="/")))
})

setReplaceMethod("$", "RestUri", function(x, name, value) {
  x
})

setReplaceMethod("[[", "RestUri", function(x, i, j, ..., value) {
  x
})

query <- function(x, ...) {
  query.params <- c(...)
  query <- paste(sapply(names(query.params), URLencode, reserved=TRUE),
                 sapply(as.character(query.params), URLencode, reserved=TRUE),
                 sep = "=", collapse = "&")
  if (nchar(query) > 0L) {
    paste0(x, "?", query)
  } else {
    x
  }
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### RestContainer factory
###

setGeneric("container", function(x, ...) standardGeneric("container"))

setMethod("container", "RestUri", function(x) {
  new("RestContainer", uri=x)
})

setGeneric("container<-",
           function(x, ..., value) standardGeneric("container<-"))

setReplaceMethod("container", "RestUri", function(x, value) {
  x
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Transactions
###

setGeneric("create", function(x, ...) standardGeneric("create"))
setMethod("create", "RestUri", function(x, value, ...) {
  x@protocol$create(query(x, ...), as(value, "Media", strict=FALSE))
  invisible(x)
})

setGeneric("read", function(x, ...) standardGeneric("read"))
setMethod("read", "RestUri", function(x, ...) {
  uri <- query(x, ...)
  cached.media <- x@cache[[uri]]
  if (!is.null(cached.media) && !expired(cached.media))
    media <- cached.media
  else {
    if(isTRUE(getOption("verbose"))) {
      message("READ: ", URLdecode(uri))
    }
    result <- x@protocol$read(uri, cacheInfo(cached.media))
    if (is(result, "CacheInfo")) {
      cacheInfo(cached.media) <- result
      media <- cached.media
    } else {
      media <- result
    }
    x@cache[[uri]] <- media
  }
  as(media, mediaTarget(media))
})

setMethod("update", "RestUri", function(object, value, ...) {
  uri <- query(x, ...)
  x@protocol$update(uri, value = as(value, "Media"))
  invisible(x)
})

setGeneric("delete", function(x, ...) standardGeneric("delete"))
setMethod("delete", "RestUri", function(x, ...) {
  uri <- query(x, ...)
  x@protocol$delete(uri)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Caching
###

setGeneric("purgeCache", function(x, ...) standardGeneric("purgeCache"))

setMethod("purgeCache", "RestUri", function(x) {
  purge(x@cache)
  x
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "RestUri", function(object) {
  cat("RestUri:", as.character(object), "\n")
})
