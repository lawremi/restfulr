### =========================================================================
### CacheInfo class
### -------------------------------------------------------------------------

setClass("CacheInfo",
         representation(expires = "POSIXt",
                        lastModified = "POSIXt",
                        hash = "characterORNULL"))

CacheInfo <- function(expires = Sys.time(),
                      lastModified = as.POSIXct("1960-01-01"),
                      hash = NULL)
{
  if (!is.null(hash) && !isSingleString(hash))
    stop("If not NULL, 'hash' must be a single, non-NA string")
  new("CacheInfo", expires = expires, lastModified = lastModified, hash = hash)
}

setGeneric("expired", function(x, ...) standardGeneric("expired"))

setMethod("expired", "CacheInfo", function(x) {
  x@expires < Sys.time()
})

setGeneric("modifiedSince",
           function(x, date, ...) standardGeneric("modifiedSince"))
  
setMethod("modifiedSince", c("CacheInfo", "Date"), function(x, date) {
  x@lastModified >= date
})


