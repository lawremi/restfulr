### =========================================================================
### Formal declaration of media types
### -------------------------------------------------------------------------

setClass("Media", representation(cacheInfo = "CacheInfo"))

setClass("application/*", contains = "Media")
setClass("audio/*", contains = "Media")
setClass("image/*", contains = "Media")
setClass("message/*", contains = "Media")
setClass("model/*", contains = "Media")
setClass("multipart/*",
         representation(boundary = "character"),
         contains = "Media")
setClass("text/*",
         representation(charset = "character"),
         prototype(charset = "us-ascii"),
         contains = c("character", "Media"))
setClass("video/*", contains = "Media")

setClass("text/plain", contains = "text/*")
setClass("text/html", contains = "text/*")
setClass("text/csv", contains = "text/*")
setClass("application/xml",
         representation(charset = "character"),
         prototype(charset = "us-ascii"),
         contains = c("character", "application/*"))
setClass("application/xhtml+xml", contains="application/xml")
setClass("application/json",
         contains = c("character", "application/*"))
##setClass("application/R", contains = "application/*")

setClass("application/x-www-form-urlencoded",
         contains = c("character", "application/*"))

setClass("NullMedia", contains = "Media")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("cacheInfo", function(x, ...) standardGeneric("cacheInfo"))
setMethod("cacheInfo", "Media", function(x) x@cacheInfo)
setMethod("cacheInfo", "NULL", function(x) NULL)

setGeneric("cacheInfo<-", function(x, ..., value) standardGeneric("cacheInfo<-"))
setReplaceMethod("cacheInfo", c("Media", "CacheInfo"), function(x, value) {
  x@cacheInfo <- value
  x
})

setMethod("expired", "Media", function(x) expired(cacheInfo(x)))

setMethod("length", "NullMedia", function(x) 0L)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## Default R <-> media type mapping
setGeneric("mediaTarget", function(x, ...) standardGeneric("mediaTarget"))

setMethod("mediaTarget", "application/json", function(x) "list")
setMethod("mediaTarget", "application/xml", function(x) "XMLAbstractNode")
setMethod("mediaTarget", "text/html", function(x) "XMLAbstractNode")
setMethod("mediaTarget", "text/csv", function(x) "data.frame")
setMethod("mediaTarget", "text/*", function(x) "character")
setMethod("mediaTarget", "NullMedia", function(x) "NULL")

setAs("application/xml", "XMLAbstractNode", function(from) {
  xmlInternalTreeParse(from, asText=TRUE)
})

setAs("text/html", "XMLAbstractNode", function(from) {
  htmlTreeParse(from, asText=TRUE, useInternalNodes=TRUE)
})

setAs("application/json", "list", function(from) {
  fromJSON(from)
})

`as.data.frame.application/json` <- function(x, row.names = NULL,
                                             optional = FALSE, ...)
{
    df <- raggedListToDF(as.list(x), optional=optional, ...)
    if (!is.null(row.names))
        rownames(df) <- row.names
    df
}

`as.data.frame.text/csv` <- function(x, row.names = NULL, optional = FALSE, ...)
{
    chr <- as.character(x)
    if (identical(chr, "") || identical(chr, "\n")) {
        return(data.frame())
    }
    con <- file()
    on.exit(close(con))
    writeLines(chr, con)
### FIXME: we are assuming a header, but there is no guarantee
    df <- read.csv(con, check.names=!optional, stringsAsFactors=FALSE, ...)
    if (!is.null(row.names))
        rownames(df) <- row.names
    df
}

setAs("Media", "data.frame", function(from) {
          as.data.frame(from, optional=TRUE)
      })

setAs("ANY", "Media", function(from) {
  as(from, "application/json")
})

setAs("list", "application/json", function(from) {
  new("application/json", toJSON(from))
})

setAs("data.frame", "Media", function(from) {
  as(from, "text/csv")
})

setAs("data.frame", "text/csv", function(from) {
  con <- file()
  on.exit(close(con))
  df <- as(from, "data.frame")
  df[] <- lapply(df, function(x) {
                     if (is.list(x)) {
                         unstrsplit(x, ",")
                     } else {
                         x
                     }
                 })
  write.csv(df, con, row.names=FALSE)
  new("text/csv", paste(readLines(con), collapse="\n"))
})

as.list.Media <- function(x) as(x, "list")

contentType <- function(x) {
  slots <- setdiff(slotNames(class(x)), c(".Data", "cacheInfo"))
  params <- vapply(slots, function(nm) as.character(slot(x, nm)), character(1L))
  paste(c(class(x), paste(names(params), params, sep="=")), collapse=";")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "Media", function(object) {
  cat("Media of type '", class(object), "'\n", sep = "")
  cat("length: ", length(object), "\n", sep = "")
})
