### =========================================================================
### HTTP protocol implementation
### -------------------------------------------------------------------------

.HTTP <- setRefClass("HTTP",
                     fields = list(
                       accept = "character"
                       ),
                     contains = "CRUDProtocol")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

HTTP <- function(accept = acceptedMediaTypes()) {
  if (!is.character(accept) || any(is.na(accept)))
    stop("'accept' must be a character() without NAs")
  .HTTP$new(accept = accept)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### CRUD implementation
###

.HTTP$methods(create = function(x, value, ...) {
  if (!isSingleString(x))
    stop("'x' must be a single, non-NA string representing a URL")
  curl <- getCurlHandle()
  reader <- dynCurlReader(curl)
  opts <- curlOptions(postfields = paste(value, collapse="\n"),
                      httpheader = c(
                        Accept = accept(.self),
                        'Content-Type' = contentType(value),
                        Authorization = authorization(x),
                          ...),
                      headerfunction = reader$update)
  content <- try(postForm(x, .opts=opts, curl=curl), silent=TRUE)
  invisible(handleResponse(content, reader, errorHandler = x@errorHandler))
})

.HTTP$methods(read = function(x, cache.info, ...) {
  if (!isSingleString(x))
    stop("'x' must be a single, non-NA string representing a URL")
  request.header <- c(headerFromCacheInfo(cache.info),
                      Authorization = authorization(x),
                      Accept = accept(.self),
                      ...)
  ## We use our own reader so that we can return the body in case of error
  curl <- getCurlHandle(httpheader = request.header)
  reader <- dynCurlReader(curl)
  content <- try(getURLContent(x, header = reader, curl = curl), silent=TRUE)
  handleResponse(content, reader, cache.info, x@errorHandler)
})

.HTTP$methods(update = function(x, ..., value) {
  stop("PUT support not yet implemented")
})

.HTTP$methods(delete = function(x, ...) {
  stop("DELETE support not yet implemented")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Helpers
###

defaultErrorHandler <- function(response) {
  stop(response$statusMessage)
}

handleResponse <- function(content, reader, cache.info = NULL,
                           errorHandler = defaultErrorHandler) {
    response <- list(header = parseHTTPHeader(reader$header()),
                     body = reader$value())
    status <- as.integer(response$header["status"])
    statusMessage <- response$header["statusMessage"]
    if (identical(status, HTTP_STATUS$Unauthorized)) {
        unauthorized()
    }
    if (is(content, "try-error")) {
      media <- responseToMedia(response)
      body <- as(media, mediaTarget(media))
      responseError <- list(status = status, statusMessage = statusMessage,
                            body = body)
      errorHandler(responseError)
    }
    if (identical(status, HTTP_STATUS$No_Content)) {
        response <- NULL
    }
    if (!is.null(cache.info) && identical(status, HTTP_STATUS$Not_Modified)) {
        cacheInfoFromHeader(response$header, cache.info)
    } else {
        responseToMedia(response)
    }
}

coercionTable <- function() {
    signatures <- names(getMethods(coerce, table = TRUE))
    matrix(unlist(strsplit(signatures, "#")), ncol=2L, byrow=TRUE,
           dimnames=list(NULL, c("from", "to")))
}

mediaCoercionTable <- function() {
    tab <- coercionTable()
    classes <- names(getClass("Media")@subclasses)
    tab[rowSums(matrix(tab %in% classes, ncol=2L)) == 1L,]
}

acceptedMediaTypes <- function() {
    intersect(mediaCoercionTable()[,"from"],
              names(getClass("Media")@subclasses))
}

responseToMedia <- function(x) {
  content.type <- head(attr(x$body, "Content-Type"), 1)
  content.params <- tail(attr(x$body, "Content-Type"), -1)
  media.class <- mediaClassFromContentType(content.type)
  content.params <-
    content.params[intersect(names(content.params), slotNames(media.class))]
  if (media.class == "NullMedia") {
    new("NullMedia", cacheInfo = cacheInfoFromHeader(x$header))
  } else {
    do.call(new, c(media.class, x$body,
                   cacheInfo = cacheInfoFromHeader(x$header),
                   content.params))
  }
}

mediaClassFromContentType <- function(x) {
  if (is.null(x))
    "NullMedia"
  else if (is.character(x)) {
    if (isClass(x))
      x
    else sub("/.*", "/*", x)
  } else stop("content type should be character or NULL")
}

headerFromCacheInfo <- function(x) {
  if (is.null(x))
    NULL
  else c("If-None-Match" = x@hash,
         "If-Modified-Since" = formatHTTPDate(x@lastModified))
}

cacheInfoFromHeader <- function(x, original = CacheInfo()) {
  x <- as.list(x)
  cache.control <- parseCacheControl(x[["Cache-Control"]])
  if (isTRUE(cache.control[["no-cache"]]))
    expires <- Sys.time()
  else if (!is.null(cache.control[["max-age"]]))
    expires <- Sys.time() + cache.control[["max-age"]]
  else expires <- parseHTTPDate(x[["Expires"]])
  info.args <- list(expires = expires,
                    lastModified = parseHTTPDate(x[["Last-Modified"]]),
                    hash = x[["ETag"]])
  info.args <- Filter(Negate(is.null), info.args)
  do.call(initialize, c(original, info.args))
}

parseCacheControl <- function(x) {
  if (is.null(x))
    return(NULL)
  fields <- strsplit(x, ", ")[[1]]
  key.val <- strsplit(fields, "=")
  keys <- sapply(key.val, head, 1)
  has.val <- sapply(key.val, length) > 1L
  l <- list()
  l[keys[!has.val]] <- TRUE
  l[keys[has.val]] <- sapply(key.val[has.val], tail, 1)
  if (!is.null(l[["max-age"]]))
    l[["max-age"]] <- as.integer(l[["max-age"]])
  l
}

.httpParseDateString <- "%a, %d %b %Y %H:%M:%S"
.httpFormatDateString <- paste(.httpParseDateString, "%Z")

formatHTTPDate <- function(x) {
  format(x, .httpFormatDateString, tz = "GMT")
}

parseHTTPDate <- function(x) {
  if (is.null(x))
    NULL
  else strptime(x, .httpParseDateString, tz = "GMT")
}

authorization <- function(x) {
    credentials <- credentials(x)
    if (!is.null(credentials)) {
        auth <- paste0(username(credentials), ":", password(credentials))
        paste("Basic", base64(auth))
    }
}

accept <- function(x) {
  paste(x$accept, collapse=", ")
}

stopIfHTTPError <- function(header) {
  stop.if.HTTP.error <- get("stop.if.HTTP.error", getNamespace("RCurl"))
  stop.if.HTTP.error(header)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Status code constants
###

HTTP_STATUS <- setNames(as.list(as.integer(names(RCurl:::httpErrorClasses))),
                        RCurl:::httpErrorClasses)
