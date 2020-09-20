### =========================================================================
### RestUri objects
### -------------------------------------------------------------------------
##
## A URI constructor with a $/[[-based syntax for path concatenation.
##
## Could be called more specifically a URL, since it can get the data.
##

setClassUnion("CredentialsORNULL", c("Credentials", "NULL"))

setClass("RestUri",
         representation(cache = "MediaCache",
                        protocol = "CRUDProtocol",
                        credentials = "CredentialsORNULL",
                        errorHandler = "function"),
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
                    errorHandler = defaultErrorHandler,
                    ...)
{
  if (!isSingleString(base.uri))
    stop("'base.uri' must be a single, non-NA string")
  if (!missing(protocol) && length(list(...)) > 0L)
    warning("arguments in '...' are ignored when 'protocol' is non-missing")
  base.uri <- sub("/$", "", base.uri)
  credentials <- findCredentials(base.uri)
  new("RestUri", base.uri, protocol = protocol, cache = cache,
      credentials = credentials, errorHandler = errorHandler)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("credentials", function(x, ...) standardGeneric("credentials"))

setMethod("credentials", "RestUri", function(x) x@credentials)

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

setMethod("[", "RestUri", function(x, i, j, ...) {
    params <- c(i=if (!missing(i)) i,
                j=if (!missing(j)) j,
                list(...))
    query(x, params)
})

query <- function(x, ...) {
  query.params <- c(...)
  query <- paste(sapply(names(query.params), URLencode, reserved=TRUE),
                 sapply(as.character(query.params), URLencode, reserved=TRUE),
                 sep = "=", collapse = "&")
  if (nchar(query) > 0L) {
      uri <- if (grepl("?", x, fixed=TRUE))
                 paste0(x, "&", query)
             else paste0(x, "?", query)
      initialize(x, uri)
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

write <- function(FUN, url, value, ..., returnResponse=FALSE) {
    stopifnot(isTRUEorFALSE(returnResponse))
    url <- query(url, ...)
    media <- as(value, "Media", strict=FALSE)
    media <- tryCatch(FUN(url, media),
                      unauthorized = function(cond) {
                          url <- authenticate(url)
                          FUN(url, media)
                      })
    if (returnResponse)
        as(media, mediaTarget(media))
    else invisible(url)    
}

setGeneric("create", function(x, ...) standardGeneric("create"))
setMethod("create", "RestUri", function(x, value, ..., returnResponse=FALSE) {
    write(x@protocol$create, x, value, ...,
          returnResponse=returnResponse)
})
setMethod("create", "character", function(x, ...) {
              create(RestUri(x), ...)
          })

setGeneric("read", function(x, ...) standardGeneric("read"))
setMethod("read", "RestUri", function(x, ...) {
  x <- query(x, ...)
  cached.media <- x@cache[[x]]
  if (!is.null(cached.media) && !expired(cached.media))
    media <- cached.media
  else {
    if(isTRUE(getOption("verbose"))) {
      message("READ: ", URLdecode(x))
    }
    cacheInfo <- cacheInfo(cached.media)
    result <- tryCatch(x@protocol$read(x, cacheInfo),
                       unauthorized = function(cond) {
                           x <- authenticate(x)
                           x@protocol$read(x, cacheInfo)
                       })
    if (is(result, "CacheInfo")) {
      cacheInfo(cached.media) <- result
      media <- cached.media
    } else {
      media <- result
    }
    x@cache[[x]] <- media
  }
  as(media, mediaTarget(media))
})
setMethod("read", "character", function(x, ...) {
              read(RestUri(x), ...)
          })

setMethod("update", "RestUri", function(object, value, ...) {
              write(object@protocol$update, object, value, ...)
          })
setMethod("update", "character", function(object, value, ...) {
              update(RestUri(object), value, ...)
          })

setGeneric("delete", function(x, ...) standardGeneric("delete"))
setMethod("delete", "RestUri", function(x, ...) {
              x <- query(x, ...)
              invisible(tryCatch(x@protocol$delete(x),
                                 unauthorized = function(cond) {
                                     x <- authenticate(x)
                                     x@protocol$delete(x)
                                 }))
          })
setMethod("delete", "character", function(x, ...) {
              delete(RestUri(x), ...)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Authentication
###

configPath <- function(...) {
    base <- path.expand(file.path(Sys.getenv("XDG_CONFIG_HOME",
                                             "~/.local/config"),
                                  "restfulr"))
    file.path(base, ...)
}

loadCredentials <- function() {
    path <- configPath("credentials.yaml")
    if (file.exists(path)) {
        yaml.load_file(path)
    } else {
        list()
    }
}

findCredentials <- function(uri) {
    config <- loadCredentials()
    hits <- which(startsWith(uri, as.character(names(config))))
    if (length(hits) > 0L) {
        userpwd <- config[[hits[1L]]]
        Credentials(userpwd$username, userpwd$password)
    }
}

saveCredentials <- function(x, uri) {
    config <- loadCredentials()
    config[[uri]] <- as.list(x)
    path <- configPath("credentials.yaml")
    if (!file.exists(dirname(path))) {
        dir.create(dirname(path), recursive=TRUE)
    }
    writeLines(as.yaml(config), path)
}

hackyGetPass <- function() {
    inTerminal <- .Platform$GUI == "X11" && !identical(Sys.getenv("EMACS"), "t")
    if (inTerminal) {
        system("stty -echo")
        on.exit(system("stty echo"))
    } else {
        cat("THIS WILL SHOW YOUR PASSWORD\n")
    }
    readline("password: ")
}

promptForCredentials <- function() {
    defaultUser <- Sys.info()["effective_user"]
    username <- readline(paste0("username (default: ",
                                defaultUser, "): "))
    if (username == "")
        username <- defaultUser
    if (requireNamespace("getPass")) {
        password <- getPass::getPass()
    } else {
        password <- hackyGetPass()
    }
    if (!is.null(password) && password != "")
        Credentials(username, password)
}

unauthorized <- function() {
    error <- simpleError("unauthorized")
    class(error) <- c("unauthorized", class(error))
    stop(error)
}

setGeneric("authenticate", function(x, ...) standardGeneric("authenticate"))

setMethod("authenticate", "RestUri", function(x) {
              credentials <- findCredentials(x)
              if (is.null(credentials)) {
                  credentials <- promptForCredentials()
                  if (is.null(credentials)) {
                      stop("authentication failed: see ?credentials")
                  }
                  if (getOption("restfulr.store.credentials", TRUE)) {
                      saveCredentials(credentials, x)
                  }
              }
              initialize(x, credentials=credentials)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Utilities
###

setGeneric("purgeCache", function(x, ...) standardGeneric("purgeCache"))

setMethod("purgeCache", "RestUri", function(x) {
  purge(x@cache)
  x
})

embedCredentials <- function(x) {
    creds <- credentials(x)
    if (is.null(creds)) {
        return(x)
    }
    auth <- paste0(URLencode(username(creds), reserved=TRUE), ":",
                   URLencode(password(creds), reserved=TRUE), "@")
    url <- sub("://", paste0("://", auth), x, fixed=TRUE)
    initialize(x, url, credentials=NULL)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "RestUri", function(object) {
  cat("RestUri:", as.character(object), "\n")
})
