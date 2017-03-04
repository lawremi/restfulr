### =========================================================================
### Credentials objects
### -------------------------------------------------------------------------

setClass("Credentials",
         representation(username = "character_OR_NULL",
                        password = "character_OR_NULL"),
         validity = function(object) {
             c(if (!is.null(object@username) &&
                   !isSingleString(object@username))
                   "username must be NULL or a single string",
               if (!is.null(object@password) &&
                   !isSingleString(object@password))
                   "password must be NULL or a single string")
         })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("username", function(x) standardGeneric("username"))
setMethod("username", "Credentials", function(x) x@username)

setGeneric("password", function(x) standardGeneric("password"))
setMethod("password", "Credentials", function(x) x@password)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

Credentials <- function(username, password) {
    new("Credentials", username=username, password=password)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coerce
###

as.list.Credentials <- function(x) {
    as.list(x)
}

setMethod("as.list", "Credentials", function(x)
    list(username=username(x), password=password(x)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "Credentials", function(object) {
              cat("A", class(object), "object\n")
              cat("username:", username(object), "\n")
          })
