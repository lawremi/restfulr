### =========================================================================
### CRUDProtocol objects
### -------------------------------------------------------------------------

### Implements a specific REST protocol (like HTTP).

.CRUDProtocol <- setRefClass("CRUDProtocol")

.CRUDProtocol$methods(create = function(x, ..., value) {
  unimplemented("create")
})

unimplemented <- function(x) {
  stop("This protocol does not implement the '", x, "' operation")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### FIXME: may be incomplete
availableCRUDProtocols <- function() {
  names(getClass("CRUDProtocol")@subclasses)
}

CRUDProtocol <- function(uri, ...) {
  if (!isSingleString(uri)) {
    stop("'uri' must be a single, non-NA string")
  }
  protocol <- toupper(parseURI(uri)$scheme)
  if (!protocol %in% availableCRUDProtocols()) {
    stop("Unsupported CRUD protocol: ", protocol)
  }
  match.fun(protocol)(...)
}
