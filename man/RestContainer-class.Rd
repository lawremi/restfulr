\name{RestContainer-class}
\docType{class}

\alias{class:RestContainer}
\alias{RestContainer-class}

% Constructor:
\alias{RestContainer}

% CRUD:
\alias{[,RestContainer-method}
\alias{[<-,RestContainer-method}
\alias{$,RestContainer-method}
\alias{[[,RestContainer-method}
\alias{$<-,RestContainer-method}
\alias{[[<-,RestContainer-method}

% Show:
\alias{show,RestContainer-method}

\title{RestContainer}
\description{
  The \code{RestContainer} object wraps a collection of resources with a
  list-like interface. Values are stored and retrieved
  using familiar accessors like \code{[[} and \code{[[<-}. Coercion
  between external media and \R objects is based on the
  \code{\linkS4class{Media}} framework.
}

\section{Data access}{
  The \code{RestContainer} object maps familiar R list accessors to CRUD
  operations on \code{\linkS4class{RestUri}}.

  \itemize{
    \item{
      \code{x[] <- value}: Creates resources for the objects in
      \code{value} at \code{x}. This is the
      \code{\link{create}}/\code{POST} operation. Unlike an R list, the
      resources are added to the collection without removing any
      existing resources. This inconsistency is unfortunate, so we might
      change this behavior in the future.
    }
    \item{
      \code{x$name}, \code{x[[i]]}: Reads the named element. This is the
      \code{\link{read}}/\code{GET} operation.
    }
    \item{
      \code{x[i]}: Reads the named elements, which are returned in a
      list. This is the vectorized \code{\link{read}}/\code{GET}
      operation. Unlike an R list, this is not an endomorphism, in that
      the return value is dropped to a list and is no longer attached to
      the REST interface.
    }
    \item{
      \code{x$name <- value}, \code{x[[i]] <- value}: Updates the named
      resource with \code{value}. This is the
      \code{\link{update}}/\code{PUT} operation.
    }
    \item{
      \code{x[i] <- value}: Updates resources at \code{x} with the
      objects in \code{value}, a list. This is the
      vectorized \code{\link{update}}/\code{PUT} operation.
    }
    \item{
      \code{x$name <- NULL}, \code{x[[i]] <- NULL}: Deletes the named
      resource. This is the \code{\link{delete}}/\code{DELETE} operation.
    }
  }
}

\section{Constructor}{
  \itemize{
    \item{\code{RestContainer(...)}: Constructs an instance based on
      \code{\link{RestUri}(...)}.
    }
  }
}

\examples{
apache <- RestContainer("http://wiki.apache.org")
apache$solr
}

\seealso{
  \code{\linkS4class{RestUri}}, which is a lower-level but perhaps more
  sensible interface.
}

\author{ Michael Lawrence }

\keyword{methods}
\keyword{classes}
