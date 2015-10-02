\name{RestUri-class}
\docType{class}

\alias{class:RestUri}
\alias{RestUri-class}

% CRUD:
\alias{create}
\alias{create,RestUri-method}
\alias{read}
\alias{read,RestUri-method}
\alias{update}
\alias{update,RestUri-method}
\alias{delete}
\alias{delete,RestUri-method}
\alias{create,character-method}
\alias{read,character-method}
\alias{update,character-method}
\alias{delete,character-method}

% Construction:
\alias{RestUri}
\alias{$,RestUri-method}
\alias{[[,RestUri-method}
\alias{$<-,RestUri-method}
\alias{[[<-,RestUri-method}

% Container:
\alias{container}
\alias{container,RestUri-method}
\alias{container<-}
\alias{container<-,RestUri-method}

% Utilities
\alias{purgeCache}
\alias{purgeCache,RestUri-method}

% Show:
\alias{show,RestUri-method}

\title{RestUri}
\description{
  The \code{RestUri} object represents a resource accessible via a
  RESTful interface. It extends \code{character} with a protocol, used
  for accessing the data, as well as a cache. R objects are converted
  to/from external media via the \code{\linkS4class{Media}} framework.
}

\section{CRUD interface}{
  There are four canonical, abstract types of REST operations: create,
  read, update, delete (CRUD). The CRUD model was borrowed from
  traditional databases. The restfulr package maps those four operations
  to R functions of the same name. The functions are generic, and there
  are methods for \code{RestUri} and \code{character} (taken to be a
  URI), described below.

  \describe{
    \item{}{
      \code{create(x, value, ...)}: Creates a resource at \code{x} by
      converting \code{value} to a supported media type. The \dots
      become query parameters on \code{x}. This corresponds to an HTTP
      \code{POST}.
    }
    \item{}{
      \code{read(x, ...)}: Reads the resource at \code{x}, coerces it to
      an R object, and returns the object. The \dots become query
      parameters on \code{x}. This corresponds to an HTTP \code{GET}.
    }
    \item{}{
      \code{update(object, value, ...)}: Updates the resource at \code{x} by
      converting \code{value} to a supported media type. The \dots
      become query parameters on \code{x}. This corresponds to an HTTP
      \code{PUT}.
    }
    \item{}{
      \code{delete(x, ...)}: Deletes the resource at \code{x}. This
      corresponds to an HTTP \code{DELETE}.
    }
  }
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{RestUri(base.uri, protocol = CRUDProtocol(base.uri, ...),
        cache = globalRestClientCache(), ...)}: Constructs a
      \code{RestUri} object, pointing to \code{base.uri}, a string
      representation of the URI. The \code{protocol} (a
      \code{\linkS4class{CRUDProtocol}} instance) is automatically
      determined from the scheme of the URI. By default, all instances
      share the same global \code{cache}, a
      \code{\linkS4class{MediaCache}} instance.
    }
    \item{}{\code{x$name}: Extends the path of \code{x} by appending
      \code{name}. This is a convenient way to narrow a URI and is
      intuitive if one thinks of a tree of resources as a nested list.
    }
    \item{}{\code{x[[i]]}: Extends the path of \code{x} by appending
      \code{i}.
    }
  }
}

\section{Container support}{
  \describe{
    \item{}{
      \code{container(x)}: Gets a \code{\linkS4class{RestContainer}}
      object for treating \code{x} as a list-like container.
    }
  }
}

\examples{
apache <- RestUri("http://wiki.apache.org")
read(apache$solr)
}

\author{ Michael Lawrence }

\keyword{methods}
\keyword{classes}
