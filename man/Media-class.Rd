\name{Media-class}
\docType{class}

\alias{class:Media}
\alias{Media-class}

\alias{class:application/*}
\alias{application/*-class}
\alias{class:audio/*}
\alias{audio/*-class}
\alias{class:image/*}
\alias{image/*-class}
\alias{class:message/*}
\alias{message/*-class}
\alias{class:model/*}
\alias{model/*-class}
\alias{class:multipart/*}
\alias{multipart/*-class}
\alias{class:text/*}
\alias{text/*-class}
\alias{class:video/*}
\alias{video/*-class}
\alias{class:text/plain}
\alias{text/plain-class}
\alias{class:text/html}
\alias{text/html-class}
\alias{class:text/csv}
\alias{text/csv-class}
\alias{class:application/xml}
\alias{application/xml-class}
\alias{class:application/json}
\alias{application/json-class}

% Utilities:
\alias{mediaCoercionTable}
\alias{length,NullMedia-method}

% Coercion:
\alias{mediaTarget}
\alias{mediaTarget,application/json-method}
\alias{mediaTarget,application/xml-method}
\alias{mediaTarget,text/html-method}
\alias{mediaTarget,text/csv-method}
\alias{mediaTarget,text/*-method}
\alias{mediaTarget,NullMedia-method}
\alias{coerce,application/json,list-method}
\alias{coerce,data.frame,text/csv-method}
\alias{coerce,application/xml,XMLAbstractNode-method}
\alias{coerce,text/csv,data.frame-method}
\alias{coerce,ANY,Media-method}
\alias{coerce,list,application/json-method}
\alias{coerce,data.frame,Media-method}

% Show:
\alias{show,Media-method}

\title{Media}
\description{
  The \code{Media} object represents a value identified by a URI. There
  is a \code{Media} subclass for each media type, such as
  \dQuote{text/csv} or \dQuote{application/xml}. Coercion methods (see
  \code{\link{setAs}}) define mappings between \code{Media} subclasses
  and \R objects. The user does not usually need to access this
  functionality directly.
}

\section{Type conversion}{
  Each \code{Media} subclass may be converted to/from one or more \R
  types. The mappings are established by \code{\link{setAs}}.

  The following bi-directional mappings are built into the package:
  \tabular{ll}{
    \code{application/xml}, \code{text/html} \tab
    \code{\link[XML:XMLAbstractNode-class]{XMLAbstractNode}} \cr
    \code{application/json} \tab \code{list} \cr
    \code{text/csv} \tab \code{data.frame} \cr
    \code{text/*} \tab \code{character}
  }
  But call \code{mediaCoercionTable} to see what is defined in the
  current session.

  The \code{\link{as}} function is the canonical interface to converting
  media to \R objects. It (usefully) requires that the user specify the
  target \R type. For convenience, the \code{mediaTarget} generic
  returns the default R type for a given \code{Media} object.

  To support a new media type, one should define a \code{Media} subclass
  with the same name as the media type (\code{application/xml}), a
  corresponding \code{mediaTarget} method, and all relevant
  \code{coerce} methods. See the \code{Media} class hierarchy to
  determine where the new type fits.
}

\section{Helpers}{
  \itemize{
    \item{\code{mediaCoercionTable()}: Returns a character matrix with
      columns \dQuote{from} and \dQuote{to}, indicating the available
      coercions of media types to/from \R objects.
    }
  }
}

\examples{
txt <- '{"json":{"rocks":true}}'
json <- as(txt, "application/json")
as(json, mediaTarget(json))
}

\author{ Michael Lawrence }

\keyword{methods}
\keyword{classes}


