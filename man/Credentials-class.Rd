\name{Credentials-class}
\docType{class}

\alias{class:Credentials}
\alias{Credentials-class}

\alias{credentials}

\alias{username}
\alias{username,Credentials-method}

\alias{password}
\alias{password,Credentials-method}

\title{Credentials}

\description{
  \code{Credentials} stores authentication credentials (username and
  password). Note that it is easy to retrieve the password from this
  object. There is no encryption or other security.
}

\section{Accessors}{
  \describe{
    \item{}{
      \code{username}: get the username as a string.
    }
    \item{}{
      \code{password}: get the password as a plain text string.
    }
  }
}


\author{ Michael Lawrence }

\keyword{methods}
\keyword{classes}
