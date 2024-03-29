% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/returning.R
\name{porcelain_returning}
\alias{porcelain_returning}
\alias{porcelain_returning_json}
\alias{porcelain_returning_binary}
\alias{porcelain_returning_text}
\title{Support for endpoint return types}
\usage{
porcelain_returning(content_type, process, validate, status_code = 200L)

porcelain_returning_json(schema = NULL, root = NULL, status_code = 200L)

porcelain_returning_binary(status_code = 200L)

porcelain_returning_text(status_code = 200L)
}
\arguments{
\item{content_type}{The MIME content type for the endpoint,
e.g. \code{text/plain}, \code{application/json}.}

\item{process}{A processing function that will convert the output
of the handler function into something of the type
\code{content_type}.  This should be independent of arguments
passed to the endpoint, so practically this is the final stage
of serialisation.}

\item{validate}{A function that validates the return value and
throws an error if the output is not expected.  This will only
be used if the endpoint is created with \code{validate = TRUE}.}

\item{status_code}{The HTTP status code that the endpoint will use
on a successful return.  The default of 200 should be
reasonable.}

\item{schema}{The name of the json schema to use}

\item{root}{The root of the schema directory.}
}
\description{
Support for describing and controlling expected return types.  The
high-level functions (\code{porcelain_returning_json} and
\code{porcelain_returning_binary}) should be generally used.
}
