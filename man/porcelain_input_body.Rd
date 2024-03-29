% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input.R
\name{porcelain_input_body_binary}
\alias{porcelain_input_body_binary}
\alias{porcelain_input_body_json}
\title{Control for body parameters}
\usage{
porcelain_input_body_binary(name, content_type = NULL)

porcelain_input_body_json(name, schema = NULL, root = NULL, extract = NULL)
}
\arguments{
\item{name}{Name of the parameter}

\item{content_type}{Content type for the input. If not given, then
\code{application/octet-stream} is used. Provide a vector of valid
types to allow any of the types to be passed.}

\item{schema}{The name of the json schema to use}

\item{root}{The root of the schema directory.}

\item{extract}{Optionally, the name of an element to extract from
the json. If given, then the body must be a json object (not an
array, for example) and \code{extract} must refer to a top-level key
within it. We will extract the \emph{JSON string} corresponding to
this key and forward that to the argument \code{name}.
Deserialisation of the json is still the target function's
responsibility but there will be less of it.}
}
\description{
Control for body parameters.  This might change.  There are
several types of HTTP bodies that we want to consider here - the
primary ones are a body uploaded in binary, the other is a json
object.  In the latter we want to validate the body against a
schema (at least if validation is used).  In future we might also
support a form input here too.
}
