% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input.R
\name{porcelain_input_query}
\alias{porcelain_input_query}
\title{Control for query parameters}
\usage{
porcelain_input_query(..., .parameters = list(...))
}
\arguments{
\item{...}{Named arguments representing accepted parameters.  The
value of each must be a type.}

\item{.parameters}{A list of named parameters to accept, instead
of using \code{...} - this interface is considerably easier to
program against if building an API programmatically, avoiding
the use of \code{\link{do.call}}.}
}
\description{
Control for query parameters.
}
\examples{
porcelain::porcelain_input_query(number = "integer")
}
