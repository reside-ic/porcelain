##' Add headers to endpoint output data
##'
##' Intended to be used from endpoint target function. Note `Content-Type`
##' headers are handled by returning arg to endpoint.
##'
##' @param data Response data
##' @param headers Named list of headers to add.
##'
##' @return Data from endpoint target with headers
##' @export
##'
##' @examples
##' porcelain_add_headers("output",
##'                    list("Content-Dispotition" = "output_file.txt"))
porcelain_add_headers <- function(data, headers) {
  attributes(data) <- list("porcelain_headers" = headers)
  data
}

get_porcelain_headers <- function(data) {
  attr(data, "porcelain_headers", exact = TRUE)
}

remove_porcelain_headers <- function(data) {
  attr(data, "porcelain_headers") <- NULL
  data
}
