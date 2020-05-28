#' Add headers to endpoint output data
#'
#' Intended to be used from endpoint target function. Note `Content-Type`
#' headers are handled by returning arg to endpoint.
#'
#' @param data Response data
#' @param headers Named list of headers to add.
#'
#' @return Data from endpoint target with headers
#' @export
#'
#' @examples
#' pkgapi_add_headers("output", list("Content-Dispotition" = "output_file.txt"))
pkgapi_add_headers <- function(data, headers) {
  attributes(data) <- list("pkgapi_headers" = headers)
  data
}

get_pkgapi_headers <- function(data) {
  attr(data, "pkgapi_headers", exact = TRUE)
}
