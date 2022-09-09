## Support for easily sending requests to the plumber without running it
plumber_request <- function(plumber, method, path, query = NULL,
                            body = NULL, content_type = NULL) {
  req <- new.env(parent = emptyenv())
  req[["REQUEST_METHOD"]] <- toupper(method)
  req[["PATH_INFO"]] <- path
  req[["QUERY_STRING"]] <- query_string(query)

  req[["rook.input"]] <- list(
    read_lines = function() body,
    read = function() body)

  if (!is.null(body)) {
    req[["HTTP_CONTENT_TYPE"]] <- request_content_type(body, content_type)
  }

  plumber$call(req)
}


query_string <- function(query) {
  if (is.null(query)) {
    return("")
  }
  assert_named(query)
  ## On input we map '?param=' to list(param = NA) so we map back to this
  ## query string for testing
  query[is.na(query)] <- ""
  pairs <- sprintf("%s=%s", names(query), as.character(query))
  utils::URLencode(paste0("?", paste(pairs, collapse = "&")))
}


## Eventually we might move directly to using httr requests at which
## point this goes away because we'd do perhaps httr::upload_file() or
## something?
request_content_type <- function(body, content_type) {
  if (!is.null(content_type)) {
    return(content_type)
  }
  if (is.raw(body)) {
    return("application/octet-stream")
  } else {
    return("application/json")
  }
}
