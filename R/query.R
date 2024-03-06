## This is not as sophisticated as plumber's parseQS function, but we
## can't use that as it's not exported. We make the assumption that
## the encoding is straightforward, and that there is only a single
## item for each key
##
## It's possible that the encoding issue will go away with the newer
## builds of windows that use a UTF-8 based runtime. See this github
## thread for discussion:
##
## https://github.com/rstudio/plumber/pull/314#discussion_r239992879
##
## It's not actually obvious what the correct thing to do with
## duplicate query keys, see:
##
##   https://stackoverflow.com/q/1746507
##
## So in keeping with what we typically do here (provide a
## comma-separated list as as a single query parameter) and in keeping
## with pushing for type stability, we'll forbid duplicate keys. This
## is also easier to implement!
##
## We also explicitly reject incomplete queries (e.g., "?a=1&b=&c=3")
## rather than silently dropping the 'b' part of the query.
parse_query <- function(query) {
  if (length(query) == 0L || query == "") {
    return(list())
  }

  query <- chartr("+", " ", sub("^\\?", "", query))
  args <- strsplit(strsplit(query, "&", fixed = TRUE)[[1]], "=", fixed = TRUE)
  if (length(args) == 0) {
    return(list())
  }

  keys <- trimws(vcapply(args, "[[", 1L))

  err <- lengths(args) != 2
  if (any(err)) {
    stop(sprintf("Incomplete query for %s",
                 paste(squote(keys[err]), collapse = ", ")))
  }
  args <- lapply(args, function(x) trimws(utils::URLdecode(x)))

  ## if (any(err)) {
  ##   ## TODO: should be 400 not 500 error
  ##   stop(sprintf("Incomplete query for %s",
  ##                paste(squote(keys[err]), collapse = ", ")))
  ## }
  args <- lapply(args, function(x) trimws(utils::URLdecode(x)))
  vals <- lapply(args, function(x) if (length(x) == 1) NA else x[[2]])

  if (anyDuplicated(keys)) {
    stop(sprintf(
      "Unexpected duplicate keys %s",
      paste(squote(unique(keys[duplicated(keys)])), collapse = ", ")))
  }

  names(vals) <- keys

  vals
}
