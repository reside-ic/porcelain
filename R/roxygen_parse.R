## The parsing of the @porcelain tag is fairly unpleasant, as is all
## parsing, especially because roxygen extensions do not allow
## namespacing and supporting multiple tags is quite heavy. So we pop
## everything into one string and try and break it up.
##
## The first line(s) must conform to the pattern
##   <VERB> <PATH> => <RETURNING>
## Following lines describe input and are in form
##   <LOCATION> <TARGET> :: <TYPE>
##
## The format is further discussed in the vignette
roxy_parse_string <- function(text, file = NULL, line = NULL) {
  re <- "^\\s*([A-Z]+)\\s+([^ =]+)\\s*=>\\s*(.*?)(\n|$)(.*)"
  if (!grepl(re, text)) {
    ## TODO: here, we'd use file/line information to make this more
    ## obvious. We could also defer errors until processing by
    ## returning error information here or in the calling function.
    stop("invalid porcelain tag")
  }
  ## TODO: Don't use sub but use regexec so that we can work out line
  ## numbers here.
  method <- sub(re, "\\1", text)
  path <- sub(re, "\\2", text)
  returning <- roxy_parse_returning(trimws(sub(re, "\\3", text)))
  inputs <- roxy_parse_inputs(sub(re, "\\5", text))

  list(method = method,
       path = path,
       inputs = inputs,
       returning = returning)
}


roxy_parse_returning <- function(text) {
  ## TODO: we will want to autoquote the arguments to this function,
  ## which might vary based on inputs (e.g. a status code of 200 might
  ## want to come through as an integer, but a schema should not need
  ## quoting).  We could respond to the actual name if need be though.
  if (grepl("(", text, fixed = TRUE)) {
    ret <- as.list(parse(text = text)[[1]])
    for (i in seq_along(ret)) {
      if (is.name(ret[[i]])) {
        ret[[i]] <- deparse(ret[[i]])
      }
    }
  } else {
    ret <- list(text)
  }

  ## Here, we could validate the name?
  ret
}


roxy_parse_inputs <- function(text) {
  ## TODO: we lose the ability to give good errors the more processing
  ## we do here, so once this works refactor this to use a different
  ## parsing approach. The key thing to eventually get is the ability
  ## to associate a line number, which we'll want to pass through here
  ## as an argument too.
  ##
  ## TODO: this sort of processing is probably not nice enough and
  ## won't allow multiline input specification (not sure we need that?)
  inputs <- trimws(strsplit(text, "\n")[[1]])
  inputs <- inputs[nzchar(inputs)]

  if (length(inputs) == 0L) {
    return(NULL)
  }

  re <- "^([^ ]+)\\s+([^ ]+)\\s+::\\s+(.*)$"
  ## TODO: better error here, possibly better regex or parsing
  ## approach
  stopifnot(all(grepl(re, inputs)))
  loc <- sub(re, "\\1", inputs)
  name <- sub(re, "\\2", inputs)
  details <- sub(re, "\\3", inputs)

  ## TODO: Better errors
  valid <- c("query", "body", "state")
  stopifnot(all(loc %in% valid))

  ## TODO: may need some additional parsing/validation here,
  ## especially for body where we have the same json(schema) and
  ## binary(content-type) system as used in returning.
  ret <- list()
  for (v in valid) {
    i <- loc == v
    if (any(i)) {
      ret[[v]] <- set_names(details[i], name[i])
    }
  }

  ret
}
