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
roxy_parse_string <- function(text, file, line) {
  re <- "^\\s*([A-Z]+)\\s+([^ =]+)\\s*=>\\s*(.+?)(\n|$)(.*)"
  newline <- which(strsplit(text, NULL)[[1]] == "\n")
  len <- length(newline) + 1L
  line_end <- line + len - 1L

  if (!grepl(re, text)) {
    ## TODO: use roxy_error
    line_range <- if (len == 1) line else sprintf("%d-%d", line, line_end)
    stop(paste(
      "Failed to find endpoint description in @porcelain tag",
      "  - must match <VERB> <PATH> => <RETURNING>",
      sprintf("  - error occured at %s:%s", file, line_range),
      sep = "\n"),
      call. = FALSE)
  }

  m <- regexec(re, text)[[1L]]

  m_start <- as.integer(m)[-1]
  m_len <- attr(m, "match.length")[-1]
  m_line <- line + rowSums(outer(m_start, newline, ">"))

  method <- str_extract(text, m_start[[1]], m_len[[1]])
  path <- str_extract(text, m_start[[2]], m_len[[2]])
  returning <- str_extract(text, m_start[[3]], m_len[[3]])
  inputs <- str_extract(text, m_start[[5]], m_len[[5]])

  list(method = method,
       path = path,
       returning = roxy_parse_returning(returning, file, m_line[[3]]),
       inputs = roxy_parse_inputs(inputs, file, m_line[[5]]))
}


roxy_parse_returning <- function(text, file, line) {
  parse_expr(text, FALSE, "@porcelain returning argument",
             file, line)
}


roxy_parse_inputs <- function(text, file, line) {
  ## TODO: Can't allow multiline inputs here without some work
  inputs <- trimws(strsplit(text, "\n")[[1]])
  line <- line + seq_along(inputs) - 1L
  i <- nzchar(inputs)

  inputs <- inputs[i]
  line <- line[i]

  if (length(inputs) == 0L) {
    return(NULL)
  }

  re <- "^([^ ]+)\\s+([^ ]+)\\s+::\\s+(.+)$"
  err <- !grepl(re, inputs)
  if (any(err)) {
    line_err <- paste(line[err], collapse = ",")
    stop(paste(
      "Failed to match input description in @porcelain tag",
      "  - must match <TYPE> <DEST> :: <DESCRIPTION>",
      sprintf("  - error occured at %s:%s", file, line_err),
      sep = "\n"),
      call. = FALSE)
  }

  loc <- sub(re, "\\1", inputs)
  name <- sub(re, "\\2", inputs)
  details <- sub(re, "\\3", inputs)

  valid <- c("query", "body", "state")
  err <- !loc %in% valid
  if (any(err)) {
    line_err <- paste(line[err], collapse = ",")
    stop(paste(
      "Invalid input type",
      sprintf("  - must be one of %s", paste(valid, collapse = ", ")),
      sprintf("  - error occured at %s:%s", file, line_err),
      sep = "\n"),
      call. = FALSE)
  }

  ret <- list()
  for (v in valid) {
    i <- loc == v
    if (any(i)) {
      description <- sprintf("input '%s'", name[i])
      parse <- if (v == "body") parse_expr else parse_identifier
      ret[[v]] <- set_names(
        Map(parse, details[i], description, file, line[i]),
        name[i])
    }
  }

  ret
}

parse_identifier <- function(text, description, file, line) {
  ## for query; a simple type (int, etc)
  ## for state; an arg name
  ## these rules might not be good enough then:
  if (!grepl("^[a-zA-Z0-9_.]+$", text)) {
    roxy_error(sprintf("Expected simple identifier for %s", description),
               file, line)
  }
  text
}


parse_expr <- function(text, simple, description, file, line) {
  ret <- tryCatch(
    ## This is not fab as we end up with a <text>:2:0 or similar from
    ## the parse error
    as.list(parse(text = text)[[1]]),
    error = function(e) {
      roxy_error(
        sprintf("Invalid syntax for %s:\n    %s", description, text),
        file, line)
    })
  for (i in seq_along(ret)) {
    if (is.name(ret[[i]])) {
      ret[[i]] <- deparse(ret[[i]])
    }
  }
  ret
}
