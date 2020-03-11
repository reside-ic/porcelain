`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


system_file <- function(...) {
  system.file(..., mustWork = TRUE)
}


vlapply <- function(x, fun, ...) {
  vapply(x, fun, logical(1), ...)
}


vcapply <- function(x, fun, ...) {
  vapply(x, fun, character(1), ...)
}


## This should probably be tuneable?
to_json <- function(x) {
  jsonlite::toJSON(x, json_verbatim = TRUE, na = "null", null = "null")
}


to_json_string <- function(x) {
  as.character(to_json(x))
}


is_directory <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}


lock_bindings <- function(names, e) {
  for (nm in names) {
    lockBinding(nm, e)
  }
}


squote <- function(x) {
  sprintf("'%s'", x)
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


## NOTE: plumber does not expose a parser here so we just bodge one
## together
parse_plumber_path <- function(x) {
  p <- strsplit(x, "/", fixed = TRUE)[[1]]
  i <- grepl("^<([^>]+)>$", p)
  name <- p[i]
  if (length(name) == 0L) {
    return(NULL)
  }
  name <- substr(name, 2L, nchar(name) - 1L)
  type <- rep("string", length(name))

  j <- grepl(":", name, fixed = TRUE)

  translate <- list(
    "logical" = c("bool", "boolean", "logical"),
    "integer" = c("int", "integer"),
    "numeric" = c("dbl", "double", "float", "number", "numeric"),
    "string" = c("chr", "str", "character", "string"))
  translate <- set_names(rep(names(translate), lengths(translate)),
                         unlist(translate, FALSE, FALSE))

  if (any(j)) {
    type[j] <- unname(translate[sub(".*:", "", name[j])])
    type[is.na(type)] <- "string"
    name[j] <- sub(":.*", "", name[j])
  }

  cbind(name = name, type = type)
}


## There's a bunch to do here with subtypes and encodings - how we
## read text will depend on that.
##
## Doing this properly requires the following:
##   https://mimesniff.spec.whatwg.org/#parsing-a-mime-type
##
## See also http://pretty-rfc.herokuapp.com/RFC2616#media.types
##
## I'm just punting on this now.
parse_mime <- function(mime) {
  if (is.null(mime)) {
    return(NULL)
  }
  if (grepl(";", mime, fixed = TRUE)) {
    ## Discard all parameters for now - later we might use these if we
    ## have text/plain inputs with encoding to get right.
    mime <- sub("\\s*;.*", "", mime)
  }
  parts <- strsplit(mime, "/", fixed = TRUE)[[1L]]
  type <- parts[[1L]]
  subtype <- parts[[2L]]
  is_text <- type == "text" ||
    (type == "application" && subtype %in% c("xml", "json"))
  list(mime = mime,
       type = type,
       subtype = subtype,
       is_text = is_text)
}


## Just a utility that works with an argslist and works out if they
## are missing or not.
formals_args <- function(args) {
  required <- logical(length(args))
  for (i in seq_along(args)) {
    x <- args[[i]]
    required[[i]] <- missing(x)
  }
  names(args)[required]
}
