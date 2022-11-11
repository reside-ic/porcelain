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
parse_path_parameters <- function(x) {
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
formals_required <- function(f) {
  args <- formals(f)
  required <- logical(length(args))
  for (i in seq_along(args)) {
    x <- args[[i]]
    required[[i]] <- missing(x)
  }
  names(args)[required]
}


## This can be done better - I don't love the .target name at all -
## but it will work at least, and the interface is good.
bind_args <- function(target, data) {
  args <- formals(target)
  keep <- args[!(names(args) %in% names(data))]

  env <- list2env(data, new.env(parent = baseenv()))
  env$.target <- target
  body <- call("{", as.call(c(list(quote(.target)),
                              lapply(names(args), as.name))))

  as.function(c(keep, body), env)
}


## Find the root for included files (not including system files) for a
## package. This corresponds to the "inst" directory.  This is
## different when the package load has been simulated with
## pkgload. This will come in useful in both tests and documentation.
##
## There are 4 cases here:
##
## porcelain real    + package real    => system.file is correct
## porcelain pkgload + package real    => system.file is correct
## porcelain real    + package pkgload => system.file is incorrect
## porcelain pkgload + package pkgload => system.file is correct
package_file_root <- function(package) {
  path <- system.file(package = package, mustWork = TRUE)
  if (pkgload_loaded() &&
      pkgload::is_dev_package(package) &&
      !pkgload::is_dev_package("porcelain")) {
    path <- file.path(path, "inst")
  }
  path
}


pkgload_loaded <- function() {
  "pkgload" %in% loadedNamespaces()
}


json_parse_extract <- function(json, name) {
  cache$v8$call("jsonParseExtract", json, name)
}


is_pkgload_package <- function(name) {
  !is.null(name) &&
     "pkgload" %in% loadedNamespaces() &&
     pkgload::is_dev_package(name)
}


free_port <- function(min, max, attempts = 20) {
  r <- max - min + 1L
  for (i in seq_len(attempts)) {
    p <- min + sample.int(r, 1L) - 1L
    if (check_port(p)) {
      return(p)
    }
  }
  stop(sprintf("Did not find a free port between %d..%d in %d attempts",
               min, max, attempts),
       call. = FALSE)
}


check_port <- function(port, timeout = 0.1) {
  con <- tryCatch(suppressWarnings(socketConnection(
    "localhost", port = port, timeout = timeout, open = "r")),
    error = function(e) NULL)
  if (is.null(con)) {
    return(TRUE)
  }
  close(con)
  FALSE
}


wait_until <- function(condition, timeout = 10, poll = timeout / 100,
                       verbose = FALSE, title = NULL) {
  t_start <- Sys.time()
  t_quit <- t_start + timeout
  now <- t_start
  if (verbose) {
    message(title %||% "Waiting ")
  }
  repeat {
    if (condition()) {
      break
    }
    if (verbose) {
      message(".", appendLF = FALSE)
    }
    Sys.sleep(poll)
    now <- Sys.time()
    if (now > t_quit) {
      stop("Timeout reached")
    }
  }
  if (verbose) {
    elapsed_s <- round(as.numeric(now - t_start, "secs"), 1)
    message(sprintf("...OK in %s s", elapsed_s))
  }
  now - t_start
}


read_lines_if_exists <- function(path) {
  if (file.exists(path)) {
    readLines(path)
  }
}


str_extract <- function(string, start, len) {
  substr(string, start, start + len - 1L)
}


dquote <- function(x) {
  sprintf('"%s"', x)
}


list_call <- function(fn, x) {
  n <- length(x)
  prefix <-  "  "
  for (i in seq_along(x)) {
    el <- paste0(prefix, x[[i]])
    end <- if (i == n) ")" else ","
    el[[length(el)]] <- paste0(el[[length(el)]], end)
    x[[i]] <- el
  }

  c(paste0(fn, "("), unlist(x))
}


package_name <- function(env) {
  utils::packageName(env)
}

now_utc <- function() {
  as.POSIXct(Sys.time(), tz = "UTC")
}

## Modified version of difftime to show timings
## in ms and to a precision
format_difftime <- function(time1, time2) {
  z <- as.numeric(time1 - time2, "secs")
  units <- if (!is.finite(z) || z <= 1) {
    "ms"
  } else if (z < 60) {
    "secs"
  } else if (z < 3600) {
    "mins"
  } else {
    "hours"
  }
  units_difftime <- if (units == "ms") "secs" else units
  z <- as.numeric(time1 - time2, units_difftime)
  if (units == "ms") {
    z <- z * 1000
    sprintf("%.0f %s", z, units)
  } else {
    sprintf("%.2f %s", z, units)
  }
}
