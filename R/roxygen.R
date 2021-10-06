## The typical approach here would be to make roxygen2 a hard
## dependency, import either the whole package or the three generics
## (roxy_tag_parse, roclet_process and roclet_output) then register
## the S3 methods with '@export' as normal.  However, this requires
## that we have roxygen2 available at runtime which is pretty weird
## given we only need it when running the package. From R 3.6.0 we can
## use the syntax below to register the methods when roxygen2 is
## loaded, which requires that we inject some raw content into the
## namespace (see R-exts).  Each of these is above the method used so
## we provide the minimum NAMESPACE support.

##' A roclet for processing `@porcelain` tags within a package. This
##' presents an automated declarative approach to defining porcelain
##' APIs using roxygen tags. When you roxygenise your package (e.g.,
##' with `devtools::document()` or `roxygen2::roxygenise()`) this
##' roclet will create a file `R/porcelain.R` within your package that
##' will be included into your package API.
##'
##' @title Define API using roxygen tags
##'
##' @return A roclet, used by `roxygen2` (not typically called by
##'   users directly)
##'
##' @export
porcelain_roclet <- function() {
  roxygen2::roclet("porcelain")
}


package_endpoints <- function(package) {
  if (is.environment(package)) {
    env <- package
  } else {
    env <- getNamespace(package)
  }
  fn <- env[["__porcelain__"]]
  if (is.null(fn)) {
    ## TODO: diagnose the issue here:
    ## * DESCRIPTION does not contain roclet command
    ## * Need to redocument
    stop("Did not find package endpoints")
  }
  fn()
}


## TODO: we really need a way of getting 'validate' through here
porcelain_package_endpoint <- function(package, method, path, state = NULL) {
  endpoint <- package_endpoints(package)[[paste(method, path)]]
  if (is.null(endpoint)) {
    stop(sprintf(
      "Did not find roxygen-based endpoint '%s %s' in package '%s'",
      method, path, package))
  }
  endpoint(state)
}


##' @rawNamespace S3method(roxygen2::roxy_tag_parse, roxy_tag_porcelain)
roxy_tag_parse.roxy_tag_porcelain <- function(x) {
  ## See roxygen_parse.R for the bulk of the implementation.
  x$val <- roxy_parse_string(x$raw, x$file, x$line)
  x
}


##' @rawNamespace S3method(roxygen2::roclet_process, roclet_porcelain)
roclet_process.roclet_porcelain <- function(x, blocks, env, base_path) {
  results <- list()
  message("Adding porcelain endpoints:")

  for (block in blocks) {
    tags <- roxygen2::block_get_tags(block, "porcelain")
    if (length(tags) > 1L) {
      found <- vcapply(tags, function(x)
        sprintf("%s:%d", basename(x$file), x$line))
      stop("More than one @porcelain block found for single function: ",
           paste(found, collapse = ", "))
    }
    if (length(tags) == 1L) {
      ## There's a bit of a trick here with getting the object out; if
      ## we are using the roxygen testing tools (rather than a
      ## package) then we do not actually have a function name and
      ## that function definitely does not exist in env. So we need to
      ## do some faff here.  I am not 100% sure that this is always
      ## desirable though - do we get this NULL type in other cases?
      target <- roxy_get_target(block, env)
      endpoint <- roxy_process(tags[[1]], target, env)
      results <- c(results, list(endpoint))
    }
  }

  if (length(results) == 0) {
    stop("Package contains no '@porcelain' tags")
  }

  c("`__porcelain__` <- function() {",
    paste0("  ", list_call("list", results)),
    "}")
}


roxy_get_target <- function(block, env) {
  if (inherits(block, "roxy_block_NULL")) {
    ## Testing mode, I believe - this is not well documented in
    ## roxygen, and worth checking I think.  This probably should
    ## never end up called outside of the porcelain tests, and would
    ## fail in all sorts of bad ways in a package for example.
    eval(block$call, env)
    as.character(block$call[[2]])
  } else {
    ## TODO: work out what our failure mode is here; are there cases
    ## where this will be NULL, e.g. if used with an anonymous
    ## function or a function name (not value)
    block$object$alias
  }
}

roxy_process <- function(tag, target, env) {
  if (!is.function(env[[target]])) {
    stop("Target is not a function")
  }
  method <- tag$val$method
  path <- tag$val$path
  message(sprintf("- %s %s (%s:%d)", method, path,
                  basename(tag$file), tag$line))

  inputs <- roxy_process_inputs(tag$val$inputs, env, tag)
  returning <- roxy_process_returning(tag$val$returning, env, tag)

  args <- c(
    list(dquote(tag$val$method), dquote(tag$val$path), target),
    inputs,
    list(sprintf("returning = %s", returning)))

  create <- list_call("porcelain::porcelain_endpoint$new", args)

  ## Then we can test this:
  e <- new.env(parent = env)
  e$state <- NULL
  tryCatch(
    eval(parse(text = create), e),
    error = function(e) browser())

  ## Each endpoint gets wrapped in an anonymous function so that
  ## we can call them later at will, rebinding state etc.
  c(sprintf('"%s %s" = function(state) {', method, path),
    paste0("  ", create),
    "}")
}


##' @rawNamespace S3method(roxygen2::roclet_output, roclet_porcelain)
roclet_output.roclet_porcelain <- function(x, results, base_path, ...) {
  header <- "# Generated by porcelain: do not edit by hand"

  dest <- file.path(base_path, "R", "porcelain.R")
  if (file.exists(dest)) {
    prev <- readLines(dest)
    if (length(prev) > 0 && prev[[1]] != header) {
      ## TODO: better, actionable, error
      stop("porcelain.R is not ours to change")
    }
  }

  writeLines(c(header, results), dest)

  invisible(NULL)
}


roxy_error <- function(msg, x) {
  if (!is.null(x)) {
    msg <- paste(msg,
                 sprintf("  (while processing %s:%d)", x$file, x$line),
                 sep = "\n")
  }
  stop(msg)
}


roxy_process_inputs <- function(inputs, env, x) {
  c(list(),
    roxy_process_input_query(inputs$query),
    roxy_process_input_body(inputs$body),
    roxy_process_input_state(inputs$state))
}


roxy_process_input_query <- function(inputs) {
  if (length(inputs) == 0) {
    return(NULL)
  }
  ## TODO: validation on query - args must be simple list with one
  ## element and not function calls.  This is the case for state too,
  ## so this should be done in the parse.
  query_type <- unname(vcapply(inputs, function(x) x[[1]]))
  query <- paste(sprintf('%s = "%s"', names(inputs), query_type),
                 collapse = ", ")
  sprintf("porcelain::porcelain_input_query(%s)", query)
}


roxy_process_input_body <- function(inputs) {
  if (length(inputs) == 0) {
    return(NULL)
  }
  if (length(inputs) > 1) {
    ## This will need relaxing where we do things like destructuring bodies
    stop("Currently only a single body parameter supported")
  }
  input <- inputs[[1]]
  map <- c(binary = "porcelain::porcelain_input_body_binary",
           json = "porcelain::porcelain_input_body_json")
  fn <- input[[1]]
  if (fn %in% names(map)) {
    fn <- map[[fn]]
  } else {
    stop(sprintf("Unknown body type '%s'", type))
  }
  args <- paste(c(dquote(names(inputs)),
                  vcapply(input[-1], deparse)), collapse = ", ")
  sprintf("%s(%s)", fn, args)
}


roxy_process_input_state <- function(inputs) {
  if (length(inputs) == 0) {
    return(NULL)
  }
  ## TODO: require that the rhs is a single arg, any type
  state <- unname(vcapply(inputs, function(x) x[[1]]))
  args <- paste(sprintf("%s = state$%s", names(inputs), state),
                collapse = ", ")
  sprintf("porcelain::porcelain_state(%s)", args)
}


roxy_process_returning <- function(returning, env, x) {
  map <- c(
    json = "porcelain::porcelain_returning_json",
    binary = "porcelain::porcelain_returning_binary",
    generic = "porcelain::porcelain_returning")
  fn <- returning[[1]]
  if (fn %in% names(map)) {
    fn <- map[[fn]]
  } else if (grepl("::", fn)) {
    fn <- fn
  } else if (!is.null(exists(fn, env, mode = "function"))) {
    fn <- fn
  } else {
    stop(sprintf("Did not find returning function '%s'", fn))
  }

  args <- paste(vcapply(returning[-1], deparse), collapse = ", ")
  sprintf("%s(%s)", fn, args)
}
