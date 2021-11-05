## The typical approach here would be to make roxygen2 a hard
## dependency, import either the whole package or the three generics
## (roxy_tag_parse, roclet_process and roclet_output) then register
## the S3 methods with '@export' as normal.  However, this requires
## that we have roxygen2 available at runtime which is pretty weird
## given we only need it when documenting the package. From R 3.6.0 we can
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
    pkg <- packageName(env)
    if (is.null(pkg)) {
      stop("No endpoints found: input is not a package name or namespace")
    }
    stop(sprintf("No endpoints found in package '%s'", pkg))
  }
  fn()
}


porcelain_package_endpoint <- function(package, method, path, state = NULL,
                                       validate = NULL) {
  endpoint <- package_endpoints(package)[[paste(method, path)]]
  if (is.null(endpoint)) {
    pkg <- packageName(package) %||% "<anonymous>"
    stop(sprintf(
      "Did not find roxygen-based endpoint '%s %s' in package '%s'",
      method, path, pkg))
  }
  endpoint(state, validate)
}


##' @rawNamespace S3method(roxygen2::roxy_tag_parse, roxy_tag_porcelain)
roxy_tag_parse.roxy_tag_porcelain <- function(x) { # nolint
  ## See roxygen_parse.R for the bulk of the implementation.
  x$val <- roxy_parse_string(x$raw, x$file, x$line)
  x
}


##' @rawNamespace S3method(roxygen2::roclet_process, roclet_porcelain)
roclet_process.roclet_porcelain <- function(x, blocks, env, base_path) { #nolint
  results <- list()
  message("Adding porcelain endpoints:")

  for (block in blocks) {
    tags <- roxygen2::block_get_tags(block, "porcelain")
    if (length(tags) > 1L) {
      found <- sprintf("%s:%s", basename(tags[[1]]$file),
              paste(vcapply(tags, function(x)
                as.character(x$line)), collapse = ","))
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
      target <- roxy_get_target(block, env, tags[[1]])
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


## This function has been determined emperically, as roxygen behaves
## quite differently here in testing mode and when loading a package
## for real.  The main aim of this function is to ensure we try known
## good ways of getting the target function and throw an error that
## locates the problematic block when not.
roxy_get_target <- function(block, env, tag) {
  target <- NULL
  if (!is.null(block$object$alias)) {
    target <- block$object$alias
  } else if (!is.null(block$call) && length(block$call) == 3) {
    eval(block$call, env)
    target <- as.character(block$call[[2]])
  }
  if (is.null(target) || !is.function(env[[target]])) {
    roxy_error("Could not determine endpoint target", tag$file, tag$line)
  }
  target
}


roxy_process <- function(tag, target, env) {
  method <- tag$val$method
  path <- tag$val$path
  message(sprintf("- %s %s (%s:%d)", method, path,
                  basename(tag$file), tag$line))

  inputs <- roxy_process_inputs(tag$val$inputs, env, tag)
  returning <- roxy_process_returning(tag$val$returning, env, tag)

  args <- c(
    list(dquote(tag$val$method), dquote(tag$val$path), target),
    inputs,
    list(sprintf("returning = %s", returning)),
    list("validate = validate"))

  create <- list_call("porcelain::porcelain_endpoint$new", args)

  ## Then we can test this:
  ## Possible issues:
  ## * parse failure
  ## * substitution failure (e.g., required args missing)
  e <- new.env(parent = env)
  e$state <- NULL
  e$validate <- NULL
  tryCatch(
    eval(parse(text = create), e),
    error = function(e) {
      msg <- sprintf(
        "Created invalid endpoint:\n%s\nCreated endpoint was:\n\n%s\n",
        e$message, paste("    ", create, collapse = "\n"))
      roxy_error(msg, tag$file, tag$line)
    })

  ## Each endpoint gets wrapped in an anonymous function so that
  ## we can call them later at will, rebinding state etc.
  c(sprintf('"%s %s" = function(state, validate) {', method, path),
    paste0("  ", create),
    "}")
}


##' @rawNamespace S3method(roxygen2::roclet_output, roclet_porcelain)
roclet_output.roclet_porcelain <- function(x, results, base_path, ...) {#nolint
  roxy_output(results, file.path(base_path, "R", "porcelain.R"))
  invisible(NULL)
}


roxy_error <- function(msg, file, line) {
  msg <- paste(msg,
               sprintf("  (while processing %s:%s)", file, line),
               sep = "\n")
  stop(msg, call. = FALSE)
}


roxy_process_inputs <- function(inputs, env, x) {
  c(list(),
    roxy_process_input_query(inputs$query),
    roxy_process_input_body(inputs$body, x),
    roxy_process_input_state(inputs$state))
}


roxy_process_input_query <- function(inputs) {
  if (length(inputs) == 0) {
    return(NULL)
  }
  query_type <- unname(vcapply(inputs, identity))
  query <- paste(sprintf('%s = "%s"', names(inputs), query_type),
                 collapse = ", ")
  sprintf("porcelain::porcelain_input_query(%s)", query)
}


roxy_process_input_body <- function(inputs, tag) {
  if (length(inputs) == 0) {
    return(NULL)
  }
  if (length(inputs) > 1) {
    ## This will need relaxing where we do things like destructuring bodies
    roxy_error("Currently only a single body parameter supported",
               tag$file, tag$line)
  }
  input <- inputs[[1]]
  map <- c(binary = "porcelain::porcelain_input_body_binary",
           json = "porcelain::porcelain_input_body_json")
  fn <- input[[1]]
  if (fn %in% names(map)) {
    fn <- map[[fn]]
  } else {
    stop(sprintf("Unknown body type '%s'", fn))
  }
  args <- paste(c(dquote(names(inputs)),
                  vcapply(input[-1], deparse)), collapse = ", ")
  sprintf("%s(%s)", fn, args)
}


roxy_process_input_state <- function(inputs) {
  if (length(inputs) == 0) {
    return(NULL)
  }
  state <- unname(vcapply(inputs, identity))
  args <- paste(sprintf("%s = state$%s", names(inputs), state),
                collapse = ", ")
  sprintf("porcelain::porcelain_state(%s)", args)
}


roxy_process_returning <- function(returning, env, tag) {
  map <- c(
    json = "porcelain::porcelain_returning_json",
    binary = "porcelain::porcelain_returning_binary",
    generic = "porcelain::porcelain_returning")
  fn <- returning[[1]]
  if (!(fn %in% names(map))) {
    roxy_error(sprintf("Did not find returning function '%s'", fn),
               tag$file, tag$line)
  }

  args <- vcapply(returning[-1], deparse)
  i <- nzchar(names(args))
  if (any(i)) {
    args[i] <- sprintf("%s = %s", names(args)[i], args[i])
  }
  sprintf("%s(%s)", map[[fn]], paste(args, collapse = ", "))
}


roxy_output <- function(code, dest) {
  header <- "# Generated by porcelain: do not edit by hand"

  if (file.exists(dest)) {
    prev <- readLines(dest, n = 1)
    if (length(prev) == 0 || prev[[1]] != header) {
      stop("Not overwriting R/porcelain.R as it was not written by porcelain")
    }
  }

  writeLines(c(header, code), dest)
}
