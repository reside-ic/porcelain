##' While porcelain makes it easy to test endpoints individually, you
##' may still want some integration or end-to-end tests where you
##' bring the entire API up and interact with it from your tests. This
##' class provides a helper for doing this in a way that is reasonably
##' tidy.
##' @export
porcelain_background <- R6::R6Class(
  "porcelain_background",
  cloneable = FALSE,

  private = list(
    verbose = NULL,
    timeout = NULL,
    env = NULL,
    create = NULL,
    args = NULL,
    process = NULL,
    path_src = NULL,

    server_is_responsive = function() {
      !isTRUE(tryCatch(httr::GET(self$url("/")), error = function(e) TRUE))
    },

    server_is_alive = function() {
      !is.null(private$process) && private$process$is_alive()
    },

    server_is_responsive_and_alive = function() {
      if (!private$server_is_alive()) {
        stop("server is not running (process has exited)")
      }
      private$server_is_responsive()
    },

    finalize = function() {
      self$stop()
    }
  ),

  public = list(
    ##' @field log The path to the log file (read-only)
    log = NULL,

    ##' @field port The port used by the background server (read-only)
    port = NULL,

    ##' @description Create a background server object
    ##'
    ##' @param create A function that will create an api object
    ##'
    ##' @param args Arguments that will be passed to `create` when creating
    ##'   the api object in the background process
    ##'
    ##' @param port The port to use for the background server.
    ##'   If not given then a random free port will be used in the range
    ##'   8000 to 10000 - you can find the created port using the `port`
    ##'   field in the resulting object, or use the `$url()` or `$request()`
    ##'   methods.
    ##'
    ##' @param log The path to a log file to use
    ##'
    ##' @param verbose Logical, indicating if we should print informational
    ##'  messages to the console on start/stop etc.
    ##'
    ##' @param timeout The number of seconds to wait for the server
    ##'   to become available. This needs to cover the time taken to spawn
    ##'   the R process, and create your API object (loading all packages
    ##'   needed) up to the point where the server is responsive. In most
    ##'   cases this will take 1-2s but if you use packages that use many
    ##'   S4 methods or run this on a slow computer (e.g., a continuous
    ##'   integration server) it may take longer than you expect.  The
    ##'   default is one minute which should be sufficient in almost all
    ##'   cases.
    ##'
    ##' @param env A named character vector of environment variables (e.g.,
    ##'   `c(VARIABLE = "value")`) to set in the background process before
    ##'   launching the server. You can use this to control the behaviour of
    ##'   the background server using variables your api recognises. In
    ##'   addition, we export `callr::rcmd_safe_env()` and the value of
    ##'   `PORCELAIN_VALIDATE`.
    initialize = function(create, args = NULL, port = NULL, log = NULL,
                          verbose = FALSE, timeout = 60, env = NULL) {
      ## The callr and httr packages are required for running this, so
      ## make fail fast if they're not available.
      loadNamespace("callr")
      loadNamespace("httr")

      ## TODO: some validation would be useful here, on all these.
      private$create <- create
      private$args <- args %||% list()
      self$port <- port %||% free_port(8000, 10000)
      self$log <- log %||% tempfile()
      private$verbose <- verbose
      private$timeout <- timeout
      private$env <- env

      ## We might want to make this more tunable in case this
      ## detection fails; the general solution would be a vector of
      ## package names to check, rather than relying on
      ## the package name found in environment(create)
      pkg <- packageName(environment(private$create))
      if (is_pkgload_package(pkg)) {
        if (private$verbose) {
          message(sprintf("Using development version of '%s' via pkgload", pkg))
        }
        private$path_src <- find.package(pkg)
      }

      if (private$verbose) {
        message(sprintf("Using port %s", self$port))
      }

      ## make fields read-only on object creation:
      lockBinding("port", self)
      lockBinding("log", self)
    },

    ##' @description Start the server. It is an error to try and start
    ##'   a server that is already running.
    start = function() {
      ## NOTE: we ignore the 'starting' process possibility here, it
      ## should not be possible to trigger.
      status <- self$status()
      if (status == "running") {
        stop("Server already running")
      }
      if (status == "blocked") {
        stop(sprintf("Port '%d' is already in use", self$port))
      }

      private$process <- r_bg_with_hook(
        function(create, args, port) {
          do.call(create, args)$run("127.0.0.1", port)
        },
        args = list(create = private$create,
                    args = private$args,
                    port = self$port),
        stdout = self$log,
        stderr = self$log,
        user_hook = background_user_hook(private$path_src),
        user_env = private$env)

      tryCatch(
        wait_until(private$server_is_responsive_and_alive,
                   timeout = private$timeout,
                   verbose = private$verbose,
                   title = "Waiting for server to become responsive"),
        error = function(e) {
          process <- private$process
          private$process <- NULL # ensure always removed on failure
          porcelain_background_error(e, process, self$log)
        })

      invisible(self)
    },

    ##' @description Return the background server status. This will be one of:
    ##'
    ##' * `running`: The server is running
    ##' * `stopped`: The server is stopped
    ##' * `blocked`: The server is stopped, but something else is running
    ##'    on the port that we would use
    ##' * `starting`: The server is starting up (not visible in normal usage)
    status = function() {
      is_responsive <- private$server_is_responsive()
      is_alive <- private$server_is_alive()
      background_status_string(is_alive, is_responsive)
    },

    ##' @description Stop a running server. If the server is not running,
    ##' this has no effect.
    stop = function() {
      if (private$server_is_alive()) {
        if (private$verbose) {
          message("Stopping server")
        }
        private$process$kill()
        private$process <- NULL
      }
      invisible(self)
    },

    ##' @description Create a url string for the server, interpolating the
    ##'   (possibly random) port number. You can use this in your tests
    ##'   like `bg$url("/path")`
    ##'
    ##' @param path String representing the absolute path
    url = function(path) {
      sprintf("http://localhost:%d%s", self$port, path)
    },

    ##' @description Run a request to the server, using `httr`. This presents
    ##'   a similar inteface to the `request` method on the porcelain object.
    ##'
    ##' @param method The http method as a string (e.g., `"GET"`), passed
    ##'   to [httr::VERB] as the `verb` argument
    ##'
    ##' @param path String representing the absolute path, passed to `$url()`
    ##'
    ##' @param ... Additional arguments passed to `httr::VERB`, such as
    ##'   `query`, or the body for a `POST` request.
    request = function(method, path, ...) {
      httr::VERB(method, self$url(path), ...)
    }
  ))


## See callr:::default_load_hook for what looks like a part
## implemented version of an extendable hook; if that support is
## completed we can use that a little more easily, though there is
## also no explicit options interface to r_bg either; in any case this
## is straightforward enough.
r_bg_with_hook <- function(func, args = list(), stdout = "|", stderr = "|",
                           user_hook = NULL, user_env = NULL) {
  options <- callr::r_process_options(
    func = func,
    args = args,
    stdout = stdout,
    stderr = stderr,
    env = background_env(user_env))
  options$load_hook <- background_load_hook(options$load_hook, user_hook)
  callr::r_process$new(options = options)
}


background_user_hook <- function(path_src) {
  if (is.null(path_src)) {
    NULL
  } else {
    bquote(pkgload::load_all(
      .(path_src), export_all = FALSE, attach_testthat = FALSE))
  }
}


porcelain_background_error <- function(e, process, log) {
  if (process$is_alive()) {
    process$kill()
  } else {
    result <- tryCatch(process$get_result(), error = identity)
    if (inherits(result, "error")) {
      e <- result
    }
  }
  e$log <- read_lines_if_exists(log)
  class(e) <- c("porcelain_background_error", class(e))
  stop(e)
}


background_status_string <- function(is_alive, is_responsive) {
  if (is_alive) {
    if (is_responsive) "running" else "starting"
  } else {
    if (is_responsive) "blocked" else "stopped"
  }
}


background_env <- function(user_env) {
  c(callr::rcmd_safe_env(),
    PORCELAIN_VALIDATE = Sys.getenv("PORCELAIN_VALIDATE"),
    user_env)
}


background_load_hook <- function(load_hook, user_hook) {
  if (!is.null(user_hook)) {
    load_hook <- c(
      "{\n",
      paste0("  ", load_hook),
      paste0("  ", deparse(user_hook), "\n"),
      "}\n")
  }
  load_hook
}
