##' While porcelain makes it easy to test endpoints individually, you
##' may still want some integration or end-to-end tests where you
##' bring the entire API up and interact with it from your tests. This
##' class provides a helper for doing this in a way that is reasonably
##' tidy.
porcelain_background <- R6::R6Class(
  "porcelain_background",
  cloneable = FALSE,

  private = list(
    create = NULL,
    args = NULL,
    process = NULL,

    server_is_up = function() {
      !isTRUE(tryCatch(httr::GET(self$url("/")), error = function(e) TRUE))
    },

    server_is_alive = function() {
      !is.null(private$process) && private$process$is_alive()
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
    initialize = function(create, args = NULL, port = NULL, log = NULL) {
      ## The callr and httr packages are required for running this, so
      ## make fail fast if they're not available.
      loadNamespace("callr")
      loadNamespace("httr")

      private$create <- create
      private$args <- args
      self$port <- port %||% free_port(8000, 10000)
      self$log <- log %||% tempfile()

      ## make fields read-only on object creation:
      lockBinding("port", self)
      lockBinding("log", self)
    },

    ##' @description Start the server. It is an error to try and start
    ##' a server that is already running.
    ##'
    ##' @param timeout The number of seconds to wait for the server
    ##'   to become available. This needs to cover the time taken to spawn
    ##'   the R process, and create your API object (loading all packages
    ##'   needed) up to the point where the server is responsive. In most
    ##'   cases this will take 1-2s but if you use packages that use many
    ##'   S4 methods or run this on a slow computer (e.g., a continuous
    ##'   integration server) it may take longer than you expect.
    ##'
    ##' @param verbose Logical, indicating if we should print information
    ##'   about connection attempts while testing if the server has come
    ##'   up.
    start = function(timeout = 60, verbose = FALSE) {
      if (!is.null(private$process)) {
        stop("Server already running")
      }

      private$process <- callr::r_bg(
        function(create, args, port) {
          api <- do.call(create$fn, create$args)
          api$run("127.0.0.1", port)
        },
        args = list(create = private$create, port = self$port),
        stdout = self$log, stderr = self$log)

      wait_until(private$server_is_up,
                 timeout = timeout, verbose = verbose,
                 title = "Waiting for server to become responsive")
      ## TODO: on failure, read the logs?
      invisible(self)
    },

    ##' @description Return the background server status. This will be one of:
    ##'
    ##' * `running`: The server is running
    ##' * `stopped`: The server is stopped
    ##' * `blocked`: The server is stopped, but something else is running
    ##'    on the port that we would use
    status = function() {
      ## NOTE: There is an additional possibility - process alive but
      ## responsive, but we do not expect to see that.
      if (private$server_is_alive()) {
        "running"
      } else if (private$server_is_up()) {
        "blocked"
      } else {
        "stopped"
      }
    },

    ##' @description Stop a running server. If the server is not running,
    ##' this has no effect.
    stop = function() {
      if (private$server_is_alive()) {
        message("Stopping server")
        private$process$kill()
        private$process <- NULL
      }
      invisible(self)
    },

    ##' @description Create a url string for the server, interpolating the
    ##'   (possibly random) port number. You can use this in your tests
    ##'   like `bg$url("/path")` or `bg$path("/path/%s/status", element)`
    url = function(path, ...) {
      if (...length() > 0) {
        path <- sprintf(path, ...)
      }
      sprintf("http://localhost:%d%s", self$port, path)
    },

    ##' @description Run a request to the server, using `httr`. This presents
    ##'   a similar inteface to the `request` method on the porcelain object.
    request = function(method, path, ...) {
      httr::VERB(method, self$url(path), ...)
    }
  ))
