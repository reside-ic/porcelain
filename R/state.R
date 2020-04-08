##' Bind state into an endpoint
##'
##' This method allows state to be bound to the target function.  Each
##' element of \code{...} (or \code{.state}) is named with the
##' argument to the target function being bound, and the value is the
##' value that argument will take.  Once bound, the arguments to the
##' target function may not be provided by an input.
##'
##' The primary use case for this is to bind mutable state (database
##' connections, etc) that may be shared amongst different endpoints
##' within an API.
##'
##' @title Bind state into an endpoint
##'
##' @param ... Named arguments representing state to bind; see
##'   \code{Details}.
##'
##' @param .state A list of named state to bind, instead
##'   of using \code{...} - this interface is considerably easier to
##'   program against if building an API programmatically, avoiding
##'   the use of \code{\link{do.call}}.
##'
##' @export
pkgapi_state <- function(..., .state = list(...)) {
  assert_named(.state, TRUE)
  pkgapi_state_collection$new(.state)
}


pkgapi_state_collection <- R6::R6Class(
  "pkgapi_state",
  public = list(
    names = NULL,
    state = NULL,
    initialize = function(state) {
      self$names <- names(state)
      self$state <- state
    },

    bind = function(target) {
      args <- formals(target)
      err <- setdiff(self$names, names(args))
      if (length(err) > 0L) {
        stop(sprintf(
          "Argument %s (used in state) missing from the target function",
          paste(squote(err), collapse = ", ")))
      }

      bind_args(target, self$state)
    }
  ))
