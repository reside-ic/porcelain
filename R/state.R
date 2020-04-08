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
