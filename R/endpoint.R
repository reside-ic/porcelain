pkgapi_endpoint <- R6::R6Class(
  "pkgapi_endpoint",

  public = list(
    methods = NULL,
    path = NULL,
    target = NULL,
    validate = NULL,

    initialize = function(methods, path, target, validate = FALSE) {
      self$methods <- methods
      self$path <- path
      self$target <- target
      self$validate <- validate
    },

    run = function(...) {
      tryCatch(
        self$process(self$target(...)),
        error = pkgapi_process_error)
    },

    plumber = function(req, res, ...) {
      self$run(...)
    }
  ))


pkgapi_endpoint_json <- R6::R6Class(
  "pkgapi_endpoint_json",
  inherit = pkgapi_endpoint,

  public = list(
    content_type = "application/json",
    schema = NULL,
    validator = NULL,

    initialize = function(methods, path, target, schema = NULL, root = NULL,
                          validate = FALSE) {
      super$initialize(methods, path, target, validate)

      ## TODO: we will have to do some tricks here to get the package
      ## root on initialisation, or somewhat lazily.  Otherwise we
      ## will get warnings in packages and that's not ideal.
      ## Something that can be sorted out later though.  First use of
      ## the validator seems like a nice way to do it.
      self$schema <- schema
      self$validator <- pkgapi_validator(schema, schema_root(root, self$target))
    },

    process = function(data) {
      value <- response_success(data)
      body <- to_json_string(value)
      pkgapi_validate(body, self$validator, self$validate)
      pkgapi_response(200L, self$content_type, body,
                      data = data, value = value)
    }
  ))


pkgapi_endpoint_binary <- R6::R6Class(
  "pkgapi_endpoint_binary",
  inherit = pkgapi_endpoint,

  public = list(
    content_type = "application/octet-stream",

    process = function(data) {
      if (self$validate) {
        assert_is(data, "raw")
      }
      pkgapi_response(200, "application/octet-stream", data)
    }
  ))
