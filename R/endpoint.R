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
      tryCatch({
        data <- self$target(...)
        body <- self$process(data)
        if (self$validate) {
          self$validate_response(body)
        }
        pkgapi_response(200, self$content_type, body, data = data)
      }, error = pkgapi_process_error)
    },

    process = function(data) {
      data
    },

    validate_response = function(body) {
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
      self$schema <- schema
      self$validator <- pkgapi_validator(schema, schema_root(root, target))
    },

    process = function(data) {
      to_json_string(response_success(data))
    },

    validate_response = function(body) {
      pkgapi_validate(body, self$validator)
    }
  ))


pkgapi_endpoint_binary <- R6::R6Class(
  "pkgapi_endpoint_binary",
  inherit = pkgapi_endpoint,

  public = list(
    content_type = "application/octet-stream",

    validate_response = function(body) {
      assert_is(body, "raw")
    }
  ))
