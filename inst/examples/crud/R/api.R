build_api <- function() {
  api <- pkgapi::pkgapi$new()
  api$handle(create_endpoint())
  api$handle(details_endpoint())
  api
}

create_endpoint <- function() {
  ## TODO: Shouldn't have to paste root here but it isn't picking up the
  ## schema directory automratically
  root <- system.file("schema", package = "crud")
  create_response <- pkgapi::pkgapi_returning_json("create_response", root)
  book_input <- pkgapi::pkgapi_input_body_json("book", "book_input", root)
  create <- pkgapi::pkgapi_endpoint$new("POST",
                                        "/books",
                                        add_book,
                                        returning = create_response,
                                        input_body = book_input,
                                        validate = TRUE)
}

details_endpoint <- function() {
  root <- system.file("schema", package = "crud")
  read_response <- pkgapi::pkgapi_returning_json("read_response", root)
  read_input <- pkgapi::pkgapi_input_query(details = "string")
  ## TODO: RESIDE-116 Use same path as the create endpoint i.e./books/<title>
  ## at the moment this will throw an error on printing
  read <- pkgapi::pkgapi_endpoint$new("GET",
                                      "/book/<title>",
                                      get_book_details,
                                      returning = read_response,
                                      input_query = read_input,
                                      validate = TRUE)
}
