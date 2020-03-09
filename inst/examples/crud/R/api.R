build_api <- function() {
  create_response <- pkgapi::pkgapi_returning_json(
    "create_response.schema",
    system.file("schema", package = "crud")
  )
  book_input <- pkgapi::pkgapi_input_body_json("book", "book_input.schema",
                                       system.file("schema", package = "crud"))
  create <- pkgapi::pkgapi_endpoint$new("POST",
                                        "/books",
                                        add_book,
                                        returning = create_response,
                                        input_body = book_input,
                                        validate = TRUE)

  read_response <- pkgapi::pkgapi_returning_json(
    "read_response.schema",
    system.file("schema", package = "crud")
  )
  read_input <- pkgapi::pkgapi_input_query(details = "string")
  read <- pkgapi::pkgapi_endpoint$new("GET",
                                      "/books/<title:string>",
                                      get_book_details,
                                      returning = read_response,
                                      input_query = read_input,
                                      validate = TRUE)
  api <- pkgapi::pkgapi$new()
  api$handle(create)
  api$handle(read)
  api
}
