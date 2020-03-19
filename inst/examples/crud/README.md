# CRUD

This package contains an example CRUD impl using pkgapi

Can test this out by running

```
api <- build_api()
api$run(swagger = FALSE)
```

Then sending http requests to localhost e.g. via curl

Create
```
curl -X POST -H 'Content-Type: application/json' --data @inst/examples/add_book_payload.json http://localhost:6279/books
```

Read
```
curl http://localhost:6279/book/The%20Picture%20of%20Dorian%20Gray
```

```
curl http://localhost:6279/book/The%20Picture%20of%20Dorian%20Gray?details=author
```
