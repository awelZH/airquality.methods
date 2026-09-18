test_that("the package imports sf and stars, not just calls them", {
  # stars and sf register S3 methods for each other's generics when their
  # namespaces load. Without a real import directive those namespaces stay
  # unloaded until the first stars::/sf:: call, and a stars object arriving from
  # readRDS() or from a caller that never touched stars hits an unregistered
  # method. A `stars::fun()` call alone does not prevent that.
  imports <- parseNamespaceFile("airquality.methods", dirname(system.file(package = "airquality.methods")))$imports
  imported_packages <- purrr::map_chr(imports, \(entry) if (is.list(entry)) entry[[1]] else entry)

  expect_contains(imported_packages, c("sf", "stars"))
})

test_that("both namespaces are loaded once the package is", {
  expect_contains(loadedNamespaces(), c("sf", "stars"))
})
