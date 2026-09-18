# Development tooling that renv cannot discover from the package source, because
# it is invoked from the console rather than called in R/ or tests/. Listing it
# here keeps `renv::restore()` enough to run everything: tests, coverage,
# documentation and the vignettes.
library(covr)
library(roxygen2)
