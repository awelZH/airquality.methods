#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom rlang %||% .data .env :=
#'
#' Loading `stars` and `sf` here is not cosmetic. Both register S3 methods for
#' each other's generics (`st_normalize.stars`, `st_crop.stars`, ...) in their
#' `.onLoad()`. Referring to them only as `stars::fun()` would leave their
#' namespaces unloaded until the first such call, so a `stars` object arriving
#' from elsewhere — `readRDS()`, a caller that never touched `stars` itself —
#' would hit an unregistered method and fail with "no applicable method".
#' @importFrom sf st_bbox
#' @importFrom stars st_dimensions
## usethis namespace: end
NULL
