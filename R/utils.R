# Shared internal helpers.

#' Check that all required names are present
#'
#' Validation at the function boundary: fails with the missing names and, as a
#' hint, the names that are actually available. Useful for inputs from online
#' sources, whose structure can change between two runs of an analysis.
#'
#' @param available Names found in the object, e.g. `names(data)`.
#' @param required Names the object must have.
#' @param what Short description of the object, used in the message.
#' @param class Additional class of the error condition, so that callers can
#'   catch or test input errors specifically; `NULL` for none.
#' @param call Environment used for the error call, see [rlang::caller_env()].
#'
#' @return `TRUE`, invisibly. Aborts if names are missing.
#'
#' @examples
#' check_names(names(mtcars), c("mpg", "cyl"), "mtcars")
#' try(check_names(names(mtcars), "speed", "mtcars", class = "my_input_error"))
#'
#' @export
check_names <- function(available, required, what = "the data", class = NULL, call = rlang::caller_env()) {
  missing <- setdiff(required, available)

  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(length(missing))}Column{?s} {.val {missing}} {?is/are} missing from {what}.",
        "i" = "Available: {.val {available}}"
      ),
      class = class,
      call = call
    )
  }

  invisible(TRUE)
}


#' Round half away from zero
#'
#' Commercial rounding ("kaufmännisches Runden"). Base R's [round()] rounds half
#' to even, so `round(0.5)` is `0` and `round(2.5)` is `2`. For reported figures
#' the expected behaviour is to round halves up in magnitude, symmetrically for
#' positive and negative values.
#'
#' A decimal literal such as `1.005` is not exactly representable in binary (it
#' is stored as `1.00499999...`), so a naive implementation rounds it down and
#' contradicts what the printed number suggests. A tolerance of
#' `sqrt(.Machine$double.eps)` absorbs that representation error, which is the
#' behaviour expected of commercial rounding.
#'
#' @param x Numeric vector.
#' @param digits Number of decimal places.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' round(c(0.5, 1.5, 2.5))     # base R, half to even: 0 2 2
#' round_off(c(0.5, 1.5, 2.5)) # half away from zero:  1 2 3
#'
#' round(2.675, 2)     # 2.67, because 2.675 is stored as 2.67499999...
#' round_off(2.675, 2) # 2.68
#'
#' @export
round_off <- function(x, digits = 0) {
  scale <- 10^digits

  sign(x) * floor(abs(x) * scale + 0.5 + sqrt(.Machine$double.eps)) / scale
}


#' Write a delimited text file
#'
#' Thin wrapper around [readr::write_delim()] with the defaults used for the
#' compiled output datasets (semicolon separated). Missing directories are
#' created. With `append = TRUE` the rows are added without a header, unless
#' the file does not exist yet: then it is created with a header, so a log
#' file can be written by always appending.
#'
#' @param data Data frame to write.
#' @param file Target path.
#' @param delim Field separator.
#' @param na String used for missing values.
#' @param append Append to an existing file instead of overwriting it.
#'
#' @return `data`, invisibly.
#'
#' @export
write_local_csv <- function(data, file, delim = ";", na = "NA", append = FALSE) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  col_names <- !append || !file.exists(file)
  readr::write_delim(data, file, delim = delim, na = na, append = append, col_names = col_names)

  invisible(data)
}


#' Drop dimension names from an array
#'
#' `stars` carries named dimensions on the underlying arrays. When arrays from
#' different sources are recombined the names must match exactly, so they are
#' stripped before reassembly.
#'
#' @param values An array.
#'
#' @return `values` with unnamed `dim`.
#'
#' @keywords internal
unname_dim <- function(values) {
  dim(values) <- unname(dim(values))

  values
}
