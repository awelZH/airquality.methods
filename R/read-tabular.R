# Readers for tabular sources: opendata.swiss and local files.

#' Get the download links of an opendata.swiss dataset
#'
#' @param apiurl Package-show url of the CKAN API.
#' @param file_filter Substring the download url must contain.
#' @param useragent User agent sent with the request.
#'
#' @return Character vector of download urls.
#'
#' @keywords internal
get_opendataswiss_metadata <- function(apiurl,
                                       file_filter = ".csv",
                                       useragent = "Amt f\u00fcr Abfall, Wasser, Energie und Luft, Kanton Z\u00fcrich") {
  metadata <- httr2::request(apiurl) |>
    httr2::req_user_agent(useragent) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("result")

  links <- metadata |>
    purrr::pluck("resources", .default = list()) |>
    purrr::map_chr(\(resource) resource[["download_url"]] %||% NA_character_) |>
    purrr::discard(is.na)

  matching <- links[stringr::str_detect(links, stringr::fixed(file_filter))]
  if (length(matching) == 0) {
    cli::cli_abort(c(
      "x" = "No resource of {.url {apiurl}} matches {.val {file_filter}}.",
      "i" = "Available: {.val {utils::head(links, 6)}}"
    ))
  }

  matching
}

#' Read a dataset published on opendata.swiss
#'
#' All matching resources are read and row-bound, which is what the historised
#' cantonal datasets need: one resource per submission.
#'
#' @param url Package-show url of the CKAN API.
#' @param source Value written into the `source` column, for provenance.
#' @param file_filter Substring the download url must contain.
#'
#' @return A tibble with an added `source` column.
#'
#' @examplesIf interactive()
#' read_opendataswiss(
#'   "https://ckan.opendata.swiss/api/3/action/package_show?id=luftschadstoffemissionen-im-kanton-zurich",
#'   source = "Ostluft & BAFU"
#' )
#'
#' @export
read_opendataswiss <- function(url, source, file_filter = ".csv") {
  read_url <- get_opendataswiss_metadata(url, file_filter)

  read_url |>
    purrr::map(\(x) readr::read_delim(x, delim = ",", show_col_types = FALSE)) |>
    purrr::list_rbind() |>
    dplyr::mutate(source = source)
}

#' Read a local delimited file
#'
#' Defaults match the cantonal exports: semicolon separated, latin1 encoded,
#' timestamps in UTC+1.
#'
#' @param file Path to the file.
#' @param delim Field separator.
#' @param locale Locale, see [readr::locale()].
#' @param ... Further arguments passed to [readr::read_delim()].
#'
#' @return A tibble.
#'
#' @export
read_local_csv <- function(file,
                           delim = ";",
                           locale = readr::locale(encoding = "latin1", tz = "Etc/GMT-1"),
                           ...) {
  readr::read_delim(file, delim = delim, locale = locale, ...)
}
