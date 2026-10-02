# Readers for tabular sources: opendata.swiss and local files.

#' List the resources of an opendata.swiss dataset
#'
#' The version of each resource as the CKAN API describes it, so that a pipeline can check it on every
#' run and download the data only when it changed. opendata.swiss publishes no checksum (`hash` is
#' empty), so the modification time and the size stand for the version.
#'
#' @param apiurl Package-show url of the CKAN API.
#' @param useragent User agent sent with the request; opendata.swiss refuses requests without one.
#'
#' @return Tibble with one row per resource that has a download url, in the order of the API:
#'   `download_url`, `format`, `modified` (character, as published), `byte_size` (numeric), `id`
#'   (the CKAN resource id) and `name` (the resource title, in German where the API has it, else
#'   English, French or Italian); `NA` where the API leaves a field out. The name is what tells
#'   resources apart -- the download urls of the cantonal datasets are opaque
#'   (`KTZH_00003175_00006879.parquet`) -- see [select_opendataswiss_resource()].
#'
#' @examplesIf interactive()
#' api <- "https://ckan.opendata.swiss/api/3/action/package_show"
#' get_opendataswiss_resources(paste0(api, "?id=luftschadstoffemissionen-im-kanton-zurich"))
#'
#' @export
get_opendataswiss_resources <- function(apiurl, useragent = "Amt f\u00fcr Abfall, Wasser, Energie und Luft, Kanton Z\u00fcrich") {
  resources <- httr2::request(apiurl) |>
    httr2::req_user_agent(useragent) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("result", "resources", .default = list()) |>
    purrr::keep(\(resource) !is.null(resource[["download_url"]]))

  field <- function(name, missing) {
    purrr::map_vec(resources, \(resource) resource[[name]] %||% missing, .ptype = missing)
  }

  tibble::tibble(
    download_url = field("download_url", NA_character_),
    format = field("format", NA_character_),
    modified = field("modified", NA_character_),
    byte_size = field("byte_size", NA_real_),
    id = field("id", NA_character_),
    name = purrr::map_chr(resources, \(resource) opendataswiss_text(resource[["name"]] %||% resource[["title"]]))
  )
}

#' The text of a multilingual CKAN field
#'
#' opendata.swiss returns titles as `list(de = , en = , fr = , it = )`; German first, because the
#' cantonal datasets are written in German, then the others in that order.
#'
#' @param x A string, a list of strings by language, or `NULL`.
#'
#' @return One string, or `NA_character_`.
#'
#' @keywords internal
opendataswiss_text <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.character(x)) return(if (length(x) && nzchar(x[[1]])) x[[1]] else NA_character_)
  for (lang in c("de", "en", "fr", "it")) {
    value <- x[[lang]]
    if (is.character(value) && length(value) == 1 && nzchar(value)) return(value)
  }
  NA_character_
}

#' Pick one resource of an opendata.swiss dataset by its name
#'
#' A dataset holds several files -- data, metadata, a description -- whose urls say nothing about
#' their content. This picks the one whose name matches `pattern` and refuses anything else: no
#' match and several matches are both errors that list every resource, so a renamed or an added
#' resource stops the pipeline instead of reading the wrong file.
#'
#' @param resources Tibble from [get_opendataswiss_resources()].
#' @param pattern Regular expression matched against `name`, ignoring case.
#' @param format Accepted formats (e.g. `"PARQUET"`), ignoring case; `NULL` accepts any.
#'
#' @return The matching row of `resources`.
#'
#' @examplesIf interactive()
#' api <- "https://ckan.opendata.swiss/api/3/action/package_show"
#' res <- get_opendataswiss_resources(paste0(api, "?id=messdaten-zu-ultrafeinen-partikeln-in-der-region-kloten"))
#' select_opendataswiss_resource(res, "^Messdaten", format = "parquet")
#'
#' @export
select_opendataswiss_resource <- function(resources, pattern, format = NULL) {
  if (!is.character(pattern) || length(pattern) != 1 || is.na(pattern)) {
    cli::cli_abort("{.arg pattern} must be a single regular expression.")
  }
  hit <- stringr::str_detect(resources$name, stringr::regex(pattern, ignore_case = TRUE))
  hit[is.na(hit)] <- FALSE
  if (!is.null(format)) hit <- hit & toupper(resources$format) %in% toupper(format)

  if (sum(hit) != 1) {
    available <- paste0(resources$name, " (", dplyr::coalesce(dplyr::na_if(resources$format, ""), "?"), ")")
    in_format <- if (is.null(format)) "" else paste0(" in format ", paste(format, collapse = "/"))
    cli::cli_abort(c(
      "x" = "{sum(hit)} resource{?s} match{?es/} {.val {pattern}}{in_format}; expected exactly one.",
      "i" = "Available: {.val {available}}"
    ))
  }
  resources[hit, ]
}

#' Download a resource of an opendata.swiss dataset, only when it changed
#'
#' The resource is stored under its own file name in `cache_dir`, with a version file beside it
#' that holds the `modified` time and the `byte_size` the API reported. A later call downloads only
#' if the API reports a different version -- opendata.swiss publishes no checksum, so these two
#' stand for one. The download goes to a temporary file first and replaces the cached one only when
#' it is complete; where the API states a size, a file of another size is refused.
#'
#' @param resource One row from [get_opendataswiss_resources()] or
#'   [select_opendataswiss_resource()].
#' @param cache_dir Directory to keep the files in.
#'
#' @return A one-row tibble: `path` (the local file), `downloaded` (`TRUE` if it was fetched by
#'   this call), `version` (as stored) and `name`.
#'
#' @examplesIf interactive()
#' api <- "https://ckan.opendata.swiss/api/3/action/package_show"
#' res <- get_opendataswiss_resources(paste0(api, "?id=messdaten-zu-ultrafeinen-partikeln-in-der-region-kloten"))
#' download_opendataswiss_resource(select_opendataswiss_resource(res, "Messorten"), tempdir())
#'
#' @export
download_opendataswiss_resource <- function(resource, cache_dir) {
  if (!is.data.frame(resource) || nrow(resource) != 1) {
    cli::cli_abort("{.arg resource} must be a single row of {.fn get_opendataswiss_resources}.")
  }
  url <- resource$download_url
  path <- file.path(cache_dir, basename(stringr::str_remove(url, "[?#].*$")))
  version_file <- paste0(path, ".version")
  version <- paste0("modified: ", resource$modified, "; byte_size: ", format(resource$byte_size, scientific = FALSE))

  current <- file.exists(path) && file.exists(version_file) &&
    identical(readLines(version_file, warn = FALSE), version)
  if (!current) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    part <- paste0(path, ".part")
    on.exit(unlink(part), add = TRUE)
    fetch_to_file(url, part)
    if (!is.na(resource$byte_size) && file.size(part) != resource$byte_size) {
      cli::cli_abort(c(
        "x" = "{.url {url}} arrived with {file.size(part)} bytes, the API states {resource$byte_size}.",
        "i" = "The cached file, if any, is left as it was."
      ))
    }
    unlink(path)
    file.rename(part, path)
    writeLines(version, version_file)
  }

  tibble::tibble(path = path, downloaded = !current, version = version, name = resource$name)
}

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
  links <- get_opendataswiss_resources(apiurl, useragent)$download_url

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
