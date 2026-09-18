# Downloading assets into a persistent, checksum-validated cache and reading
# tabular assets from it.

#' Default cache directory
#'
#' Persists between R sessions, so repeated analyses run offline.
#'
#' @return Path to the cache directory.
#'
#' @export
geo_admin_cache_dir <- function() {
  tools::R_user_dir("airquality.methods", which = "cache")
}

#' Checksum of an asset
#'
#' STAC v1 uses `file:checksum`, v0.9 used `checksum:multihash`.
#'
#' @param asset A one-row data frame from [get_geo_admin_assets()].
#'
#' @return The multihash string, or `NA_character_`.
#'
#' @keywords internal
asset_checksum <- function(asset) {
  intersect(c("file:checksum", "checksum:multihash"), names(asset)) |>
    purrr::map_chr(\(field) as.character(asset[[field]])) |>
    purrr::discard(is.na) |>
    dplyr::first(default = NA_character_)
}

#' Validate a file against a multihash checksum
#'
#' Only SHA2-256 (multihash prefix `"1220"`) is verified. A missing checksum or
#' one using a different hash function counts as valid, because an unverifiable
#' file is not evidence of a corrupt one.
#'
#' @param path Local file path.
#' @param multihash Multihash string, or `NA`.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @keywords internal
checksum_matches <- function(path, multihash) {
  if (is.na(multihash) || !stringr::str_starts(tolower(multihash), "1220")) {
    return(TRUE)
  }
  actual <- unclass(as.character(openssl::sha256(file(path)))) # hex string, no hash class

  isTRUE(actual == tolower(stringr::str_sub(multihash, 5)))
}

#' Extract the matching member from a zip archive
#'
#' Paths that are not archives are returned unchanged.
#'
#' @param path Local path of the archive.
#' @param format Format of the member to extract.
#' @param call Environment used for the error call.
#'
#' @return Path of the extracted file.
#'
#' @keywords internal
extract_zip_member <- function(path, format, call = rlang::caller_env()) {
  if (!stringr::str_detect(tolower(path), "\\.zip$")) {
    return(path)
  }
  members <- utils::unzip(path, list = TRUE)$Name
  target <- members[asset_format(members) %in% format]
  if (length(target) != 1) {
    cli::cli_abort(
      "{.file {basename(path)}} holds {length(target)} file{?s} of format {.val {format}}, expected exactly one.",
      call = call
    )
  }

  exdir <- stringr::str_remove(path, stringr::regex("\\.zip$", ignore_case = TRUE))
  out <- file.path(exdir, target)
  if (!file.exists(out)) utils::unzip(path, files = target, exdir = exdir)

  out
}

#' Fetch a url into a local file
#'
#' The single point where this package touches the network for downloads, kept
#' separate so it can be stubbed in tests.
#'
#' @param href Source url.
#' @param path Destination path.
#' @param max_tries Number of attempts before giving up.
#'
#' @return `path`, invisibly.
#'
#' @keywords internal
fetch_to_file <- function(href, path, max_tries = 5) {
  httr2::request(href) |>
    httr2::req_user_agent(geo_admin_user_agent) |>
    httr2::req_retry(max_tries = max_tries) |>
    httr2::req_perform(path = path)

  invisible(path)
}

#' Download an asset and cache it locally
#'
#' Existing files are reused as long as their checksum matches. The download
#' lands in a `.part` file first, so an interrupted transfer can never be
#' mistaken for a valid cache entry.
#'
#' @param asset A single row from [get_geo_admin_assets()].
#' @param cache_dir Target directory; the server's path structure is mirrored
#'   inside it.
#' @param overwrite Force a fresh download.
#'
#' @return Local path, or the path of the extracted file for zip assets.
#'
#' @export
download_geo_admin_asset <- function(asset, cache_dir = geo_admin_cache_dir(), overwrite = FALSE) {
  asset <- check_single_asset(asset)
  href <- asset$href
  checksum <- asset_checksum(asset)

  relative_path <- href |>
    stringr::str_remove("[?#].*$") |>
    stringr::str_remove("^https?://[^/]+/")
  dest <- file.path(cache_dir, relative_path)

  if (overwrite || !file.exists(dest) || !checksum_matches(dest, checksum)) {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    part <- paste0(dest, ".part")
    on.exit(unlink(part), add = TRUE)

    cli::cli_inform("Downloading {.file {basename(dest)}}.")
    fetch_to_file(href, part)

    if (!checksum_matches(part, checksum)) {
      cli::cli_abort("Checksum mismatch for {.url {href}}.")
    }
    if (!file.rename(part, dest)) {
      cli::cli_abort("Could not write {.file {dest}}.")
    }
  }

  extract_zip_member(dest, asset$format)
}


# ---- Reading tabular assets -----------------------------------------------

#' Guess the field separator of a delimited file
#'
#' @param path Local file path.
#' @param candidates Separators to consider, most likely first.
#'
#' @return The separator as a single character.
#'
#' @keywords internal
guess_delim <- function(path, candidates = c(";", ",", "\t", "|")) {
  header <- readr::read_lines(path, n_max = 1)
  counts <- purrr::map_int(candidates, \(delim) stringr::str_count(header, stringr::fixed(delim)))

  if (all(counts == 0)) "," else candidates[[which.max(counts)]]
}

#' Column names of a parquet file
#'
#' Read from the schema, without loading any data.
#'
#' @param path Local file path.
#'
#' @return Character vector of column names.
#'
#' @keywords internal
parquet_column_names <- function(path) {
  rlang::check_installed("arrow", reason = "to read parquet files.")

  arrow::open_dataset(path)$schema$names
}

#' Read selected columns of a parquet file
#'
#' @param path Local file path.
#' @param columns Columns to read.
#'
#' @return A data frame.
#'
#' @keywords internal
read_parquet_columns <- function(path, columns) {
  rlang::check_installed("arrow", reason = "to read parquet files.")

  arrow::read_parquet(path, col_select = tidyselect::all_of(columns))
}

#' Read a tabular asset
#'
#' @param path Local file path.
#' @param format `"parquet"` or `"csv"`.
#' @param columns Columns to read (`NULL` = all). Missing columns abort with the
#'   list of available ones.
#'
#' @return A tibble.
#'
#' @keywords internal
read_asset_table <- function(path, format, columns = NULL) {
  delim <- if (format == "csv") guess_delim(path)

  available <- switch(format,
    parquet = parquet_column_names(path),
    csv     = names(readr::read_delim(path, delim = delim, n_max = 0, show_col_types = FALSE)),
    cli::cli_abort("Table format {.val {format}} is not supported (parquet, csv).")
  )
  columns <- unique(columns %||% available)
  check_names(available, columns, basename(path))

  data <- switch(format,
    parquet = read_parquet_columns(path, columns),
    csv = readr::read_delim(
      path,
      delim = delim,
      col_select = tidyselect::all_of(columns),
      show_col_types = FALSE,
      progress = FALSE
    )
  )

  tibble::as_tibble(data)
}
