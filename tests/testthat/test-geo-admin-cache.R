sha256_hex <- function(path) as.character(openssl::sha256(file(path)))

make_asset <- function(href, format = "csv", checksum = NA_character_) {
  tibble::tibble(
    asset = basename(href), href = href, format = format,
    `file:checksum` = checksum
  )
}

test_that("geo_admin_cache_dir is a stable per-user path", {
  expect_type(geo_admin_cache_dir(), "character")
  expect_length(geo_admin_cache_dir(), 1)
  expect_match(geo_admin_cache_dir(), "airquality.methods")
})

test_that("asset_checksum finds the field of either STAC version", {
  expect_equal(asset_checksum(tibble::tibble(`file:checksum` = "1220ab")), "1220ab")
  expect_equal(asset_checksum(tibble::tibble(`checksum:multihash` = "1220cd")), "1220cd")
  expect_true(is.na(asset_checksum(tibble::tibble(href = "x"))))
  expect_true(is.na(asset_checksum(tibble::tibble(`file:checksum` = NA_character_))))
})

test_that("checksum_matches validates sha2-256 multihashes", {
  path <- withr::local_tempfile(lines = "hello world")
  digest <- sha256_hex(path)

  expect_true(checksum_matches(path, paste0("1220", digest)))
  expect_true(checksum_matches(path, toupper(paste0("1220", digest))))
  expect_false(checksum_matches(path, paste0("1220", strrep("0", 64))))
})

test_that("checksum_matches passes files it cannot check", {
  path <- withr::local_tempfile(lines = "hello world")

  # missing checksum, or a hash function other than sha2-256
  expect_true(checksum_matches(path, NA_character_))
  expect_true(checksum_matches(path, "1114deadbeef"))
})

test_that("guess_delim reads the separator off the header line", {
  semi <- withr::local_tempfile(lines = c("a;b;c", "1;2;3"))
  comma <- withr::local_tempfile(lines = c("a,b,c", "1,2,3"))
  tabbed <- withr::local_tempfile(lines = c("a\tb\tc", "1\t2\t3"))
  single <- withr::local_tempfile(lines = c("a", "1"))

  expect_equal(guess_delim(semi), ";")
  expect_equal(guess_delim(comma), ",")
  expect_equal(guess_delim(tabbed), "\t")
  expect_equal(guess_delim(single), ",")
})

test_that("read_asset_table reads csv and selects columns", {
  path <- withr::local_tempfile(lines = c("E_KOORD;N_KOORD;BBTOT;OTHER", "2600000;1200000;5;9"))

  all_cols <- read_asset_table(path, "csv")
  expect_s3_class(all_cols, "tbl_df")
  expect_named(all_cols, c("E_KOORD", "N_KOORD", "BBTOT", "OTHER"))

  subset <- read_asset_table(path, "csv", columns = c("E_KOORD", "N_KOORD", "BBTOT"))
  expect_named(subset, c("E_KOORD", "N_KOORD", "BBTOT"))
  expect_equal(subset$BBTOT, 5)
})

test_that("read_asset_table names the missing columns and the available ones", {
  path <- withr::local_tempfile(lines = c("E_KOORD;N_KOORD;BBTOT", "2600000;1200000;5"))

  expect_error(read_asset_table(path, "csv", columns = "NOPE"), "NOPE")
  expect_error(read_asset_table(path, "csv", columns = "NOPE"), "BBTOT")
})

test_that("read_asset_table rejects formats it cannot read", {
  path <- withr::local_tempfile(lines = "x")

  expect_error(read_asset_table(path, "gpkg"), "not supported")
})

test_that("extract_zip_member unpacks the single matching file", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "inner.csv")
  writeLines(c("a;b", "1;2"), csv)
  zip <- file.path(dir, "outer.zip")
  withr::with_dir(dir, utils::zip(zip, "inner.csv", flags = "-q"))
  skip_if_not(file.exists(zip), "zip utility unavailable")

  out <- extract_zip_member(zip, "csv")

  expect_true(file.exists(out))
  expect_equal(basename(out), "inner.csv")
  expect_equal(readLines(out)[[1]], "a;b")
})

test_that("extract_zip_member leaves non-archives alone", {
  path <- withr::local_tempfile(fileext = ".csv", lines = "a;b")

  expect_equal(extract_zip_member(path, "csv"), path)
})

test_that("download_geo_admin_asset mirrors the server path under the cache", {
  cache <- withr::local_tempdir()
  local_mocked_bindings(
    fetch_to_file = function(href, path, ...) writeLines("a;b", path)
  )

  out <- download_geo_admin_asset(
    make_asset("https://data.geo.admin.ch/ch.test/2020/data.csv"),
    cache_dir = cache
  )

  expect_equal(out, file.path(cache, "ch.test/2020/data.csv"))
  expect_true(file.exists(out))
})

test_that("download_geo_admin_asset reuses a cached file without fetching again", {
  cache <- withr::local_tempdir()
  calls <- 0L
  local_mocked_bindings(
    fetch_to_file = function(href, path, ...) {
      calls <<- calls + 1L
      writeLines("a;b", path)
    }
  )
  asset <- make_asset("https://data.geo.admin.ch/ch.test/data.csv")

  download_geo_admin_asset(asset, cache_dir = cache)
  download_geo_admin_asset(asset, cache_dir = cache)

  expect_equal(calls, 1L)
})

test_that("download_geo_admin_asset refetches when the cached checksum is wrong", {
  cache <- withr::local_tempdir()
  calls <- 0L
  local_mocked_bindings(
    fetch_to_file = function(href, path, ...) {
      calls <<- calls + 1L
      writeLines("correct", path)
    }
  )
  asset <- make_asset("https://data.geo.admin.ch/ch.test/data.csv")

  dest <- file.path(cache, "ch.test/data.csv")
  dir.create(dirname(dest), recursive = TRUE)
  writeLines("stale", dest)

  # the expectation has to be written exactly the way the mock writes it,
  # otherwise the line endings alone make the hashes differ
  reference <- withr::local_tempfile()
  writeLines("correct", reference)
  asset$`file:checksum` <- paste0("1220", sha256_hex(reference))

  download_geo_admin_asset(asset, cache_dir = cache)

  expect_equal(calls, 1L)
  expect_equal(readLines(dest), "correct")
})

test_that("download_geo_admin_asset aborts and leaves no cache entry on a bad checksum", {
  cache <- withr::local_tempdir()
  local_mocked_bindings(
    fetch_to_file = function(href, path, ...) writeLines("corrupt", path)
  )
  asset <- make_asset(
    "https://data.geo.admin.ch/ch.test/data.csv",
    checksum = paste0("1220", strrep("0", 64))
  )

  expect_error(download_geo_admin_asset(asset, cache_dir = cache), "hecksum")
  # neither the destination nor the partial download may survive
  expect_false(file.exists(file.path(cache, "ch.test/data.csv")))
  expect_false(file.exists(file.path(cache, "ch.test/data.csv.part")))
})

test_that("download_geo_admin_asset strips query strings from the cache path", {
  cache <- withr::local_tempdir()
  local_mocked_bindings(
    fetch_to_file = function(href, path, ...) writeLines("a;b", path)
  )

  out <- download_geo_admin_asset(
    make_asset("https://data.geo.admin.ch/ch.test/data.csv?token=secret"),
    cache_dir = cache
  )

  expect_equal(out, file.path(cache, "ch.test/data.csv"))
})
