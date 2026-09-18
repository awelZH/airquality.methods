# STAC-API data.geo.admin.ch: Assets abfragen, auswählen und als stars einlesen
#
# Reihenfolge beim Laden: geo_admin_stac.R -> statpop.R -> geo_admin_cube.R
#
# Abhängigkeiten:
#   httr2 (>= 1.0.0), purrr (>= 1.0.0), dplyr (>= 1.1.0), tibble, vctrs, stringr,
#   readr, tidyselect, rlang, cli, openssl, sf, stars, arrow (Parquet)

if (getRversion() < "4.4.0") `%||%` <- rlang::`%||%`

geo_admin_user_agent <- "r-geoadmin-stac-client"

#' Standard-Ausschnitt: Kanton Zürich in LV95 (auf 5 km nach aussen gerundet)
#'
#' Für den exakten Kantonsumriss kann jedes sf-Objekt als `bbox` übergeben werden
#' (z. B. aus swissBOUNDARIES3D); verwendet wird dessen Ausdehnung.
bbox_zh_lv95 <- c(xmin = 2665000, ymin = 1220000, xmax = 2720000, ymax = 1285000)

#' Ganze Schweiz in LV95 (auf 10 km gerundet, mit Rand)
bbox_ch_lv95 <- c(xmin = 2480000, ymin = 1070000, xmax = 2840000, ymax = 1300000)


# ---- Validierung ---------------------------------------------------------------

#' Prüfen, ob alle benötigten Spalten vorhanden sind
check_names <- function(available, required, what = "den Daten", call = rlang::caller_env()) {
  missing <- setdiff(required, available)
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(length(missing))}Spalte{?n} {.val {missing}} fehl{?t/en} in {what}.",
        "i" = "Vorhanden: {.val {available}}"
      ),
      call = call
    )
  }
  invisible(TRUE)
}

#' Sicherstellen, dass genau ein Asset (eine Zeile) übergeben wurde
check_single_asset <- function(asset, call = rlang::caller_env()) {
  if (!is.data.frame(asset) || nrow(asset) != 1) {
    cli::cli_abort("{.arg asset} muss genau eine Zeile aus {.fn get_geo_admin_assets} sein.", call = call)
  }
  check_names(names(asset), c("asset", "href", "format"), "asset", call = call)
  asset
}

#' KBS eines Assets (Feld proj:epsg) mit dem erwarteten KBS vergleichen
check_asset_crs <- function(asset, crs, call = rlang::caller_env()) {
  epsg <- asset[["proj:epsg"]]
  if (!is.null(epsg) && !is.na(epsg) && sf::st_crs(as.integer(epsg)) != sf::st_crs(crs)) {
    cli::cli_abort(
      c(
        "x" = "Asset {.val {asset$asset}} liegt in EPSG:{epsg} vor, erwartet: {sf::st_crs(crs)$input}.",
        "i" = "Assets vorher filtern, z. B. {.code dplyr::filter(`proj:epsg` == 2056)}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}


# ---- Bounding Box ----------------------------------------------------------------

#' Bounding Box vereinheitlichen
#'
#' @param bbox NULL, ein numerischer Vektor c(xmin, ymin, xmax, ymax) im KBS `crs`
#'   (Standard LV95) oder ein Objekt mit eigenem KBS (sf, sfc, bbox, stars), das
#'   bei Bedarf nach `crs` transformiert wird.
#' @param crs  Ziel-KBS (EPSG-Code oder `sf::st_crs()`-Objekt).
#' @return `sf::st_bbox()`-Objekt in `crs` oder `NULL`.
as_bbox <- function(bbox, crs = 2056, call = rlang::caller_env()) {
  if (is.null(bbox)) {
    return(NULL)
  }
  crs <- sf::st_crs(crs)

  if (inherits(bbox, c("sf", "sfc", "bbox", "stars"))) {
    bb <- sf::st_bbox(bbox)
    if (is.na(sf::st_crs(bb))) attr(bb, "crs") <- crs
    return(if (sf::st_crs(bb) == crs) bb else transform_bbox(bb, crs))
  }

  if (!is.numeric(bbox) || length(bbox) != 4 || anyNA(bbox)) {
    cli::cli_abort("{.arg bbox} muss ein numerischer Vektor c(xmin, ymin, xmax, ymax) sein.", call = call)
  }
  if (!is.null(names(bbox))) {
    check_names(names(bbox), c("xmin", "ymin", "xmax", "ymax"), "bbox", call = call)
    bbox <- bbox[c("xmin", "ymin", "xmax", "ymax")]
  }
  if (!isTRUE(sf::st_is_longlat(crs)) && all(abs(bbox) <= 360)) {
    cli::cli_abort(
      c(
        "x" = "{.arg bbox} sieht nach Grad (WGS84) aus, erwartet werden Koordinaten in {crs$input}.",
        "i" = "Beispiel LV95: {.code c(2590000, 1190000, 2610000, 1210000)}"
      ),
      call = call
    )
  }

  bb <- sf::st_bbox(stats::setNames(as.numeric(bbox), c("xmin", "ymin", "xmax", "ymax")), crs = crs)
  if (bb[["xmin"]] >= bb[["xmax"]] || bb[["ymin"]] >= bb[["ymax"]]) {
    cli::cli_abort("{.arg bbox} ist ungültig: xmin < xmax und ymin < ymax erforderlich.", call = call)
  }
  bb
}

#' Bounding Box in ein anderes KBS transformieren
#'
#' Die Kanten werden verdichtet, damit die transformierte Box die gekrümmten
#' Kanten vollständig umschliesst (nicht nur die vier Eckpunkte).
transform_bbox <- function(bbox, crs, n = 50) {
  xs <- seq(bbox[["xmin"]], bbox[["xmax"]], length.out = n)
  ys <- seq(bbox[["ymin"]], bbox[["ymax"]], length.out = n)
  edges <- rbind(
    cbind(xs, bbox[["ymin"]]), cbind(xs, bbox[["ymax"]]),
    cbind(bbox[["xmin"]], ys), cbind(bbox[["xmax"]], ys)
  )

  sf::st_multipoint(edges) |>
    sf::st_sfc(crs = sf::st_crs(bbox)) |>
    sf::st_transform(crs) |>
    sf::st_bbox()
}


# ---- Request -------------------------------------------------------------------

#' Request für die Items einer STAC-Collection aufbauen
#'
#' @param collection   Collection-ID, z. B. "ch.bfs.statistik-bevoelkerung_haushalte".
#' @param bbox         Optional: Bounding Box, standardmässig in LV95 (siehe `as_bbox()`).
#'                     Die STAC-API erwartet WGS84, die Umrechnung erfolgt intern.
#' @param bbox_crs     KBS eines numerischen `bbox`-Vektors.
#' @param datetime     Optional: RFC-3339-Zeitpunkt oder -Intervall, z. B. "2020-01-01T00:00:00Z/..".
#' @param limit        Anzahl Items pro Seite.
#' @param stac_version API-Version ("v1" empfohlen, "v0.9" ist deprecated).
#' @param base_url     Basis-URL der STAC-API.
geo_admin_items_request <- function(collection,
                                    bbox = NULL,
                                    bbox_crs = 2056,
                                    datetime = NULL,
                                    limit = 100,
                                    stac_version = c("v1", "v0.9"),
                                    base_url = "https://data.geo.admin.ch/api/stac") {
  stac_version <- rlang::arg_match(stac_version)
  if (!rlang::is_string(collection)) {
    cli::cli_abort("{.arg collection} muss ein einzelner String sein.")
  }

  bbox_query <- NULL
  if (!is.null(bbox)) {
    bb <- transform_bbox(as_bbox(bbox, bbox_crs), 4326)
    # nach aussen runden, damit die Box nicht schrumpft
    bbox_query <- paste(c(floor(bb[1:2] * 1e6), ceiling(bb[3:4] * 1e6)) / 1e6, collapse = ",")
  }

  httr2::request(base_url) |>
    httr2::req_url_path_append(stac_version, "collections", collection, "items") |>
    httr2::req_url_query(bbox = bbox_query, datetime = datetime, limit = limit) |>
    httr2::req_user_agent(geo_admin_user_agent) |>
    httr2::req_retry(max_tries = 5)
}


# ---- Paginierung ---------------------------------------------------------------

#' Nächste Seite anhand des STAC-Links mit rel = "next" (für req_perform_iterative)
next_stac_page <- function(resp, req) {
  next_href <- resp |>
    httr2::resp_body_json() |>
    purrr::pluck("links", .default = list()) |>
    purrr::keep(\(link) identical(link[["rel"]], "next")) |>
    purrr::map_chr("href")

  if (length(next_href) == 0) {
    return(NULL)
  }
  httr2::req_url(req, next_href[[1]])
}


# ---- Parsing -------------------------------------------------------------------

#' Assets eines STAC-Items in eine Tabelle umwandeln (alle skalaren Felder)
parse_item_assets <- function(item) {
  assets <- item[["assets"]]
  if (length(assets) == 0) {
    return(NULL)
  }
  props <- item[["properties"]]

  assets |>
    purrr::map(\(asset) {
      asset |>
        purrr::keep(\(field) is.atomic(field) && length(field) == 1) |>
        tibble::as_tibble()
    }) |>
    purrr::list_rbind(names_to = "asset") |>
    tibble::add_column(
      collection     = item[["collection"]] %||% NA_character_,
      item           = item[["id"]],
      datetime       = props[["datetime"]] %||% NA_character_,
      start_datetime = props[["start_datetime"]] %||% NA_character_,
      end_datetime   = props[["end_datetime"]] %||% NA_character_,
      .before = 1
    )
}

#' Eine Seite der API-Antwort in eine Asset-Tabelle umwandeln
parse_stac_page <- function(resp) {
  resp |>
    httr2::resp_body_json() |>
    purrr::pluck("features", .default = list()) |>
    purrr::map(parse_item_assets) |>
    purrr::list_rbind()
}

#' Dateiformat aus der URL ableiten (".csv.zip" -> "csv", ".tiff" -> "tif")
asset_format <- function(href) {
  file <- href |>
    stringr::str_remove("[?#].*$") |>
    basename() |>
    tolower() |>
    stringr::str_remove("\\.(zip|gz)$")
  ext <- stringr::str_extract(file, "(?<=\\.)[a-z0-9]+$")
  # Varianten vereinheitlichen (ohne case_match(), das in dplyr 1.2.0 deprecated ist)
  ext[ext %in% c("tiff", "geotiff")] <- "tif"
  ext
}

#' Bezugsjahr eines Items bestimmen
#'
#' Vorrang hat eine eindeutige Jahreszahl in der Item-ID (z. B. "..._2020"), sonst
#' das Jahr aus datetime, end_datetime oder start_datetime. Vierstellige Zahlen
#' nach dem laufenden Jahr (z. B. EPSG 2056) gelten nicht als Jahr.
item_year <- function(item, datetime, start_datetime, end_datetime) {
  max_year <- as.integer(format(Sys.Date(), "%Y")) + 1L

  from_id <- purrr::map_int(item, \(id) {
    candidates <- as.integer(stringr::str_extract_all(id, "(?<!\\d)(19|20)\\d{2}(?!\\d)")[[1]])
    candidates <- unique(candidates[candidates <= max_year])
    if (length(candidates) == 1) candidates else NA_integer_
  })
  from_datetime <- dplyr::coalesce(datetime, end_datetime, start_datetime) |>
    stringr::str_sub(1, 4) |>
    as.integer()

  dplyr::coalesce(from_id, from_datetime)
}

#' Komprimierung aus der URL ableiten ("zip", "gz" oder NA)
asset_compression <- function(href) {
  href |>
    stringr::str_remove("[?#].*$") |>
    tolower() |>
    stringr::str_extract("(?<=\\.)(zip|gz)$")
}


# ---- Assets abfragen -----------------------------------------------------------

#' Alle Assets einer Collection von data.geo.admin.ch abfragen
#'
#' @param collection Collection-ID.
#' @param ...        Weitere Argumente an `geo_admin_items_request()`
#'                   (`bbox` in LV95, `datetime`, `limit`, `stac_version`, ...).
#' @param max_pages  Maximale Anzahl abgefragter Seiten.
#' @return Tibble mit einer Zeile pro Asset, inkl. `format` und `compression`.
get_geo_admin_assets <- function(collection, ..., max_pages = Inf) {
  req <- geo_admin_items_request(collection, ...)

  resps <- httr2::req_perform_iterative(req, next_req = next_stac_page, max_reqs = max_pages)

  last_resp <- resps[[length(resps)]]
  if (length(resps) >= max_pages && !is.null(next_stac_page(last_resp, req))) {
    cli::cli_warn("{.arg max_pages} ({max_pages}) erreicht, es gibt weitere Seiten. Das Ergebnis ist unvollständig.")
  }

  assets <- tibble::as_tibble(httr2::resps_data(resps, parse_stac_page))
  if (nrow(assets) == 0) {
    return(assets)
  }

  assets |>
    dplyr::mutate(
      year = item_year(.data$item, .data$datetime, .data$start_datetime, .data$end_datetime),
      .after = "item"
    ) |>
    dplyr::mutate(
      format      = asset_format(.data$href),
      compression = asset_compression(.data$href),
      .after = "asset"
    )
}


# ---- Asset-Auswahl -------------------------------------------------------------

#' Pro Gruppe (Standard: Item) das bevorzugte Dateiformat wählen
#'
#' Zweite Stufe der Filterhierarchie: Zuerst inhaltlich filtern (z. B. "_ha_2056"),
#' dann mit dieser Funktion pro Item das beste verfügbare Format behalten.
#'
#' @param assets  Tibble aus `get_geo_admin_assets()`.
#' @param formats Formate in absteigender Präferenz. Andere Formate werden verworfen.
#' @param by      Spalte(n), innerhalb derer die Präferenz gilt.
#' @return Gefilterte Assets. Gruppen ohne passendes Format lösen eine Warnung aus.
select_preferred_format <- function(assets, formats = c("parquet", "csv", "tif"), by = "item") {
  check_names(names(assets), c("format", by), "assets")

  selected <- assets |>
    dplyr::filter(.data$format %in% formats) |>
    dplyr::filter(
      match(.data$format, formats) == min(match(.data$format, formats)),
      .by = dplyr::all_of(by)
    )

  dropped <- assets |>
    dplyr::select(dplyr::all_of(by)) |>
    dplyr::distinct() |>
    dplyr::anti_join(selected, by = by)

  if (nrow(dropped) > 0) {
    cli::cli_warn(c(
      "!" = "{nrow(dropped)} Gruppe{?n} ohne Asset in den Formaten {.val {formats}} verworfen.",
      "i" = "Betroffen: {.val {dropped[[by[[1]]]]}}"
    ))
  }
  selected
}


#' Dateihierarchie auflösen: genau ein Asset pro Item (Jahr) bestimmen
#'
#' Stufen:
#' 1. Inhalt:        `pattern` (Regex auf den Asset-Namen, z. B. "_ha_")
#' 2. KBS:           `proj:epsg == epsg` (Assets ohne Angabe bleiben erhalten)
#' 3. Format:        pro Item das erste verfügbare Format aus `formats`
#' 4. Eindeutigkeit: bei mehreren Kandidaten Vorzug für Namen mit dem EPSG-Code,
#'                   sonst Abbruch mit Liste der Kandidaten
#' 5. Jahre:         nur `years`; nicht verfügbare Jahre werden gemeldet
#'
#' @return Tibble mit einer Zeile pro Item, nach Jahr sortiert.
resolve_assets <- function(assets,
                           pattern = NULL,
                           formats = c("tif", "parquet", "csv"),
                           epsg = 2056,
                           years = NULL) {
  check_names(names(assets), c("item", "year", "asset", "format"), "assets")
  label <- dplyr::first(assets[["collection"]], default = "assets")

  candidates <- assets
  if (!is.null(pattern)) {
    candidates <- dplyr::filter(candidates, stringr::str_detect(.data$asset, pattern))
  }
  if (!is.null(epsg) && "proj:epsg" %in% names(candidates)) {
    candidates <- dplyr::filter(candidates, is.na(.data$`proj:epsg`) | .data$`proj:epsg` == epsg)
  }
  candidates <- dplyr::filter(candidates, .data$format %in% formats)

  if (nrow(candidates) == 0) {
    cli::cli_abort(c(
      "x" = "{label}: kein Asset passt zu Muster {.val {pattern %||% '(keins)'}}, EPSG {epsg} und Formaten {.val {formats}}.",
      "i" = "Vorhandene Assets (Auszug): {.val {utils::head(unique(assets$asset), 6)}}"
    ))
  }

  resolved <- select_preferred_format(candidates, formats)

  if (!is.null(epsg)) {
    epsg_regex <- paste0("(?<!\\d)", epsg, "(?!\\d)")
    resolved <- resolved |>
      dplyr::mutate(.epsg_in_name = stringr::str_detect(.data$asset, epsg_regex)) |>
      dplyr::filter(.data$.epsg_in_name | !any(.data$.epsg_in_name), .by = "item") |>
      dplyr::select(-".epsg_in_name")
  }

  ambiguous <- dplyr::filter(resolved, dplyr::n() > 1, .by = "item")
  if (nrow(ambiguous) > 0) {
    cli::cli_abort(c(
      "x" = "{label}: mehrere Assets pro Item nach Auflösung der Dateihierarchie.",
      "i" = "Kandidaten: {.val {unique(ambiguous$asset)}}",
      "i" = "{.arg pattern} präzisieren, z. B. in der Collection-Vorgabe."
    ))
  }

  if (!is.null(years)) {
    available <- sort(unique(stats::na.omit(resolved$year)))
    missing_years <- setdiff(years, available)
    if (length(missing_years) > 0) {
      cli::cli_inform(c(
        "i" = "{label}: nicht verfügbar: {paste(missing_years, collapse = ', ')}",
        " " = "verfügbar: {paste(available, collapse = ', ')}"
      ))
    }
    resolved <- dplyr::filter(resolved, .data$year %in% years)
  }

  dplyr::arrange(resolved, .data$year)
}


# ---- Download & Cache ----------------------------------------------------------

#' Standard-Cache-Verzeichnis (bleibt zwischen R-Sessions erhalten)
geo_admin_cache_dir <- function() {
  tools::R_user_dir("geoadmin-stac", which = "cache")
}

#' Prüfsumme eines Assets (v1: file:checksum, v0.9: checksum:multihash)
asset_checksum <- function(asset) {
  intersect(c("file:checksum", "checksum:multihash"), names(asset)) |>
    purrr::map_chr(\(field) as.character(asset[[field]])) |>
    purrr::discard(is.na) |>
    dplyr::first(default = NA_character_)
}

#' Datei gegen eine Multihash-Prüfsumme validieren
#'
#' Geprüft wird nur SHA2-256 (Präfix "1220"). Fehlt die Prüfsumme oder hat sie ein
#' anderes Format, gilt die Datei als gültig.
checksum_matches <- function(path, multihash) {
  if (is.na(multihash) || !stringr::str_starts(tolower(multihash), "1220")) {
    return(TRUE)
  }
  actual <- unclass(as.character(openssl::sha256(file(path)))) # Hex-String ohne hash-Klasse
  isTRUE(actual == tolower(stringr::str_sub(multihash, 5)))
}

#' Passende Datei aus einem ZIP-Archiv extrahieren (sonst Pfad unverändert)
extract_zip_member <- function(path, format, call = rlang::caller_env()) {
  if (!stringr::str_detect(tolower(path), "\\.zip$")) {
    return(path)
  }
  members <- utils::unzip(path, list = TRUE)$Name
  target <- members[asset_format(members) %in% format]
  if (length(target) != 1) {
    cli::cli_abort(
      "{.file {basename(path)}} enthält {length(target)} Datei{?en} im Format {.val {format}}, erwartet genau eine.",
      call = call
    )
  }

  exdir <- stringr::str_remove(path, stringr::regex("\\.zip$", ignore_case = TRUE))
  out <- file.path(exdir, target)
  if (!file.exists(out)) utils::unzip(path, files = target, exdir = exdir)
  out
}

#' Asset herunterladen und lokal cachen
#'
#' Bereits vorhandene Dateien werden wiederverwendet, sofern die Prüfsumme passt.
#' Der Download landet zuerst in einer .part-Datei, damit abgebrochene Downloads
#' nie als gültiger Cache gelten.
#'
#' @param asset     Eine Zeile aus `get_geo_admin_assets()`.
#' @param cache_dir Zielverzeichnis. Die Serverstruktur wird darin gespiegelt.
#' @param overwrite Download erzwingen.
#' @return Lokaler Pfad (bei ZIP: Pfad der extrahierten Datei).
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

    cli::cli_inform("Lade {.file {basename(dest)}} herunter.")
    httr2::request(href) |>
      httr2::req_user_agent(geo_admin_user_agent) |>
      httr2::req_retry(max_tries = 5) |>
      httr2::req_perform(path = part)

    if (!checksum_matches(part, checksum)) {
      cli::cli_abort("Prüfsumme stimmt nicht für {.url {href}}.")
    }
    if (!file.rename(part, dest)) {
      cli::cli_abort("Konnte {.file {dest}} nicht schreiben.")
    }
  }

  extract_zip_member(dest, asset$format)
}


# ---- Tabellen lesen ------------------------------------------------------------

#' Trennzeichen einer CSV-Datei anhand der Kopfzeile bestimmen
guess_delim <- function(path, candidates = c(";", ",", "\t", "|")) {
  header <- readr::read_lines(path, n_max = 1)
  counts <- purrr::map_int(candidates, \(delim) stringr::str_count(header, stringr::fixed(delim)))
  if (all(counts == 0)) "," else candidates[[which.max(counts)]]
}

#' Spaltennamen einer Parquet-Datei aus dem Schema lesen (ohne Daten zu laden)
parquet_column_names <- function(path) {
  rlang::check_installed("arrow", reason = "zum Lesen von Parquet-Dateien.")
  arrow::open_dataset(path)$schema$names
}

#' Nur ausgewählte Spalten einer Parquet-Datei lesen
read_parquet_columns <- function(path, columns) {
  rlang::check_installed("arrow", reason = "zum Lesen von Parquet-Dateien.")
  arrow::read_parquet(path, col_select = tidyselect::all_of(columns))
}

#' Tabellarisches Asset (parquet, csv) lesen, optional nur ausgewählte Spalten
#'
#' @param path    Lokaler Dateipfad.
#' @param format  "parquet" oder "csv".
#' @param columns Zu lesende Spalten (`NULL` = alle). Fehlende Spalten führen zu
#'   einem Fehler mit Liste der vorhandenen Spalten.
read_asset_table <- function(path, format, columns = NULL) {
  delim <- if (format == "csv") guess_delim(path)

  available <- switch(format,
    parquet = parquet_column_names(path),
    csv     = names(readr::read_delim(path, delim = delim, n_max = 0, show_col_types = FALSE)),
    cli::cli_abort("Tabellenformat {.val {format}} wird nicht unterstützt (parquet, csv).")
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


# ---- Tabelle -> Raster ---------------------------------------------------------

#' Punkt-/Zelltabelle auf einem regelmässigen Raster in ein stars-Objekt umwandeln
#'
#' Die Zellen werden direkt über ihren Index befüllt (kein Rasterisieren über
#' Geometrien). Das ist exakt, schnell und prüft, ob die Koordinaten wirklich auf
#' einem regelmässigen Raster liegen.
#'
#' @param data      Tabelle mit Koordinatenspalten und Werten.
#' @param variables Spalten, die zu Attributen werden (`NULL` = alle numerischen
#'   Spalten ausser den Koordinaten).
#' @param coords    Namen der Koordinatenspalten (Ost, Nord).
#' @param cellsize  Zellgrösse in KBS-Einheiten.
#' @param anchor    Bezugspunkt der Koordinaten: linke untere Ecke (BFS-Hektarraster)
#'   oder Zellmittelpunkt.
#' @param crs       KBS der Koordinaten.
#' @param bbox      Optional: Ausschnitt (siehe `as_bbox()`). Wird nach aussen auf
#'   das Raster eingerastet. Mit einer festen bbox haben alle Ergebnisse dasselbe
#'   Gitter und lassen sich kombinieren.
table_to_stars <- function(data,
                           variables = NULL,
                           coords = c("E_KOORD", "N_KOORD"),
                           cellsize = 100,
                           anchor = c("lowerleft", "center"),
                           crs = 2056,
                           bbox = NULL) {
  anchor <- rlang::arg_match(anchor)
  check_names(names(data), coords, "data")
  variables <- variables %||% setdiff(names(data)[purrr::map_lgl(data, is.numeric)], coords)
  check_names(names(data), variables, "data")
  if (nrow(data) == 0 && is.null(bbox)) {
    cli::cli_abort("Keine Daten und keine {.arg bbox}: Rasterausdehnung ist unbestimmt.")
  }

  # Linke untere Zellecke
  shift <- if (anchor == "center") cellsize / 2 else 0
  x0 <- data[[coords[[1]]]] - shift
  y0 <- data[[coords[[2]]]] - shift

  # Rasterursprung ableiten und prüfen, ob alle Koordinaten auf dem Raster liegen
  origin <- if (nrow(data) > 0) c(x0[[1]], y0[[1]]) %% cellsize else c(0, 0)
  steps_x <- (x0 - origin[[1]]) / cellsize
  steps_y <- (y0 - origin[[2]]) / cellsize
  off_grid <- abs(steps_x - round(steps_x)) > 1e-6 | abs(steps_y - round(steps_y)) > 1e-6
  if (any(off_grid)) {
    cli::cli_abort(c(
      "x" = "{sum(off_grid)} Koordinate{?n} lieg{?t/en} nicht auf dem {cellsize}-m-Raster.",
      "i" = "{.arg cellsize}, {.arg anchor} und {.arg coords} prüfen."
    ))
  }

  if (is.null(bbox)) {
    ext <- c(xmin = min(x0), ymin = min(y0), xmax = max(x0) + cellsize, ymax = max(y0) + cellsize)
  } else {
    bb <- as_bbox(bbox, crs)
    snap <- function(value, o, fun) o + fun((value - o) / cellsize) * cellsize
    ext <- c(
      xmin = snap(bb[["xmin"]], origin[[1]], floor),
      ymin = snap(bb[["ymin"]], origin[[2]], floor),
      xmax = snap(bb[["xmax"]], origin[[1]], ceiling),
      ymax = snap(bb[["ymax"]], origin[[2]], ceiling)
    )
    keep <- x0 >= ext[["xmin"]] & x0 < ext[["xmax"]] & y0 >= ext[["ymin"]] & y0 < ext[["ymax"]]
    data <- data[keep, ]
    x0 <- x0[keep]
    y0 <- y0[keep]
  }

  nx <- as.integer(round((ext[["xmax"]] - ext[["xmin"]]) / cellsize))
  ny <- as.integer(round((ext[["ymax"]] - ext[["ymin"]]) / cellsize))
  idx <- cbind(
    as.integer(round((x0 - ext[["xmin"]]) / cellsize)) + 1L,
    as.integer(round((ext[["ymax"]] - y0) / cellsize))
  )

  if (anyDuplicated(idx) > 0) {
    cli::cli_abort(c(
      "x" = "Mehrere Zeilen pro Rasterzelle gefunden.",
      "i" = "Enthält die Tabelle mehrere Jahre oder Varianten? Vorher filtern oder aggregieren."
    ))
  }

  layers <- purrr::map(rlang::set_names(variables), \(variable) {
    values <- data[[variable]]
    layer <- array(values[NA_integer_], dim = c(nx, ny)) # NA im Typ der Spalte
    layer[idx] <- values
    layer
  })

  grid <- stars::st_as_stars(sf::st_bbox(ext, crs = sf::st_crs(crs)), nx = nx, ny = ny, values = NA)
  stars::st_as_stars(layers, dimensions = stars::st_dimensions(grid))
}


# ---- Asset -> stars ------------------------------------------------------------

#' GeoTIFF als stars lesen (per /vsicurl/ gestreamt, nur der bbox-Ausschnitt)
read_tif_stars <- function(asset, variables, bbox, crs, cache_dir) {
  compression <- asset[["compression"]] %||% NA_character_
  source <- if (is.na(compression)) {
    paste0("/vsicurl/", asset$href)
  } else {
    download_geo_admin_asset(asset, cache_dir)
  }

  x <- stars::read_stars(source, proxy = TRUE)
  if (sf::st_crs(x) != sf::st_crs(crs)) {
    cli::cli_abort("{.val {asset$asset}} hat nicht das KBS {sf::st_crs(crs)$input}.")
  }
  if (!is.null(bbox)) x <- sf::st_crop(x, bbox)
  x <- stars::st_as_stars(x)

  if ("band" %in% names(stars::st_dimensions(x))) x <- split(x, "band")
  if (is.null(variables)) {
    return(x)
  }

  if (length(x) == 1 && length(variables) == 1 && !variables %in% names(x)) {
    cli::cli_inform("Einband-GeoTIFF {.val {asset$asset}}: Attribut wird als {.val {variables}} benannt.")
    names(x) <- variables
  }
  check_names(names(x), variables, paste("den Bändern von", asset$asset))
  x[variables]
}

#' Ein Asset als georeferenziertes stars-Objekt einlesen
#'
#' GeoTIFFs werden direkt gelesen; parquet/csv (Hektarraster-Tabellen) werden
#' heruntergeladen, gecacht und über `table_to_stars()` gerastert.
#'
#' @param asset     Eine Zeile aus `get_geo_admin_assets()`.
#' @param variables Attribute bzw. Bänder (`NULL` = alle). Bei grossen Ausschnitten
#'   nur benötigte Variablen wählen: CH-weit 100 m sind ~8 Mio. Zellen pro Variable.
#' @param bbox      Ausschnitt, numerisch in LV95 oder als sf/bbox/stars-Objekt.
#' @param coords,cellsize,anchor Siehe `table_to_stars()` (nur Tabellenformate).
#' @param crs       Erwartetes KBS.
#' @param cache_dir Cache-Verzeichnis für Downloads.
read_asset_stars <- function(asset,
                             variables = NULL,
                             bbox = NULL,
                             coords = c("E_KOORD", "N_KOORD"),
                             cellsize = 100,
                             anchor = c("lowerleft", "center"),
                             crs = 2056,
                             cache_dir = geo_admin_cache_dir()) {
  asset <- check_single_asset(asset)
  check_asset_crs(asset, crs)
  bbox <- as_bbox(bbox, crs)

  x <- switch(asset$format,
    tif = read_tif_stars(asset, variables, bbox, crs, cache_dir),
    parquet = ,
    csv = {
      columns <- if (!is.null(variables)) c(coords, variables)
      download_geo_admin_asset(asset, cache_dir) |>
        read_asset_table(asset$format, columns) |>
        table_to_stars(variables, coords, cellsize, anchor, crs, bbox)
    },
    cli::cli_abort(c(
      "x" = "Format {.val {asset$format}} kann nicht als Raster gelesen werden.",
      "i" = "Unterstützt: tif, parquet, csv. Vektordaten (gpkg) mit {.fn sf::read_sf} lesen."
    ))
  )

  # Einheitliche Dimensionen (from = 1), damit Gitter direkt vergleichbar sind
  sf::st_normalize(x)
}


#' Rasterdaten einer Collection pro Jahr einlesen
#'
#' Löst die Dateihierarchie mit `resolve_assets()` auf und liest jedes Jahr als
#' stars-Objekt ein. Einbändige Raster erhalten den Attributnamen `name`, damit
#' alle Jahre gleich benannt sind.
#'
#' @param assets    Tibble aus `get_geo_admin_assets()`.
#' @param years     Gewünschte Jahre (`NULL` = alle verfügbaren).
#' @param bbox      Ausschnitt (Standard: Kanton Zürich).
#' @param pattern,formats,epsg Siehe `resolve_assets()`.
#' @param variables Bänder bzw. Tabellenspalten (`NULL` = alle).
#' @param name      Attributname für einbändige Raster (Standard: aus der Collection-ID).
#' @param ...       Weitere Argumente an `read_asset_stars()`.
#' @return Tibble: item, year, format, href, stars (Listen-Spalte).
read_geo_admin_rasters <- function(assets,
                                   years = NULL,
                                   bbox = bbox_zh_lv95,
                                   pattern = NULL,
                                   formats = c("tif", "parquet", "csv"),
                                   variables = NULL,
                                   name = NULL,
                                   epsg = 2056,
                                   cache_dir = geo_admin_cache_dir(),
                                   ...) {
  resolved <- resolve_assets(assets, pattern, formats, epsg, years)
  name <- name %||% collection_label(dplyr::first(assets[["collection"]], default = "value"))

  rows <- resolved |>
    vctrs::vec_chop() |>
    purrr::map(\(asset) {
      cli::cli_inform("Lese {.val {asset$item}} ({asset$format}).")
      x <- read_asset_stars(asset, variables = variables, bbox = bbox, crs = epsg, cache_dir = cache_dir, ...)
      if (length(x) == 1 && is.null(variables)) names(x) <- name

      tibble::tibble(
        item   = asset$item,
        year   = asset$year,
        format = asset$format,
        href   = asset$href,
        stars  = list(x)
      )
    })

  if (length(rows) == 0) {
    return(tibble::tibble(item = character(), year = integer(), format = character(), href = character(), stars = list()))
  }
  purrr::list_rbind(rows)
}

#' Kurzbezeichnung aus einer Collection-ID ("ch.bafu.luftreinhaltung-x_y" -> "luftreinhaltung_x_y")
collection_label <- function(collection) {
  collection |>
    stringr::str_remove("^ch\\.[^.]+\\.") |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_")
}


# ---- Beispiele -----------------------------------------------------------------

if (FALSE) {
  # Übersicht über Varianten und Jahre einer Collection
  pm25_assets <- get_geo_admin_assets("ch.bafu.luftreinhaltung-feinstaub_pm2_5")
  dplyr::count(pm25_assets, year, format)

  # Dateihierarchie automatisch auflösen (ein Asset pro Jahr) ...
  resolve_assets(pm25_assets, formats = "tif", years = 2020:2024)

  # ... und direkt einlesen: Kanton Zürich, nur verfügbare Jahre
  pm25 <- read_geo_admin_rasters(pm25_assets, years = 2020:2024, formats = "tif", name = "pm2_5")
  pm25
  plot(pm25$stars[[1]])
}
