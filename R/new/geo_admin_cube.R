# Mehrere Collections einlesen, zu Würfeln stapeln und auf ein Referenzraster mitteln
#
# Reihenfolge beim Laden: geo_admin_stac.R -> statpop.R -> geo_admin_cube.R
#
# Ablauf:
#   1. read_geo_admin()      Collections x Jahre in Originalauflösung (Tibble, ein Raster pro Zeile)
#   2. stack_years()         verlustfreie Würfel: einer pro Collection und Gitter
#   3. optional auf ein gemeinsames Gitter normalisieren (STATPOP als Summe, Rest gemittelt):
#      a) align_to_reference()  pro Jahr auf das Gitter einer Referenz-Collection
#      b) align_to_grid()       auf ein frei vorgegebenes Raster (Datei, stars, make_reference_grid())
#   4. stack_years()         auf das Ergebnis von 3.: ein gemeinsamer Würfel (x, y, year)
#   5. Auswertung mit tidyverse-Logik direkt auf dem Würfel oder über as_tibble() und
#      tibble_to_cube() (filter() auf Würfeln braucht das Paket cubelyr)


# ---- Collection-Vorgaben -------------------------------------------------------

#' Vorgabe, wie eine Collection eingelesen wird
#'
#' @param reader Funktion mit der Schnittstelle
#'   `function(assets, years, bbox, cache_dir, ...)`, die ein Tibble mit mindestens
#'   `year` und `stars` zurückgibt, z. B. `read_geo_admin_rasters` oder `read_statpop_ha`.
#' @param label  Kurzname der Collection in den Ergebnissen.
#' @param resampling GDAL-Methode beim Normalisieren auf ein gemeinsames Gitter:
#'   "average" für Konzentrationen/Raten, "sum" für Zählgrössen (z. B. Einwohner).
#' @param ...    Feste Argumente für `reader` (z. B. `formats`, `pattern`, `name`).
collection_spec <- function(reader, label, resampling = "average", ...) {
  if (!is.function(reader)) cli::cli_abort("{.arg reader} muss eine Funktion sein.")
  if (!rlang::is_string(label)) cli::cli_abort("{.arg label} muss ein einzelner String sein.")
  if (!rlang::is_string(resampling)) cli::cli_abort("{.arg resampling} muss ein einzelner String sein.")
  list(reader = reader, label = label, resampling = resampling, args = list(...))
}

year_match_choices <- c("exact", "nearest", "linear")

check_year_match <- function(year_match, call = rlang::caller_env()) {
  invalid <- setdiff(year_match, year_match_choices)
  if (!is.character(year_match) || length(year_match) == 0 || length(invalid) > 0) {
    cli::cli_abort(
      "{.arg year_match} erlaubt nur {.val {year_match_choices}}, erhalten: {.val {invalid}}.",
      call = call
    )
  }
  invisible(TRUE)
}

#' Bekannte Collections
#'
#' Erweitern mit z. B.
#' `specs <- c(geo_admin_specs, list("ch.x.y" = collection_spec(read_geo_admin_rasters, "y", name = "y")))`
geo_admin_specs <- list(
  "ch.bfs.statistik-bevoelkerung_haushalte" = collection_spec(
    read_statpop_ha,
    label = "statpop",
    resampling = "sum",
    variables = "BBTOT",
    formats = c("parquet", "csv", "tif"),
    ha_pattern = "_ha_"
  ),
  "ch.bafu.luftreinhaltung-feinstaub_pm2_5" = collection_spec(
    read_geo_admin_rasters,
    label = "pm2_5",
    name = "pm2_5",
    formats = "tif"
  ),
  # Modellierte Gesamtdeposition von Stickstoff (nass, trocken, gasförmig), 500 m,
  # Jahre 1990, 2000, 2005, 2010, 2015, 2020 (ein COG pro Jahr). Die Collection
  # enthält zusätzlich ein Item "Additional files" (ZIP ohne Jahr), das über
  # `pattern` ausgeschlossen wird. Flächenbezogene Rate -> "average". Für Jahre
  # zwischen den Modelljahren beim Normalisieren optional `year_match = "linear"`.
  "ch.bafu.luftreinhaltung-stickstoffdeposition" = collection_spec(
    read_geo_admin_rasters,
    label = "n_deposition",
    resampling = "average",
    name = "n_deposition",
    formats = "tif",
    pattern = "_(19|20)\\d{2}_2056\\.tif$"
  )
)

#' Vorgabe für eine Collection nachschlagen (unbekannt: generischer Rasterleser)
get_collection_spec <- function(collection, specs = geo_admin_specs) {
  if (collection %in% names(specs)) {
    return(specs[[collection]])
  }
  label <- collection_label(collection)
  cli::cli_inform(c("i" = "Keine Vorgabe für {.val {collection}}, verwende {.fn read_geo_admin_rasters}."))
  collection_spec(read_geo_admin_rasters, label = label, name = label)
}


# ---- Gitter-Hilfsfunktionen ----------------------------------------------------

#' Gitterbeschreibung eines stars-Rasters
#'
#' @return Tibble mit res_x, res_y und `grid` (Auflösung, Grösse, Ursprung).
#'   Raster mit identischem `grid` lassen sich ohne Resampling stapeln.
raster_grid <- function(x) {
  x <- sf::st_normalize(x)
  dims <- stars::st_dimensions(x)
  xy <- attr(dims, "raster")$dimensions
  dx <- dims[[xy[[1]]]]
  dy <- dims[[xy[[2]]]]
  if (is.na(dx$delta) || is.na(dy$delta)) {
    cli::cli_abort("Nur regelmässige Raster werden unterstützt.")
  }

  tibble::tibble(
    res_x = abs(dx$delta),
    res_y = abs(dy$delta),
    grid  = sprintf(
      "%g x %g m | %d x %d Zellen | Ursprung %.1f / %.1f",
      abs(dx$delta), abs(dy$delta), dx$to, dy$to, dx$offset, dy$offset
    )
  )
}

#' Attribute eines stars-Objekts mit vorgegebenen Dimensionen neu zusammensetzen
with_dimensions <- function(x, dimensions) {
  names(x) |>
    rlang::set_names() |>
    purrr::map(\(attribute) unname_dim(x[[attribute]])) |>
    stars::st_as_stars(dimensions = dimensions)
}

unname_dim <- function(values) {
  dim(values) <- unname(dim(values))
  values
}


# ---- 1. Einlesen ---------------------------------------------------------------

#' Mehrere Collections für die gewünschten Jahre einlesen
#'
#' Pro Collection werden die Assets abgefragt, die Dateihierarchie aufgelöst und
#' nur verfügbare Jahre gelesen (nicht verfügbare werden gemeldet). Alle Raster
#' behalten ihre Originalauflösung.
#'
#' @param collections Collection-IDs.
#' @param years       Gewünschte Jahre (`NULL` = alle verfügbaren).
#' @param bbox        Ausschnitt, Standard Kanton Zürich (LV95 oder sf-Objekt).
#' @param specs       Collection-Vorgaben (siehe `geo_admin_specs`).
#' @param on_error    "stop" bricht ab, "warn" überspringt fehlerhafte Collections.
#' @param cache_dir   Cache-Verzeichnis.
#' @return Tibble mit einer Zeile pro Collection und Jahr: collection, label, year,
#'   format, res_x, res_y, grid, stars, item, href sowie für STATPOP die
#'   noloc-Spalten.
read_geo_admin <- function(collections,
                           years = NULL,
                           bbox = bbox_zh_lv95,
                           specs = geo_admin_specs,
                           on_error = c("stop", "warn"),
                           cache_dir = geo_admin_cache_dir()) {
  on_error <- rlang::arg_match(on_error)
  if (!is.character(collections) || length(collections) == 0) {
    cli::cli_abort("{.arg collections} muss ein Character-Vektor mit mindestens einer Collection-ID sein.")
  }

  result <- unique(collections) |>
    purrr::map(\(collection) read_one_collection(collection, years, bbox, specs, on_error, cache_dir)) |>
    purrr::list_rbind()

  if (nrow(result) == 0) {
    cli::cli_warn("Für die gewünschten Collections und Jahre wurden keine Daten gefunden.")
    return(result)
  }

  result |>
    dplyr::relocate(
      dplyr::any_of(c("collection", "label", "year", "format", "res_x", "res_y", "grid", "stars")),
      .before = 1
    ) |>
    dplyr::arrange(.data$collection, .data$year)
}

read_one_collection <- function(collection, years, bbox, specs, on_error, cache_dir) {
  spec <- get_collection_spec(collection, specs)
  cli::cli_inform(c("*" = "Collection {.val {collection}} ({spec$label})"))

  data <- tryCatch(
    {
      assets <- get_geo_admin_assets(collection, bbox = bbox)
      rlang::exec(spec$reader, assets, years = years, bbox = bbox, cache_dir = cache_dir, !!!spec$args)
    },
    error = function(err) {
      msg <- "Einlesen von {.val {collection}} fehlgeschlagen."
      if (on_error == "stop") cli::cli_abort(msg, parent = err)
      cli::cli_warn(c(msg, "i" = "Collection wird übersprungen."), parent = err)
      NULL
    }
  )

  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  grids <- purrr::map(data$stars, raster_grid) |> purrr::list_rbind()
  data |>
    tibble::add_column(collection = collection, label = spec$label, .before = 1) |>
    dplyr::bind_cols(grids)
}


# ---- 2. Verlustfreie Würfel ----------------------------------------------------

#' Jahre zu Würfeln (x, y, year) stapeln, ohne zu resamplen
#'
#' Ein stars-Würfel hat pro Dimension genau eine Auflösung. Deshalb entsteht ein
#' Würfel pro Gruppe mit identischem Gitter: bei Rohdaten pro Collection und
#' Gitter (wechselt die Auflösung zwischen Jahren, gibt es mehrere Würfel), beim
#' Ergebnis von `align_to_reference()` in der Regel genau einer.
#'
#' @param x  Tibble aus `read_geo_admin()` oder `align_to_reference()`.
#' @param by Gruppierungsspalten zusätzlich zum Gitter.
#' @return Tibble: by-Spalten, grid, years (Listen-Spalte), cube (Listen-Spalte).
stack_years <- function(x, by = intersect(c("collection", "label"), names(x))) {
  check_names(names(x), c("year", "stars", by), "x")

  x |>
    dplyr::mutate(.grid = purrr::map_chr(.data$stars, \(s) raster_grid(s)$grid)) |>
    dplyr::arrange(.data$year) |>
    dplyr::summarise(
      years = list(.data$year),
      cube  = list(stack_group(.data$stars, .data$year)),
      .by   = dplyr::all_of(c(by, ".grid"))
    ) |>
    dplyr::rename(grid = ".grid") |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(by)), purrr::map_int(.data$years, min))
}

stack_group <- function(rasters, years) {
  if (anyDuplicated(years) > 0) {
    cli::cli_abort("Mehrere Raster pro Jahr im selben Gitter: {.val {years[duplicated(years)]}}.")
  }
  attribute_names <- purrr::map(rasters, names)
  if (!all(purrr::map_lgl(attribute_names, \(n) identical(n, attribute_names[[1]])))) {
    cli::cli_abort("Attributnamen unterscheiden sich zwischen den Jahren, Stapeln nicht möglich.")
  }

  dimensions <- stars::st_dimensions(sf::st_normalize(rasters[[1]]))
  rasters <- purrr::map(rasters, \(r) with_dimensions(sf::st_normalize(r), dimensions))
  cube <- if (length(rasters) == 1) {
    # c() fügt bei nur einem Objekt keine Dimension hinzu: verdoppeln, erste Schicht behalten
    c(rasters[[1]], rasters[[1]], along = list(year = c(1L, 2L)))[, , , 1, drop = FALSE]
  } else {
    do.call(c, c(rasters, list(along = list(year = as.integer(years)))))
  }
  # Jahre als Zeitpunkte, nicht als Intervalle (wichtig bei Lücken wie 2020, 2023, 2024)
  stars::st_set_dimensions(cube, "year", values = as.integer(years), point = TRUE)
}


# ---- 3. Auf ein gemeinsames Gitter normalisieren --------------------------------

#' Raster mit GDAL auf das Gitter einer Vorlage resamplen
#'
#' Mit `method = "average"` wird flächengewichtet gemittelt: Ist die Vorlage gröber,
#' entsteht der Mittelwert der überlappenden Zellen; ist sie feiner, übernimmt jede
#' Zielzelle den Wert der Quellzelle, in der sie liegt. Mit `method = "sum"` werden
#' die Werte anteilig nach Fläche verteilt, die Summe bleibt beim Vergröbern und
#' Verfeinern erhalten (GDAL >= 3.1). NA-Zellen werden ignoriert.
#'
#' GDAL "average" setzt beim Verfeinern eine Zellreihe über den Rand der Quelldaten
#' hinaus. Zielzellen ohne echte Flächenüberlappung mit der Quelle werden deshalb
#' anschliessend auf NA gesetzt (bei gleichem KBS).
#'
#' @param x             stars-Objekt (beliebig viele Attribute, 2D).
#' @param template      stars-Objekt, dessen Gitter übernommen wird.
#' @param method        GDAL-Resampling ("average", "sum", "near", "bilinear", "mode", ...).
#' @param no_data_value Interner NoData-Wert für GDAL (darf in den Daten nicht vorkommen).
resample_to_grid <- function(x, template, method = "average", no_data_value = -9999) {
  template <- sf::st_normalize(template)
  dimensions <- stars::st_dimensions(template)
  target_dim <- unname(dim(template[[1]]))
  x <- sf::st_normalize(x)
  overlap <- if (sf::st_crs(x) == sf::st_crs(template)) source_overlap_mask(x, template) else NULL

  names(x) |>
    rlang::set_names() |>
    purrr::map(\(attribute) {
      warped <- stars::st_warp(
        x[attribute],
        dest = template[1],
        method = method,
        use_gdal = TRUE,
        no_data_value = no_data_value
      )
      values <- unname_dim(warped[[1]])
      if (!identical(dim(values), target_dim)) {
        cli::cli_abort("Resampling von {.val {attribute}} ergab ein Raster mit abweichender Grösse.")
      }
      if (!is.null(overlap)) values[!overlap] <- NA
      values
    }) |>
    stars::st_as_stars(dimensions = dimensions)
}

#' Zielzellen markieren, die sich flächig mit der Ausdehnung der Quelle überlappen
#'
#' @return Logische Matrix in der Grösse des Vorlagen-Gitters (x, y).
source_overlap_mask <- function(x, template, tolerance = 1e-6) {
  source_bbox <- sf::st_bbox(x)
  dims <- stars::st_dimensions(sf::st_normalize(template))
  xy <- attr(dims, "raster")$dimensions
  dx <- dims[[xy[[1]]]]
  dy <- dims[[xy[[2]]]]

  x_start <- dx$offset + (seq_len(dx$to) - 1) * dx$delta
  x_end <- x_start + dx$delta
  y_start <- dy$offset + (seq_len(dy$to) - 1) * dy$delta
  y_end <- y_start + dy$delta

  overlap_x <- pmin(pmax(x_start, x_end), source_bbox[["xmax"]]) -
    pmax(pmin(x_start, x_end), source_bbox[["xmin"]]) > tolerance * abs(dx$delta)
  overlap_y <- pmin(pmax(y_start, y_end), source_bbox[["ymax"]]) -
    pmax(pmin(y_start, y_end), source_bbox[["ymin"]]) > tolerance * abs(dy$delta)

  outer(overlap_x, overlap_y)
}

#' Resampling-Methode je Collection aus den Vorgaben (Standard "average")
resampling_methods <- function(specs = geo_admin_specs) {
  purrr::map_chr(specs, \(spec) spec$resampling %||% "average")
}

#' Wert für eine Collection nachschlagen
#'
#' Ein einzelner, unbenannter Wert gilt für alle Collections; ein benannter Vektor
#' wird nach Collection-ID nachgeschlagen, sonst gilt `default_method`.
method_for <- function(collection, methods, default_method) {
  if (length(methods) == 1 && is.null(names(methods))) {
    return(methods[[1]])
  }
  if (collection %in% names(methods)) methods[[collection]] else default_method
}

#' Quelljahre für ein Zieljahr bestimmen
#'
#' * "exact":   nur das Zieljahr selbst (Standard für alle Collections)
#' * "nearest": nächstgelegenes Jahr innerhalb `max_year_diff` (Gleichstand: früheres)
#' * "linear":  lineare Interpolation zwischen dem letzten Jahr davor und dem ersten
#'              danach (beide innerhalb `max_year_diff`); ausserhalb der Zeitreihe
#'              wird wie bei "nearest" das nächste Jahr übernommen, nie extrapoliert
#'
#' @param rows Zeilen einer Collection aus `read_geo_admin()`.
#' @return `NULL` oder Liste mit `rows` (1 oder 2 Zeilen), `weights` und `status`.
select_year_sources <- function(rows, target_year, year_match, max_year_diff) {
  rows <- dplyr::arrange(rows, .data$year)

  exact <- dplyr::filter(rows, .data$year == target_year)
  if (nrow(exact) > 0) {
    return(list(rows = dplyr::slice_head(exact, n = 1), weights = 1, status = "same_year"))
  }
  if (year_match == "exact") {
    return(NULL)
  }

  before <- rows |>
    dplyr::filter(.data$year < target_year, target_year - .data$year <= max_year_diff) |>
    dplyr::slice_tail(n = 1)
  after <- rows |>
    dplyr::filter(.data$year > target_year, .data$year - target_year <= max_year_diff) |>
    dplyr::slice_head(n = 1)

  if (year_match == "linear" && nrow(before) == 1 && nrow(after) == 1) {
    weight_after <- (target_year - before$year) / (after$year - before$year)
    return(list(
      rows = dplyr::bind_rows(before, after),
      weights = c(1 - weight_after, weight_after),
      status = "interpolated"
    ))
  }

  candidates <- dplyr::bind_rows(before, after)
  if (nrow(candidates) == 0) {
    return(NULL)
  }
  nearest <- candidates |>
    dplyr::mutate(.year_diff = abs(.data$year - target_year)) |>
    dplyr::arrange(.data$.year_diff, .data$year) |>
    dplyr::slice_head(n = 1) |>
    dplyr::select(-".year_diff")
  list(rows = nearest, weights = 1, status = "nearest_year")
}

#' Referenzgitter aus Ausdehnung und Zellgrösse erzeugen
#'
#' Die Ausdehnung wird nach aussen auf Vielfache der Zellgrösse gerundet. So liegt
#' das Gitter bei Zellgrössen wie 100, 200, 500 oder 1000 m auf dem STATPOP-Raster.
#'
#' @param bbox     Ausdehnung (LV95-Vektor oder sf/bbox/stars-Objekt).
#' @param cellsize Zellgrösse in Metern.
#' @param crs      KBS des Gitters.
make_reference_grid <- function(bbox = bbox_zh_lv95, cellsize = 100, crs = 2056) {
  if (!is.numeric(cellsize) || length(cellsize) != 1 || cellsize <= 0) {
    cli::cli_abort("{.arg cellsize} muss eine positive Zahl sein.")
  }
  bb <- as_bbox(bbox, crs)
  extent <- c(
    xmin = floor(bb[["xmin"]] / cellsize) * cellsize,
    ymin = floor(bb[["ymin"]] / cellsize) * cellsize,
    xmax = ceiling(bb[["xmax"]] / cellsize) * cellsize,
    ymax = ceiling(bb[["ymax"]] / cellsize) * cellsize
  )

  stars::st_as_stars(
    sf::st_bbox(extent, crs = sf::st_crs(crs)),
    nx = as.integer(round((extent[["xmax"]] - extent[["xmin"]]) / cellsize)),
    ny = as.integer(round((extent[["ymax"]] - extent[["ymin"]]) / cellsize)),
    values = NA_real_
  )
}

#' Leere Vorlage (nur Geometrie) aus einem Gitter ableiten
#'
#' @param grid stars-Objekt, stars_proxy oder Pfad/URL einer Rasterdatei. Nur die
#'   Geometrie wird verwendet; eine Datei wird nie verändert.
#' @param crs  KBS, falls das Gitter keines hat.
grid_template <- function(grid, crs = 2056, call = rlang::caller_env()) {
  if (rlang::is_string(grid)) {
    source <- if (grepl("^https?://", grid)) paste0("/vsicurl/", grid) else grid
    grid <- stars::read_stars(source, proxy = TRUE)
  }
  if (!inherits(grid, "stars")) {
    cli::cli_abort("{.arg grid} muss ein stars-Objekt oder ein Pfad zu einer Rasterdatei sein.", call = call)
  }
  if (!identical(stars::st_raster_type(grid), "regular")) {
    cli::cli_abort("{.arg grid} muss ein regelmässiges, nicht rotiertes Raster sein.", call = call)
  }

  bb <- sf::st_bbox(grid)
  if (is.na(sf::st_crs(bb))) {
    cli::cli_warn("{.arg grid} hat kein KBS, verwende {sf::st_crs(crs)$input}.")
    attr(bb, "crs") <- sf::st_crs(crs)
  }
  xy <- attr(stars::st_dimensions(grid), "raster")$dimensions
  size <- dim(grid)[xy]

  stars::st_as_stars(bb, nx = size[[1]], ny = size[[2]], values = NA_real_)
}

#' Alle Collections für ein Zieljahr auf eine Vorlage bringen (intern)
#'
#' @return Liste mit `layers` (benannte Arrays), `log` (Protokoll pro Attribut)
#'   und `rows` (verwendete Zeilen aus `candidates`).
align_layers <- function(target_year, template, candidates, methods, default_method,
                         year_match, max_year_diff, no_data_value) {
  per_collection <- candidates |>
    dplyr::distinct(.data$collection, .data$label) |>
    vctrs::vec_chop() |>
    purrr::map(\(target) {
      method <- method_for(target$collection, methods, default_method)
      matching <- method_for(target$collection, year_match, "exact")
      collection_rows <- dplyr::filter(candidates, .data$collection == target$collection)
      selection <- select_year_sources(collection_rows, target_year, matching, max_year_diff)

      if (is.null(selection)) {
        # Struktur beibehalten: Attribute des jüngsten vorhandenen Jahres, gefüllt mit NA
        attribute_names <- collection_rows |>
          dplyr::slice_max(.data$year, n = 1, with_ties = FALSE) |>
          dplyr::pull("stars") |>
          purrr::pluck(1) |>
          names()
        layers <- purrr::map(
          rlang::set_names(attribute_names),
          \(n) array(NA_real_, dim = unname(dim(template[[1]])))
        )
        log <- tibble::tibble(
          collection = target$collection, label = target$label, attribute = attribute_names,
          source_year = NA_integer_, source_year_after = NA_integer_, weight_after = NA_real_,
          source_res_x = NA_real_, source_res_y = NA_real_,
          method = method, year_match = matching, status = "missing",
          total_source = NA_real_, total_aligned = NA_real_
        )
        return(list(layers = layers, log = log, rows = NULL))
      }

      sources <- selection$rows$stars
      resampled <- purrr::map(sources, \(source) resample_to_grid(source, template, method, no_data_value))
      attributes <- names(resampled[[1]])
      if (!all(purrr::map_lgl(resampled, \(r) identical(names(r), attributes)))) {
        cli::cli_abort("{target$label}: Attribute der Jahre {selection$rows$year} unterscheiden sich, keine Interpolation möglich.")
      }

      # gewichtete Summe: bei einem Quelljahr Gewicht 1, bei Interpolation zwei Gewichte
      layers <- purrr::map(rlang::set_names(attributes), \(n) {
        purrr::map2(resampled, selection$weights, \(r, w) unname_dim(r[[n]]) * w) |>
          purrr::reduce(`+`)
      })

      # Summen protokollieren, damit die Massenerhaltung bei "sum" nachvollziehbar ist
      totals <- method == "sum"
      total_source <- purrr::map_dbl(attributes, \(n) {
        sum(purrr::map2_dbl(sources, selection$weights, \(s, w) w * sum(s[[n]], na.rm = TRUE)))
      })
      source_grid <- raster_grid(sources[[1]])
      interpolated <- selection$status == "interpolated"

      log <- tibble::tibble(
        collection = target$collection, label = target$label, attribute = attributes,
        source_year = as.integer(selection$rows$year[[1]]),
        source_year_after = if (interpolated) as.integer(selection$rows$year[[2]]) else NA_integer_,
        weight_after = if (interpolated) selection$weights[[2]] else NA_real_,
        source_res_x = source_grid$res_x, source_res_y = source_grid$res_y,
        method = method, year_match = matching, status = selection$status,
        total_source = if (totals) total_source else NA_real_,
        total_aligned = if (totals) purrr::map_dbl(attributes, \(n) sum(layers[[n]], na.rm = TRUE)) else NA_real_
      )
      list(layers = layers, log = log, rows = selection$rows)
    })

  list(
    layers = purrr::list_flatten(purrr::map(per_collection, "layers")),
    log    = purrr::list_rbind(purrr::map(per_collection, "log")),
    rows   = purrr::list_rbind(purrr::map(per_collection, "rows"))
  )
}

#' Attribut-Arrays zu einem stars-Objekt auf der Vorlage zusammensetzen (intern)
combine_layers <- function(layers, template, call = rlang::caller_env()) {
  duplicated_names <- unique(names(layers)[duplicated(names(layers))])
  if (length(duplicated_names) > 0) {
    cli::cli_abort(c(
      "x" = "Doppelte Attributnamen: {.val {duplicated_names}}.",
      "i" = "In den Collection-Vorgaben eindeutige {.arg name}/{.arg variables} setzen."
    ), call = call)
  }
  stars::st_as_stars(layers, dimensions = stars::st_dimensions(template))
}

#' noloc-Spalten der verwendeten STATPOP-Zeile übernehmen (intern)
noloc_columns <- function(rows, has_noloc) {
  if (!has_noloc) {
    return(tibble::tibble(.rows = 1))
  }
  empty <- tibble::tibble(noloc_subtracted = NA_real_, noloc_unmatched = NA_real_, noloc = list(NULL))
  if (is.null(rows) || !"noloc_subtracted" %in% names(rows)) {
    return(empty)
  }
  rows <- dplyr::filter(rows, !is.na(.data$noloc_subtracted))
  if (nrow(rows) == 0) {
    return(empty)
  }
  dplyr::select(dplyr::slice_head(rows, n = 1), dplyr::any_of(names(empty)))
}

#' Collections pro Jahr auf das Gitter einer Referenz-Collection bringen
#'
#' Für jedes Jahr der Referenz-Collection wird deren Raster als Vorlage verwendet
#' (die Referenzauflösung darf also zwischen Jahren wechseln). Die Referenz selbst
#' bleibt unverändert, alle anderen Collections werden mit GDAL auf dieses Gitter
#' gebracht, je nach `methods` gemittelt oder summiert.
#'
#' @param x              Tibble aus `read_geo_admin()` mit mindestens zwei Collections.
#' @param reference      Collection-ID der Referenz (Standard: STATPOP).
#' @param methods        Benannter Vektor Collection-ID -> GDAL-Methode
#'   (Standard aus `geo_admin_specs`: STATPOP "sum", sonst "average").
#' @param default_method Methode für Collections, die nicht in `methods` stehen.
#' @param year_match     Zeitliche Zuordnung. Standard "exact": nur Daten desselben
#'   Jahres. Optional ein Wert für alle Collections ("nearest", "linear") oder ein
#'   benannter Vektor Collection-ID -> Methode; nicht genannte Collections bleiben
#'   "exact". Siehe `select_year_sources()`.
#' @param max_year_diff  Maximale Jahresdifferenz bei "nearest" und "linear".
#' @param no_data_value  Siehe `resample_to_grid()`.
#' @return Tibble mit einer Zeile pro Referenzjahr: year, reference, res_x, res_y,
#'   grid, stars, sources (Protokoll pro Attribut) und die noloc-Spalten.
align_to_reference <- function(x,
                               reference = "ch.bfs.statistik-bevoelkerung_haushalte",
                               methods = resampling_methods(),
                               default_method = "average",
                               year_match = "exact",
                               max_year_diff = Inf,
                               no_data_value = -9999) {
  check_year_match(year_match)
  check_names(names(x), c("collection", "label", "year", "stars"), "x")

  if (!reference %in% x$collection) {
    cli::cli_abort(c(
      "x" = "Referenz-Collection {.val {reference}} ist nicht in {.arg x} enthalten.",
      "i" = "Vorhanden: {.val {unique(x$collection)}}"
    ))
  }
  if (dplyr::n_distinct(x$collection) < 2) {
    cli::cli_abort(c(
      "x" = "{.arg x} enthält nur eine Collection, es gibt nichts anzugleichen.",
      "i" = "Für ein frei gewähltes Gitter {.fn align_to_grid} verwenden."
    ))
  }

  references <- dplyr::filter(x, .data$collection == reference)
  targets <- dplyr::filter(x, .data$collection != reference)
  if (anyDuplicated(references$year) > 0) {
    cli::cli_abort("Die Referenz-Collection hat mehrere Raster pro Jahr.")
  }
  has_noloc <- "noloc_subtracted" %in% names(x)

  references |>
    dplyr::arrange(.data$year) |>
    vctrs::vec_chop() |>
    purrr::map(\(ref) {
      template <- sf::st_normalize(ref$stars[[1]])
      info <- raster_grid(template)
      cli::cli_inform("Referenzjahr {ref$year} ({info$grid}).")

      aligned <- align_layers(
        ref$year, template, targets, methods, default_method,
        year_match, max_year_diff, no_data_value
      )
      reference_layers <- purrr::map(rlang::set_names(names(template)), \(n) unname_dim(template[[n]]))

      tibble::tibble(year = as.integer(ref$year), reference = ref$collection) |>
        dplyr::bind_cols(info) |>
        dplyr::mutate(
          stars   = list(combine_layers(c(reference_layers, aligned$layers), template)),
          sources = list(aligned$log)
        ) |>
        dplyr::bind_cols(noloc_columns(dplyr::bind_rows(ref, aligned$rows), has_noloc))
    }) |>
    purrr::list_rbind()
}

#' Alle Collections auf ein frei vorgegebenes Raster normalisieren
#'
#' Jede Collection wird pro Jahr mit GDAL auf `grid` gebracht, feiner oder gröber:
#' gemäss `methods` summiert (STATPOP, Summe bleibt erhalten) oder gemittelt (alle
#' anderen). Da alle Jahre dasselbe Gitter haben, ergibt `stack_years()` auf dem
#' Ergebnis genau einen Würfel.
#'
#' Die Spalten `total_source` und `total_aligned` in `sources` zeigen bei "sum" die
#' Summe vor und nach dem Resampling. Sie weichen nur ab, wenn das Gitter kleiner
#' ist als die eingelesenen Daten. Die noloc-Spalten beziehen sich auf den beim
#' Einlesen verwendeten Ausschnitt.
#'
#' @param x              Tibble aus `read_geo_admin()`.
#' @param grid           Zielraster: stars-Objekt, Pfad/URL einer Rasterdatei oder
#'   `make_reference_grid()`. Nur die Geometrie wird verwendet.
#' @param years          Zieljahre (`NULL` = alle Jahre, die in `x` vorkommen). Für eine
#'   lückenlose Jahreszeitreihe mit Interpolation die Jahre explizit angeben.
#' @param methods,default_method,year_match,max_year_diff,no_data_value Siehe
#'   `align_to_reference()`.
#' @return Tibble mit einer Zeile pro Jahr: year, res_x, res_y, grid, stars,
#'   sources und die noloc-Spalten.
align_to_grid <- function(x,
                          grid,
                          years = NULL,
                          methods = resampling_methods(),
                          default_method = "average",
                          year_match = "exact",
                          max_year_diff = Inf,
                          no_data_value = -9999) {
  check_year_match(year_match)
  check_names(names(x), c("collection", "label", "year", "stars"), "x")

  template <- grid_template(grid)
  info <- raster_grid(template)
  years <- sort(unique(years %||% x$year))
  has_noloc <- "noloc_subtracted" %in% names(x)
  cli::cli_inform("Normalisiere {dplyr::n_distinct(x$collection)} Collection(s) auf {info$grid}.")

  years |>
    purrr::map(\(target_year) {
      cli::cli_inform("Jahr {target_year}.")
      aligned <- align_layers(
        target_year, template, x, methods, default_method,
        year_match, max_year_diff, no_data_value
      )

      tibble::tibble(year = as.integer(target_year)) |>
        dplyr::bind_cols(info) |>
        dplyr::mutate(
          stars   = list(combine_layers(aligned$layers, template)),
          sources = list(aligned$log)
        ) |>
        dplyr::bind_cols(noloc_columns(aligned$rows, has_noloc))
    }) |>
    purrr::list_rbind()
}


# ---- 5. Auswertung: Tabelle -> Würfel --------------------------------------------

#' Lange Tabelle zurück auf das Gitter eines Würfels bringen
#'
#' Gegenstück zu `as_tibble(cube)`. Die Zellen werden über die Koordinaten der
#' Vorlage zugeordnet, daher dürfen Zeilen fehlen (z. B. nach `filter()`), sie werden
#' zu NA. KBS, Gitter und Jahre als Zeitpunkte bleiben erhalten, was bei
#' `st_as_stars(dims = ...)` verloren ginge. Ohne Spalte `year` entsteht ein
#' zweidimensionaler Würfel, z. B. nach einer Zusammenfassung pro Zelle.
#'
#' @param data     Tabelle mit x, y (Zellmittelpunkte), optional year, und den Attributspalten.
#' @param template Würfel, dessen Gitter verwendet wird (z. B. der Ausgangswürfel).
#' @return stars-Objekt mit allen Spalten ausser x, y, year als Attributen.
tibble_to_cube <- function(data, template) {
  check_names(names(data), c("x", "y"), "data")
  template <- sf::st_normalize(template)
  has_year <- "year" %in% names(data)
  template_has_year <- "year" %in% names(stars::st_dimensions(template))

  if (has_year && !template_has_year) {
    cli::cli_abort("{.arg data} hat eine Spalte {.field year}, {.arg template} aber keine Jahresdimension.")
  }
  if (!has_year && template_has_year) {
    template <- dplyr::slice(template, "year", 1)
  }

  match_value <- function(values, grid_values) match(round(values, 6), round(grid_values, 6))
  idx <- cbind(
    match_value(data$x, stars::st_get_dimension_values(template, "x", center = TRUE)),
    match_value(data$y, stars::st_get_dimension_values(template, "y", center = TRUE))
  )
  if (has_year) {
    idx <- cbind(idx, match(data$year, stars::st_get_dimension_values(template, "year")))
  }

  if (anyNA(idx)) {
    cli::cli_abort("{sum(!stats::complete.cases(idx))} Zeile{?n} lieg{?t/en} nicht auf dem Gitter von {.arg template}.")
  }
  if (anyDuplicated(idx) > 0) {
    cli::cli_abort("Mehrere Zeilen pro Zelle{if (has_year) ' und Jahr'}: vorher zusammenfassen.")
  }

  attributes <- setdiff(names(data), c("x", "y", "year"))
  if (length(attributes) == 0) {
    cli::cli_abort("{.arg data} enthält keine Attributspalten ausser x, y, year.")
  }
  size <- unname(dim(template))

  attributes |>
    rlang::set_names() |>
    purrr::map(\(attribute) {
      values <- data[[attribute]]
      layer <- array(values[NA_integer_], dim = size) # NA im Typ der Spalte
      layer[idx] <- values
      layer
    }) |>
    stars::st_as_stars(dimensions = stars::st_dimensions(template))
}


# ---- Beispiele -----------------------------------------------------------------

if (FALSE) {
  # Zuerst ausführen: Die Beispiele verwenden dplyr-Verben und stars-Funktionen ohne
  # Präfix. Ist plyr nach dplyr geladen, überdeckt es count(), mutate(), summarise().
  library(dplyr)
  library(stars)

  collections <- c(
    "ch.bfs.statistik-bevoelkerung_haushalte",
    "ch.bafu.luftreinhaltung-feinstaub_pm2_5",
    "ch.bafu.luftreinhaltung-stickstoffdeposition"
  )

  # 1. Einlesen: Kanton Zürich, nur verfügbare Jahre, Originalauflösung
  data <- read_geo_admin(collections, years = 2015:2025)
  data |> select(label, year, format, grid, noloc_subtracted)

  # 2. Verlustfreie Würfel: einer pro Collection und Gitter
  stack_years(data) |> select(label, grid, years)

  # 3a. Auf das STATPOP-Gitter des jeweiligen Jahres bringen (nur Daten desselben Jahres)
  aligned <- align_to_reference(data)

  # 3b. Auf ein frei gewähltes Raster normalisieren: STATPOP summiert, Rest gemittelt
  grid_250 <- make_reference_grid(bbox_zh_lv95, cellsize = 250)
  normalized <- align_to_grid(data, grid_250)

  # Optional bei Bedarf: fehlende Jahre auffüllen, pro Collection oder für alle
  normalized_filled <- align_to_grid(
    data, grid_250,
    years = 2015:2025,
    year_match = c("ch.bafu.luftreinhaltung-stickstoffdeposition" = "linear"),
    max_year_diff = 5
  )
  # align_to_grid(data, grid_250, year_match = "nearest", max_year_diff = 3)
  # alternativ ein vorhandenes Raster als Vorlage:
  # normalized <- align_to_grid(data, "pfad/zu/referenz.tif")

  normalized$sources[[1]] # Quelljahr(e), Gewicht, Originalauflösung, Methode, Summen

  # Stickstoffdeposition allein: Modelljahre 1990-2020 in Originalauflösung (500 m) ...
  n_dep <- read_geo_admin("ch.bafu.luftreinhaltung-stickstoffdeposition")
  n_dep |> select(year, grid)

  # ... optional als Jahreszeitreihe 2000-2020 auf dem eigenen 500-m-Gitter
  n_dep_annual <- align_to_grid(n_dep, grid = n_dep$stars[[1]], years = 2000:2020, year_match = "linear")
  n_dep_annual$sources[[3]] # 2002: 60 % 2000, 40 % 2005
  n_dep_cube <- stack_years(n_dep_annual)$cube[[1]]

  # 4. Ein gemeinsamer Würfel (x, y, year) mit BBTOT, pm2_5, n_deposition
  cube <- stack_years(normalized)$cube[[1]]
  cube_years <- st_get_dimension_values(cube, "year")

  # Speichern: RDS behält Würfel, Protokoll und noloc-Spalten vollständig
  saveRDS(normalized, "geoadmin_zh_250m.rds")


  # ---- 5a. Direkt auf dem Würfel: zellweise über Attribute, alle Jahre zugleich ----

  exposure <- cube |>
    mutate(
      pm2_5_exposure = pm2_5 * BBTOT,                     # Einwohner x µg/m³
      pm2_5_above_10 = pm2_5 > 10,                        # logisches Attribut
      bbtot_above_10 = ifelse(pm2_5 > 10, BBTOT, 0)       # Einwohner über dem Wert
    ) |>
    select(BBTOT, pm2_5_exposure, pm2_5_above_10, bbtot_above_10)

  cube |> filter(year >= 2020)          # Jahre nach Wert (braucht das Paket cubelyr)
  cube |> slice(year, 1)                # ein Jahr nach Index, ohne cubelyr
  cube |> pull(pm2_5) |> dim()          # Attribut als Array (x, y, year)

  # Veränderung zwischen zwei Jahren, für alle Attribute
  change <- slice(cube, year, length(cube_years)) - slice(cube, year, 1)
  plot(change["pm2_5"])


  # ---- 5b. Über Dimensionen reduzieren (statt group_by/summarise) ------------------

  # pro Zelle über alle Jahre
  pm2_5_mean <- st_apply(cube["pm2_5"], c("x", "y"), mean, na.rm = TRUE)
  # pro Jahr über den ganzen Ausschnitt
  pop_per_year <- st_apply(cube["BBTOT"], "year", sum, na.rm = TRUE)
  pop_per_year$sum

  # pro Gemeinde (sf-Polygone, z. B. aus swissBOUNDARIES3D)
  # pop_gemeinden <- aggregate(cube["BBTOT"], by = gemeinden, FUN = sum, na.rm = TRUE)


  # ---- 5c. Voller dplyr-Umfang über die lange Tabelle ------------------------------

  cube_tbl <- as_tibble(cube) # x, y (Zellmittelpunkte), year, BBTOT, pm2_5, n_deposition

  # Kennzahlen pro Jahr: bevölkerungsgewichtete Belastung und Anteil über 10 µg/m³
  cube_tbl |>
    filter(!is.na(BBTOT), !is.na(pm2_5)) |>
    summarise(
      pop               = sum(BBTOT),
      pm2_5_popweighted = weighted.mean(pm2_5, BBTOT),
      share_above_10    = sum(BBTOT[pm2_5 > 10]) / pop,
      .by = year
    ) |>
    left_join(select(normalized, year, noloc_subtracted), by = "year")

  # Veränderung pro Zelle zum Vorjahr, zurück in einen Würfel (x, y, year)
  pm2_5_change <- cube_tbl |>
    arrange(year) |>
    mutate(pm2_5_change = pm2_5 - lag(pm2_5), .by = c(x, y)) |>
    select(x, y, year, pm2_5_change) |>
    tibble_to_cube(cube)

  # Zusammenfassung pro Zelle über alle Jahre, zurück in einen 2D-Würfel
  cell_summary <- cube_tbl |>
    filter(!is.na(pm2_5)) |>
    summarise(
      pm2_5_mean = mean(pm2_5),
      pm2_5_max  = max(pm2_5),
      n_years    = n(),
      .by = c(x, y)
    ) |>
    tibble_to_cube(cube)

  # Klassen bilden (mit case_when in der Tabelle, danach wieder als Würfel)
  pm2_5_classes <- cube_tbl |>
    mutate(pm2_5_class = case_when(
      is.na(pm2_5) ~ NA_character_,
      pm2_5 <= 5  ~ "unter WHO-Richtwert",
      pm2_5 <= 10 ~ "unter Grenzwert LRV",
      .default    = "über Grenzwert LRV"
    )) |>
    select(x, y, year, pm2_5_class) |>
    tibble_to_cube(cube)
}
