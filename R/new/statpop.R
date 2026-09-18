# STATPOP (ch.bfs.statistik-bevoelkerung_haushalte): Hektarraster als stars einlesen
# und nicht lokalisierbare Einwohner (Sammelpixel) herausrechnen.
#
# Setzt geo_admin_stac.R voraus. Reihenfolge: geo_admin_stac.R -> statpop.R -> geo_admin_cube.R

# ---- Sammelpixel-Korrektur -----------------------------------------------------

#' Nicht lokalisierbare Einwohner von den Sammelpixeln abziehen
#'
#' Jede Gemeinde hat einen Sammelpixel, in dem die nicht räumlich zuweisbaren
#' Einwohner enthalten sind. Diese Funktion zieht sie von der betreffenden
#' Rasterzelle ab und protokolliert jede Zelle.
#'
#' Status pro Sammelpixel:
#' * "subtracted":     vollständig abgezogen
#' * "exceeds_cell":   noloc > Zellwert, nur der Zellwert wurde abgezogen (Zelle = 0)
#' * "cell_missing":   Zelle ist im Raster leer (NA), nichts abgezogen
#' * "outside_extent": Sammelpixel liegt ausserhalb der bbox, nichts abgezogen
#'
#' @param x        stars-Objekt (2D) mit dem Attribut `variable`.
#' @param noloc    Tabelle der Sammelpixel mit `coords` und `variable`.
#' @param variable Zu korrigierendes Attribut (Einwohner).
#' @param coords   Koordinatenspalten in `noloc`.
#' @param anchor   Bezugspunkt der Sammelpixel-Koordinaten.
#' @return Liste mit `stars` (korrigiert) und `noloc` (Protokoll pro Sammelpixel).
subtract_noloc <- function(x,
                           noloc,
                           variable = "BBTOT",
                           coords = c("E_KOORD", "N_KOORD"),
                           anchor = c("lowerleft", "center")) {
  anchor <- rlang::arg_match(anchor)
  check_names(names(noloc), c(coords, variable), "noloc")
  check_names(names(x), variable, "x")

  x <- stars::st_as_stars(x)
  xy <- attr(stars::st_dimensions(x), "raster")$dimensions
  if (length(dim(x)) != 2) {
    cli::cli_abort("{.arg x} muss zweidimensional sein (nur {.val {xy}}), hat aber {.val {names(dim(x))}}.")
  }
  dim_x <- stars::st_dimensions(x)[[xy[[1]]]]
  dim_y <- stars::st_dimensions(x)[[xy[[2]]]]

  # Über den Zellmittelpunkt zuordnen, damit Rundung an Zellgrenzen keine Rolle spielt
  half <- if (anchor == "lowerleft") abs(c(dim_x$delta, dim_y$delta)) / 2 else c(0, 0)

  pixels <- noloc |>
    dplyr::summarise(
      n_records = dplyr::n(),
      noloc = sum(.data[[variable]], na.rm = TRUE),
      .by = dplyr::all_of(coords)
    ) |>
    dplyr::mutate(
      .col = floor((.data[[coords[[1]]]] + half[[1]] - dim_x$offset) / dim_x$delta) + 2 - dim_x$from,
      .row = floor((.data[[coords[[2]]]] + half[[2]] - dim_y$offset) / dim_y$delta) + 2 - dim_y$from,
      .inside = .data$.col >= 1 & .data$.col <= dim(x)[[xy[[1]]]] &
        .data$.row >= 1 & .data$.row <= dim(x)[[xy[[2]]]]
    )

  values <- x[[variable]]
  idx <- cbind(pixels$.col, pixels$.row)
  cell_before <- rep(NA_real_, nrow(pixels))
  cell_before[pixels$.inside] <- values[idx[pixels$.inside, , drop = FALSE]]

  report <- pixels |>
    dplyr::mutate(
      cell_before = cell_before,
      status = dplyr::case_when(
        !.data$.inside ~ "outside_extent",
        is.na(.data$cell_before) ~ "cell_missing",
        .data$noloc > .data$cell_before ~ "exceeds_cell",
        .default = "subtracted"
      ),
      subtracted = dplyr::if_else(
        .data$status %in% c("subtracted", "exceeds_cell"),
        pmin(.data$noloc, .data$cell_before),
        0
      ),
      cell_after = .data$cell_before - .data$subtracted
    )

  to_update <- report$subtracted > 0
  new_values <- report$cell_after[to_update]
  if (is.integer(values)) new_values <- as.integer(round(new_values))
  values[idx[to_update, , drop = FALSE]] <- new_values
  x[[variable]] <- values

  problems <- dplyr::count(dplyr::filter(report, .data$status %in% c("exceeds_cell", "cell_missing")), .data$status)
  if (nrow(problems) > 0) {
    cli::cli_warn(c(
      "!" = "{sum(problems$n)} Sammelpixel konnte{?n} nicht vollständig abgezogen werden.",
      rlang::set_names(paste0(problems$status, ": ", problems$n), rep("*", nrow(problems))),
      "i" = "Details in der Spalte {.field noloc} des Ergebnisses."
    ))
  }

  list(
    stars = x,
    noloc = dplyr::select(report, dplyr::all_of(coords), "n_records", "noloc", "cell_before", "subtracted", "cell_after", "status")
  )
}


# ---- STATPOP einlesen ----------------------------------------------------------

#' Ein STATPOP-Item (Jahr) einlesen und korrigieren
read_statpop_item <- function(ha_asset, noloc_assets, variables, bbox, noloc_variable,
                              year_col, coords, anchor, correct_noloc, epsg, cache_dir, ...) {
  cli::cli_inform("Lese {.val {ha_asset$item}} ({ha_asset$format}).")
  raster <- read_asset_stars(
    ha_asset,
    variables = variables, bbox = bbox, coords = coords, anchor = anchor,
    crs = epsg, cache_dir = cache_dir, ...
  )

  result <- tibble::tibble(
    item   = ha_asset$item,
    year   = ha_asset$year,
    format = ha_asset$format,
    href   = ha_asset$href
  )

  without_correction <- function(message = NULL) {
    if (!is.null(message)) cli::cli_warn(message)
    dplyr::mutate(
      result,
      stars = list(raster), noloc_subtracted = NA_real_, noloc_unmatched = NA_real_, noloc = list(NULL)
    )
  }

  if (!correct_noloc) {
    return(without_correction())
  }

  noloc_asset <- dplyr::filter(noloc_assets, .data$item == ha_asset$item)
  if (nrow(noloc_asset) != 1) {
    return(without_correction(
      "Item {.val {ha_asset$item}}: {nrow(noloc_asset)} noloc-Dateien gefunden (erwartet 1), keine Korrektur."
    ))
  }

  noloc <- download_geo_admin_asset(noloc_asset, cache_dir) |>
    read_asset_table(noloc_asset$format)
  check_names(names(noloc), year_col, noloc_asset$asset)

  noloc_year <- unique(noloc[[year_col]])
  if (length(noloc_year) != 1) {
    cli::cli_abort("{.val {noloc_asset$asset}} enthält {length(noloc_year)} Werte in {.field {year_col}}, erwartet 1.")
  }
  if (!is.na(ha_asset$year) && noloc_year != ha_asset$year) {
    cli::cli_warn(c(
      "!" = "Item {.val {ha_asset$item}}: Metadaten-Jahr {ha_asset$year}, {year_col} = {noloc_year}.",
      "i" = "Verwendet wird {year_col}. Die Jahresauswahl basiert auf den Metadaten."
    ))
  }

  corrected <- subtract_noloc(raster, noloc, variable = noloc_variable, coords = coords, anchor = anchor)

  result |>
    dplyr::mutate(
      year             = as.integer(.env$noloc_year),
      stars            = list(corrected$stars),
      noloc_subtracted = sum(corrected$noloc$subtracted),
      noloc_unmatched  = sum(corrected$noloc$noloc) - sum(corrected$noloc$subtracted),
      noloc            = list(corrected$noloc)
    )
}

#' STATPOP-Hektarraster einlesen, pro Jahr ein stars-Objekt
#'
#' Die Dateihierarchie wird mit `resolve_assets()` aufgelöst (Muster, EPSG,
#' Formatpräferenz, Jahre). Die nicht lokalisierbaren Einwohner werden von den
#' Sammelpixeln abgezogen und ausserhalb des Rasters mitgeführt:
#' * `noloc_subtracted`: Summe der abgezogenen Einwohner (im Ausschnitt)
#' * `noloc_unmatched`:  Summe der noloc-Einwohner, die nicht abgezogen wurden
#'   (Sammelpixel ausserhalb der bbox oder Datenprobleme)
#' * `noloc`:            Protokoll pro Sammelpixel
#' Es gilt: Summe noloc-Datei = noloc_subtracted + noloc_unmatched.
#'
#' @param assets         Tibble aus `get_geo_admin_assets()`.
#' @param years          Gewünschte Jahre (`NULL` = alle verfügbaren).
#' @param variables      Einzulesende Variablen. Muss `noloc_variable` enthalten,
#'   wenn korrigiert wird. Nur `noloc_variable` wird korrigiert.
#' @param formats        Formatpräferenz.
#' @param bbox           Ausschnitt (Standard: Kanton Zürich).
#' @param ha_pattern     Regex für die Hektarraster-Assets.
#' @param noloc_pattern  Regex für die noloc-Assets.
#' @param noloc_variable Einwohnervariable in beiden Datensätzen.
#' @param year_col       Jahresspalte in der noloc-Datei.
#' @param coords,anchor  Koordinatenspalten und Bezugspunkt (Hektarraster und noloc).
#' @param correct_noloc  Korrektur durchführen?
#' @param epsg           Erwartetes KBS.
#' @param cache_dir      Cache-Verzeichnis.
#' @param ...            Weitere Argumente an `read_asset_stars()` (z. B. `cellsize`).
#' @return Tibble: item, year, format, href, stars, noloc_subtracted,
#'   noloc_unmatched, noloc.
read_statpop_ha <- function(assets,
                            years = NULL,
                            variables = "BBTOT",
                            formats = c("parquet", "csv", "tif"),
                            bbox = bbox_zh_lv95,
                            ha_pattern = "_ha_",
                            noloc_pattern = "_noloc_",
                            noloc_variable = "BBTOT",
                            year_col = "ERHJAHR",
                            coords = c("E_KOORD", "N_KOORD"),
                            anchor = c("lowerleft", "center"),
                            correct_noloc = TRUE,
                            epsg = 2056,
                            cache_dir = geo_admin_cache_dir(),
                            ...) {
  anchor <- rlang::arg_match(anchor)
  if (correct_noloc && !is.null(variables) && !noloc_variable %in% variables) {
    cli::cli_abort("{.arg variables} muss {.val {noloc_variable}} enthalten, wenn {.arg correct_noloc} = TRUE.")
  }
  if (correct_noloc && length(variables) > 1) {
    cli::cli_inform(c("i" = "Nur {.val {noloc_variable}} wird um die Sammelpixel korrigiert."))
  }

  ha <- resolve_assets(assets, pattern = ha_pattern, formats = formats, epsg = epsg, years = years)
  noloc_assets <- dplyr::filter(
    assets,
    stringr::str_detect(.data$asset, noloc_pattern),
    .data$format == "csv"
  )

  ha |>
    vctrs::vec_chop() |> # eine Zeile pro Jahr
    purrr::map(\(ha_asset) read_statpop_item(
      ha_asset, noloc_assets, variables, bbox, noloc_variable,
      year_col, coords, anchor, correct_noloc, epsg, cache_dir, ...
    )) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$year)
}


# ---- Beispiel ------------------------------------------------------------------

if (FALSE) {
  statpop_assets <- get_geo_admin_assets("ch.bfs.statistik-bevoelkerung_haushalte")

  # Kanton Zürich, Dateihierarchie und Jahre werden automatisch aufgelöst
  statpop <- read_statpop_ha(statpop_assets, years = 2015:2025)

  statpop |> dplyr::select(year, format, noloc_subtracted, noloc_unmatched)
  statpop$noloc[[1]] # Protokoll pro Sammelpixel

  # Kontrolle: lokalisierte + nicht lokalisierte Einwohner
  statpop |>
    dplyr::mutate(
      bbtot_raster = purrr::map_dbl(stars, \(s) sum(s$BBTOT, na.rm = TRUE)),
      bbtot_total  = bbtot_raster + noloc_subtracted
    )
}
