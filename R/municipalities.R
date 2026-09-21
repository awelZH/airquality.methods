# Assigning grid cells to the municipalities of the Canton of Zurich (geolion map, see
# read_geolion_wfs()) and giving the STATPOP collector pixel inhabitants back to their municipality
# (the counterpart of the subtraction in read_statpop_ha()).

#' Remove enclaves of other cantons from the municipality map
#'
#' The geolion map contains the Kloster Fahr, an enclave of the Canton of Aargau
#' (`bfs = 0`, "ausserkantonale Enklave"). It is not part of the canton.
#'
#' @param map_municipalities `sf` polygons with `art_text`.
#'
#' @return `map_municipalities` without enclaves of other cantons.
#'
#' @export
drop_foreign_enclaves <- function(map_municipalities) {
  foreign <- stringr::str_detect(map_municipalities$art_text, "ausserkantonal")
  dplyr::filter(map_municipalities, !foreign)
}

#' Assign each cell the municipality its centre lies in
#'
#' Exclaves are separate features with the same `bfs` number, so every cell is
#' matched to at most one feature and counted exactly once. Cells whose centre
#' lies in a lake belonging to no municipality (`bfs = 0`, no name) are
#' inhabited shore cells: they get the nearest municipality. Cells outside every
#' feature are outside the canton. Remove enclaves of other cantons first, see
#' [drop_foreign_enclaves()].
#'
#' @param cells Tibble with cell centre coordinates `x`, `y`.
#' @param map_municipalities `sf` polygons with `bfs` and `gemeindename`.
#'
#' @return `cells` with `bfsnr` and `gemeindename` added (`NA` outside the
#'   canton).
#'
#' @export
assign_municipalities <- function(cells, map_municipalities) {
  lookup <- map_municipalities |>
    sf::st_drop_geometry() |>
    dplyr::distinct(.data$bfs, .data$gemeindename)
  if (anyDuplicated(lookup$bfs) > 0) {
    cli::cli_abort(c(
      "x" = "Each {.field bfs} number must map to exactly one {.field gemeindename}.",
      "i" = "Ambiguous: {.val {unique(lookup$bfs[duplicated(lookup$bfs)])}}."
    ))
  }

  centres <- dplyr::distinct(cells, .data$x, .data$y)
  points <- sf::st_as_sf(centres, coords = c("x", "y"), crs = sf::st_crs(map_municipalities))
  hits <- sf::st_intersects(points, map_municipalities)
  feature <- purrr::map_int(hits, \(i) if (length(i) > 0) i[[1]] else NA_integer_)

  # cells in a lake: nearest municipality
  municipal <- which(!is.na(map_municipalities$gemeindename))
  in_lake <- which(!is.na(feature) & !feature %in% municipal)
  if (length(in_lake) > 0) {
    nearest <- sf::st_nearest_feature(points[in_lake, ], map_municipalities[municipal, ])
    feature[in_lake] <- municipal[nearest]
    cli::cli_inform("{length(in_lake)} cell{?s} with the centre in a lake assigned to the nearest municipality.")
  }

  centres <- centres |>
    dplyr::mutate(
      bfsnr = map_municipalities$bfs[feature],
      gemeindename = map_municipalities$gemeindename[feature]
    )

  dplyr::left_join(cells, centres, by = dplyr::join_by("x", "y"), relationship = "many-to-one")
}


# ---- STATPOP collector pixels ------------------------------------------------------

#' Inhabitants subtracted from the STATPOP collector pixels
#'
#' @param aligned Output of [align_to_reference()] with a STATPOP reference read by
#'   [read_statpop_ha()]; its `noloc` column holds the audit of [subtract_noloc()] per
#'   year (`NULL` if no correction was applied).
#'
#' @return Tibble with `year`, `x`, `y` (cell centre) and `noloc` (inhabitants
#'   subtracted from that cell).
#'
#' @export
noloc_from_aligned <- function(aligned) {
  empty <- tibble::tibble(year = numeric(), x = numeric(), y = numeric(), noloc = numeric())
  if (!"noloc" %in% names(aligned)) {
    return(empty)
  }

  purrr::pmap(list(aligned$noloc, aligned$year, aligned$res_x), \(audit, year, res) {
    if (is.null(audit) || nrow(audit) == 0) {
      return(NULL)
    }
    # collector pixel coordinates are lower-left cell corners
    tibble::tibble(
      year = as.numeric(year),
      x = audit$E_KOORD + res / 2,
      y = audit$N_KOORD + res / 2,
      noloc = audit$subtracted
    )
  }) |>
    purrr::list_rbind() |>
    (\(x) if (is.null(x) || nrow(x) == 0) empty else dplyr::filter(x, .data$noloc > 0))()
}

#' Spread the inhabitants of the collector pixels over their municipality
#'
#' The inhabitants BFS cannot locate sit in one collector pixel per
#' municipality; [read_statpop_ha()] subtracts them. Here
#' they are given back to their municipality, spread over its inhabited cells
#' in proportion to the inhabitants of each cell (factor
#' `1 + noloc / located`). Assumption: they are exposed like the located
#' inhabitants of the same municipality; the municipality's population-weighted
#' mean stays unchanged, its population becomes complete again.
#'
#' @param cells Cell table with `year`, `population`, `bfsnr`.
#' @param noloc Output of [noloc_from_aligned()].
#' @param map_municipalities Municipality polygons; collector pixels outside
#'   the canton are ignored.
#'
#' @return `cells` with adjusted `population`.
#'
#' @export
redistribute_noloc <- function(cells, noloc, map_municipalities) {
  if (nrow(noloc) == 0) {
    return(cells)
  }

  noloc_municipal <- noloc |>
    assign_municipalities(map_municipalities) |>
    dplyr::filter(!is.na(.data$bfsnr)) |>
    dplyr::summarise(noloc = sum(.data$noloc), .by = c("year", "bfsnr", "gemeindename"))

  located <- cells |>
    dplyr::filter(!is.na(.data$bfsnr), .data$population > 0) |>
    dplyr::summarise(located = sum(.data$population), .by = c("year", "bfsnr"))

  factors <- noloc_municipal |>
    dplyr::left_join(located, by = dplyr::join_by("year", "bfsnr"), relationship = "one-to-one")

  lost <- dplyr::filter(factors, is.na(.data$located))
  if (nrow(lost) > 0) {
    cli::cli_warn(c(
      "!" = "{sum(lost$noloc)} collector inhabitant{?s} cannot be redistributed: no inhabited cell.",
      "i" = "Municipalities: {.val {unique(lost$gemeindename)}}."
    ))
  }

  factors <- factors |>
    dplyr::filter(!is.na(.data$located)) |>
    dplyr::transmute(.data$year, .data$bfsnr, .factor = 1 + .data$noloc / .data$located)

  cells |>
    dplyr::left_join(factors, by = dplyr::join_by("year", "bfsnr"), relationship = "many-to-one") |>
    dplyr::mutate(population = .data$population * dplyr::coalesce(.data$.factor, 1)) |>
    dplyr::select(!".factor")
}
