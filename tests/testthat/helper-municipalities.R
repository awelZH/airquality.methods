# Synthetic municipality maps and cell tables for the municipality and collector pixel tests.

make_square <- function(xmin, ymin, size) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmin + size, ymin), c(xmin + size, ymin + size),
    c(xmin, ymin + size), c(xmin, ymin)
  )))
}

make_map <- function() {
  # municipality 1 with an exclave (same bfs, two features), municipality 2,
  # a lake that belongs to no municipality (bfs 0, no name) and an enclave of
  # another canton (bfs 0, no name), as in the geolion map
  sf::st_sf(
    bfs = c(1L, 1L, 2L, 0L, 0L),
    gemeindename = c("A", "A", "B", NA, NA),
    art_text = c(
      "Gemeinde", "Exklave einer Gemeinde", "Gemeinde",
      "zu keiner Gemeinde gehörende Seefläche", "ausserkantonale Enklave (Kloster Fahr)"
    ),
    geometry = sf::st_sfc(
      make_square(0, 0, 200),
      make_square(1000, 1000, 100),
      make_square(200, 0, 200),
      make_square(0, 200, 400),
      make_square(2000, 2000, 100),
      crs = 2056
    )
  )
}

make_aligned_noloc <- function() {
  # audit tables as returned by align_to_reference(); the
  # coordinates are the lower-left corners of the 100 m cells
  tibble::tibble(
    year = c(2020L, 2021L),
    res_x = 100,
    noloc = list(
      tibble::tibble(E_KOORD = c(0, 5000), N_KOORD = c(0, 5000), noloc = c(6, 9), subtracted = c(6, 9)),
      NULL
    )
  )
}

make_cells_noloc <- function() {
  # municipality A: two inhabited cells and one empty cell; B: one cell;
  # one cell outside the canton
  tibble::tibble(
    x = c(50, 150, 150, 250, 5000),
    y = c(50, 50, 150, 50, 5000),
    year = 2020,
    population = c(10, 30, 0, 20, 7),
    bfsnr = c(1L, 1L, 1L, 2L, NA),
    gemeindename = c("A", "A", "A", "B", NA),
    no2 = c(10, 20, 99, 15, 50)
  )
}
