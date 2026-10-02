# A catalog of plots for Quarto reports: the plots are built once, collected in one table and taken out
# again by name and keys (parameter, year, or the caller's own) where a page shows them. print_tabset()
# writes several plots as a Quarto tabset.

#' Collect plots in a catalog
#'
#' The caller says what the names of the list mean, so the catalog does not guess its structure. Catalogs
#' of several plots are combined with [dplyr::bind_rows()].
#'
#' Every catalog has the columns `plot`, `parameter` and `year`, so that catalogs built for different
#' plots bind and are searched alike. Any other key named in `names_to` (a site, a time of day, a
#' scenario) becomes a column of its own; [dplyr::bind_rows()] fills it with `NA` for the plots that do
#' not have it.
#'
#' @param figures A plot, a named list of plots, or a list of such lists, one level per element of
#'   `names_to` (names of the outer list = `names_to[1]`). Plots are usually ggplots, but any object
#'   works.
#' @param plot Name of the plot, e.g. "distribution_histogram". `NULL` when the names of a list level
#'   are the plot names (`names_to` contains `"plot"`).
#' @param names_to What the names of the list levels are, outermost first: `character()` for a single
#'   plot, otherwise any of `"plot"`, `"parameter"`, `"year"` or a key of the caller's own (e.g.
#'   `c("parameter", "year")`, `c("site", "daypart")`). Each key once; `"figure"` is reserved.
#'
#' @return Tibble with `plot`, `parameter`, `year`, the caller's own keys (all character, `NA` if not
#'   applicable) and the list column `figure`, one row per plot. Stops with an error of class
#'   `plot_catalog_error` if a list level is not named, a key is repeated or `"figure"`, or the plot
#'   name is given neither or twice (as `plot` and as `"plot"` in `names_to`).
#'
#' @seealso [get_plot()] to take one plot out again, [catalog_entries()] for several;
#'   [fig_meta()] for the caption and alt text a plot carries to the page.
#'
#' @examples
#' p <- ggplot2::ggplot()
#' catalog <- dplyr::bind_rows(
#'   plot_catalog(p, "overview"),
#'   plot_catalog(list(NO2 = p, PM10 = p), "timeseries", names_to = "parameter"),
#'   plot_catalog(list(NO2 = list(`2023` = p, `2024` = p)), "map", names_to = c("parameter", "year")),
#'   plot_catalog(list(A1 = list(Tag = p, Nacht = p)), "rose", names_to = c("site", "daypart"))
#' )
#' catalog
#'
#' # a flat list whose names are the plot names
#' plot_catalog(list(distance = p, windrose = p), names_to = "plot")
#'
#' @export
plot_catalog <- function(figures, plot = NULL, names_to = character()) {

  if (anyDuplicated(names_to) || "figure" %in% names_to) {
    cli::cli_abort(
      c("{.arg names_to} must name each key once and cannot use {.val figure}.",
        x = "Got {.val {names_to}}."),
      class = "plot_catalog_error"
    )
  }
  if (("plot" %in% names_to) == !is.null(plot)) {
    cli::cli_abort(
      c("The plot name must be given exactly once.",
        i = "Either as {.arg plot}, or as the names of a list level with {.code names_to = \"plot\"}."),
      class = "plot_catalog_error"
    )
  }

  keys <- setdiff(names_to, c("plot", "parameter", "year"))
  catalog_rows(figures, plot %||% NA_character_, names_to) |>
    dplyr::relocate(dplyr::all_of(c("plot", "parameter", "year", keys, "figure")))
}


# rows of plot_catalog(), one list level per element of names_to
catalog_rows <- function(figures, plot, names_to, call = rlang::caller_env()) {

  if (length(names_to) == 0) {
    return(tibble::tibble(plot = plot, parameter = NA_character_, year = NA_character_, figure = list(figures)))
  }
  if (!rlang::is_named(figures)) {
    of <- if (is.na(plot)) "" else " of {.val {plot}}"
    cli::cli_abort(paste0("{.arg figures}", of, " must be a named list (names = {names_to[1]})."),
                   class = "plot_catalog_error", call = call)
  }

  figures |>
    purrr::imap(\(figure, name) {
      catalog_rows(figure, plot, names_to[-1], call) |>
        dplyr::mutate("{names_to[1]}" := name)
    }) |>
    purrr::list_rbind()
}


#' Rows of a plot catalog matching plot and keys
#'
#' @param catalog Plot catalog as built by [plot_catalog()].
#' @param plot Name of the plot.
#' @param parameter,year Parameter and year; `NULL` to keep all.
#' @param ... Further keys of the catalog by name, e.g. `site = "A1"`; each may hold several values.
#'
#' @return The matching rows. Stops with an error of class `plot_catalog_error` if there are none,
#'   naming the available plots, or if a key in `...` is unnamed or not a column of the catalog.
#'
#' @examples
#' p <- ggplot2::ggplot()
#' catalog <- plot_catalog(list(`2023` = p, `2024` = p), "map", names_to = "year")
#' catalog_entries(catalog, "map")
#'
#' roses <- plot_catalog(list(A1 = list(Tag = p, Nacht = p)), "rose", names_to = c("site", "daypart"))
#' catalog_entries(roses, "rose", daypart = "Nacht")
#'
#' @export
catalog_entries <- function(catalog, plot, parameter = NULL, year = NULL, ...) {

  filters <- catalog_filters(catalog, parameter, year, ...)

  match <- catalog$plot == plot
  for (key in names(filters)) match <- match & catalog[[key]] %in% filters[[key]]

  if (!any(match)) abort_plot_match(catalog, plot, filters, 0)
  catalog[match, ]
}


#' Get one plot from a plot catalog
#'
#' @inheritParams catalog_entries
#' @param parameter,year Parameter and year of the plot; `NULL` if the plot has none.
#' @param ... Further keys of the plot by name, e.g. `site = "A1"`.
#'
#' @return The plot. Stops with an error of class `plot_catalog_error` unless exactly one plot matches,
#'   naming the plots available.
#'
#' @examples
#' p <- ggplot2::ggplot() + ggplot2::ggtitle("NO2")
#' catalog <- plot_catalog(list(NO2 = p), "timeseries", names_to = "parameter")
#' get_plot(catalog, "timeseries", "NO2")
#'
#' roses <- plot_catalog(list(A1 = p, B2 = p), "rose", names_to = "site")
#' get_plot(roses, "rose", site = "B2")
#'
#' @export
get_plot <- function(catalog, plot, parameter = NULL, year = NULL, ...) {

  entries <- catalog_entries(catalog, plot, parameter, year, ...)
  if (nrow(entries) != 1) {
    abort_plot_match(catalog, plot, catalog_filters(catalog, parameter, year, ...), nrow(entries))
  }

  entries$figure[[1]]
}


# the keys asked for, by column name and as character; NULL keys dropped. Keys in ... must be named
# columns of the catalog
catalog_filters <- function(catalog, parameter, year, ..., call = rlang::caller_env()) {
  own <- rlang::list2(...)
  keys <- setdiff(names(catalog), c("plot", "figure"))

  if (length(own) > 0 && !rlang::is_named(own)) {
    cli::cli_abort(
      c("Further keys must be named.", i = "E.g. {.code site = \"A1\"}; the catalog has {.val {keys}}."),
      class = "plot_catalog_error", call = call
    )
  }
  unknown <- setdiff(names(own), keys)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c("The catalog has no key {.val {unknown}}.", i = "Its keys: {.val {keys}}."),
      class = "plot_catalog_error", call = call
    )
  }

  c(list(parameter = parameter, year = year), own) |>
    purrr::compact() |>
    purrr::map(as.character)
}


# error for get_plot() and catalog_entries(): how many plots match, and which ones exist, by the keys
# the plot has
abort_plot_match <- function(catalog, plot, filters, n) {
  available <- catalog[catalog$plot == plot, ]
  wanted <- paste(c(plot, unlist(filters, use.names = FALSE)), collapse = " / ")
  keys <- setdiff(names(available), c("plot", "figure")) |>
    purrr::keep(\(key) any(!is.na(available[[key]])))
  sep <- " / "
  combinations <- unique(do.call(paste, c(unname(as.list(available[keys])), sep = sep)))

  cli::cli_abort(
    c(
      "{n} plot{?s} match {.val {wanted}}, expected exactly one.",
      i = if (nrow(available) == 0) {
        "Available plots: {.val {unique(catalog$plot)}}."
      } else if (length(keys) == 0) {
        "{.val {plot}} has no keys to choose by."
      } else {
        "Available for {.val {plot}} ({.field {paste(keys, collapse = sep)}}): {.val {combinations}}."
      }
    ),
    class = "plot_catalog_error",
    call = rlang::caller_env(2)
  )
}


#' Print plots as a tabset (Quarto page)
#'
#' For a chunk with `#| output: asis`: writes a `.panel-tabset` div with one tab per element.
#'
#' @param figures Named list of plots, or of functions printing the content of a tab (e.g. a slider);
#'   the names are the tab titles.
#' @param level Heading level of the tab titles; it must be lower than the headings around the tabset.
#'
#' @return `NULL`, invisibly; called for its output.
#'
#' @examples
#' print_tabset(list(absolut = \() cat("plot 1\n"), relativ = \() cat("plot 2\n")))
#'
#' @export
print_tabset <- function(figures, level = 5) {

  cat("\n\n::: {.panel-tabset}\n\n")
  purrr::iwalk(figures, \(figure, title) {
    cat(strrep("#", level), " ", title, "\n\n", sep = "")
    if (is.function(figure)) figure() else print(figure)
    cat("\n\n")
  })
  cat(":::\n\n")

  invisible(NULL)
}
