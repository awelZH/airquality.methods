# A catalog of plots for Quarto reports: the plots are built once, collected in one table and taken out
# again by name, parameter and year where a page shows them. print_tabset() writes several plots as a
# Quarto tabset.

#' Collect plots in a catalog
#'
#' The caller says what the names of the list mean, so the catalog does not guess its structure. Catalogs
#' of several plots are combined with [dplyr::bind_rows()].
#'
#' @param figures A plot, a list of plots named by parameter or year, or a list of such lists (names of
#'   the outer list = parameter, of the inner lists = year). Plots are usually ggplots, but any object
#'   works.
#' @param plot Name of the plot, e.g. "distribution_histogram".
#' @param names_to What the names of the list levels are: `character()` for a single plot, `"parameter"`,
#'   `"year"` or `c("parameter", "year")`.
#'
#' @return Tibble with `plot`, `parameter` and `year` (character, `NA` if not applicable) and the list
#'   column `figure`, one row per plot. Stops with an error of class `plot_catalog_error` if a list
#'   level is not named.
#'
#' @seealso [get_plot()] to take one plot out again, [catalog_entries()] for several.
#'
#' @examples
#' p <- ggplot2::ggplot()
#' catalog <- dplyr::bind_rows(
#'   plot_catalog(p, "overview"),
#'   plot_catalog(list(NO2 = p, PM10 = p), "timeseries", names_to = "parameter"),
#'   plot_catalog(list(NO2 = list(`2023` = p, `2024` = p)), "map", names_to = c("parameter", "year"))
#' )
#' catalog
#'
#' @export
plot_catalog <- function(figures, plot, names_to = character()) {

  if (length(names_to) == 0) {
    return(tibble::tibble(plot = plot, parameter = NA_character_, year = NA_character_, figure = list(figures)))
  }
  if (!rlang::is_named(figures)) {
    cli::cli_abort("{.arg figures} of {.val {plot}} must be a named list (names = {names_to[1]}).",
                   class = "plot_catalog_error")
  }

  figures |>
    purrr::imap(\(figure, name) {
      plot_catalog(figure, plot, names_to[-1]) |>
        dplyr::mutate("{names_to[1]}" := name)
    }) |>
    purrr::list_rbind()
}


#' Rows of a plot catalog matching plot, parameter and year
#'
#' @param catalog Plot catalog as built by [plot_catalog()].
#' @param plot Name of the plot.
#' @param parameter,year Parameter and year; `NULL` to keep all.
#'
#' @return The matching rows. Stops with an error of class `plot_catalog_error` if there are none,
#'   naming the available plots.
#'
#' @examples
#' p <- ggplot2::ggplot()
#' catalog <- plot_catalog(list(`2023` = p, `2024` = p), "map", names_to = "year")
#' catalog_entries(catalog, "map")
#'
#' @export
catalog_entries <- function(catalog, plot, parameter = NULL, year = NULL) {

  match <- catalog$plot == plot
  if (!is.null(parameter)) match <- match & catalog$parameter %in% parameter
  if (!is.null(year)) match <- match & catalog$year %in% as.character(year)

  if (!any(match)) abort_plot_match(catalog, plot, parameter, year, 0)
  catalog[match, ]
}


#' Get one plot from a plot catalog
#'
#' @inheritParams catalog_entries
#' @param parameter,year Parameter and year of the plot; `NULL` if the plot has none.
#'
#' @return The plot. Stops with an error of class `plot_catalog_error` unless exactly one plot matches,
#'   naming the plots available.
#'
#' @examples
#' p <- ggplot2::ggplot() + ggplot2::ggtitle("NO2")
#' catalog <- plot_catalog(list(NO2 = p), "timeseries", names_to = "parameter")
#' get_plot(catalog, "timeseries", "NO2")
#'
#' @export
get_plot <- function(catalog, plot, parameter = NULL, year = NULL) {

  entries <- catalog_entries(catalog, plot, parameter, year)
  if (nrow(entries) != 1) abort_plot_match(catalog, plot, parameter, year, nrow(entries))

  entries$figure[[1]]
}


# error for get_plot() and catalog_entries(): how many plots match, and which ones exist
abort_plot_match <- function(catalog, plot, parameter, year, n) {
  available <- catalog[catalog$plot == plot, ]
  wanted <- paste(c(plot, parameter, year), collapse = " / ")
  cli::cli_abort(
    c(
      "{n} plot{?s} match {.val {wanted}}, expected exactly one.",
      i = if (nrow(available) == 0) {
        "Available plots: {.val {unique(catalog$plot)}}."
      } else {
        "Available for {.val {plot}} (parameter / year): {.val {unique(paste(available$parameter, available$year, sep = ' / '))}}."
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
