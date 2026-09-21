# A legend with one block per group (e.g. one block per sector with its subsectors), for stacked
# plots whose elements repeat across groups ("verschiedene" in several sectors).

#' Unique key per group and element, for a grouped legend
#'
#' Elements with the same name in different groups (e.g. "verschiedene" in several sectors) get
#' different keys. The factor levels define the order of the stack and of the legend: groups in
#' the order of their first element (or `group_order`), elements within a group by `order`.
#'
#' @param group Group of each element, e.g. the sector.
#' @param key Element, e.g. the subsector.
#' @param order Sort order of the elements (numeric); `NULL` keeps the order of appearance.
#' @param group_order Groups in the wanted order; groups not listed follow in their default order.
#' @param sep Separator between group and element; must not occur in the names.
#'
#' @return Factor with values `"<group><sep><key>"`.
#'
#' @examples
#' grouped_key(c("Verkehr", "Industrie", "Verkehr"), c("Strasse", "verschiedene", "verschiedene"))
#'
#' @export
grouped_key <- function(group, key, order = NULL, group_order = NULL, sep = "::") {
  if (any(stringr::str_detect(c(group, key), stringr::fixed(sep)))) {
    cli::cli_abort("Group and element names must not contain the separator {.val {sep}}.")
  }
  order <- order %||% seq_along(group)
  groups <- unique(c(intersect(group_order, group), group[base::order(order)]))

  id <- paste0(group, sep, key)
  levels <- unique(id[base::order(match(group, groups), order)])
  factor(id, levels = levels)
}


#' Use a grouped legend: one block per group, with the group as title
#'
#' Each group of the legend (e.g. a sector) becomes a block with the group as title and its
#' elements (e.g. the subsectors) without the group name, drawn by
#' [legendry::guide_legend_group()]. Works for any discrete scale of `aesthetic` whose breaks are
#' keys from [grouped_key()]. The block titles look like the entries, the keys of a block have no
#' gaps (as in a ggplot legend), and the blocks are set apart by `spacing`.
#'
#' The result is an ordinary ggplot: position, size etc. of the legend follow the theme, also
#' when changed later. Only the default style of the block titles is taken from the theme at the
#' time of the call.
#'
#' @param plot A ggplot object.
#' @param aesthetic Aesthetic with the grouped keys, e.g. `"fill"` or `"colour"`.
#' @param sep Separator used in [grouped_key()].
#' @param spacing Space between the blocks.
#' @param subtitle Text element of the block titles; `NULL` takes the plot's `legend.text`.
#' @param key_spacing Vertical space between the keys of a block.
#'
#' @return The ggplot object with the grouped legend.
#'
#' @examplesIf rlang::is_installed("legendry")
#' data <- data.frame(
#'   year = rep(2020:2021, each = 3),
#'   sector = c("Verkehr", "Verkehr", "Industrie"),
#'   subsector = c("Strasse", "verschiedene", "verschiedene"),
#'   emission = 1:6
#' )
#' data$key <- grouped_key(data$sector, data$subsector)
#' plot <- ggplot2::ggplot(data, ggplot2::aes(year, emission, fill = key)) +
#'   ggplot2::geom_col()
#' add_grouped_legend(plot)
#'
#' @export
add_grouped_legend <- function(plot, aesthetic = "fill", sep = "::", spacing = grid::unit(3, "mm"), subtitle = NULL,
                               key_spacing = grid::unit(0, "pt")) {
  rlang::check_installed("legendry", reason = "to draw a grouped legend.")
  if (is.null(ggplot2::get_guide_data(plot, aesthetic))) {
    cli::cli_abort("The plot has no legend for {.val {aesthetic}}.")
  }

  if (is.null(subtitle)) {
    # style of the entries, but not blank when the legend title is blank (subtitles inherit from it)
    text <- ggplot2::calc_element("legend.text", ggplot2::complete_theme(plot$theme))
    subtitle <- ggplot2::element_text(family = text$family, face = text$face, colour = text$colour,
                                      size = text$size, inherit.blank = FALSE)
  }
  guide <- legendry::guide_legend_group(key = legendry::key_group_split(sep = sep))

  plot +
    do.call(ggplot2::guides, rlang::set_names(list(guide), aesthetic)) +
    ggplot2::theme(legendry.group.spacing = spacing, legendry.legend.subtitle = subtitle,
                   legend.key.spacing.y = key_spacing)
}
