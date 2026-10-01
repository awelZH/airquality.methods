# =============================================================================
#  Package-level registration
# -----------------------------------------------------------------------------
#  The polar grid is drawn as ordinary layers on a Cartesian coord, not by the
#  coord itself, so ggplot2's own `panel.grid` cannot style it -- and must not
#  be borrowed for the purpose either, because a live `panel.grid.major` in a
#  Cartesian panel draws *straight* lines across the rose.
#
#  A registered element of our own gives the styling a proper home: it is
#  themable exactly like any other element, but ggplot2 never renders it
#  itself, so nothing straight appears. See `.polar_grid_style()`.
# =============================================================================

# The dot-prefixed names are columns the polar functions create inside layer
# data frames. R CMD check cannot see through data masking, so they have to be
# declared or every aes() using them becomes a "no visible binding" note.
utils::globalVariables(c(".u", ".v", ".r", ".E", ".N", ".site", "n"))

.onLoad <- function(libname, pkgname) {
  ggplot2::register_theme_elements(
    polar.grid = ggplot2::element_line(colour = "grey35", linewidth = 0.25,
                                       linetype = 2),
    element_tree = list(
      polar.grid = ggplot2::el_def("element_line", "line",
                                   description = "rings and spokes of the polar grid")
    )
  )
  invisible()
}
