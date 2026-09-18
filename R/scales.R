# Colour scales for air quality maps and emission figures.

#' Official colour ramp for air quality raster maps
#'
#' The 18-step ramp used by the cantonal air quality maps, as a capped fill
#' scale: values beyond `limits` are squished onto the edge colour and the legend
#' flags them with "<="/">=" instead of turning them into holes in the map.
#'
#' @param ... Arguments passed to [scale_fill_capped()], `limits`, `breaks` and
#'   `name` above all.
#'
#' @return A ggplot2 scale.
#'
#' @keywords internal
immission_colorscale <- function(...) {
  colours <- c(
    "#004DA8", "#005ce6", "#0070ff", "#00c5ff", "#47d9fa", "#56f9fb",
    "#2e9c6b", "#38bd00", "#56d900", "#51f551", "#ffff00", "#ffd400",
    "#ffa300", "#ff5200", "#ff0000", "#ff0094", "#de00a1", "#c500ba"
  )

  scale_fill_capped(palette = colours, na.value = NA, ...)
}

#' Pollutant-specific fill scale for raster maps
#'
#' Limits, breaks and legend title follow the reporting conventions for each
#' parameter, so the same pollutant always looks the same across figures.
#'
#' @param parameter Parameter code, for example `"NO2"` or
#'   `"O3_peakseason_mean_d1_max_mean_h8gl"`.
#'
#' @return A ggplot2 scale.
#'
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density * 1000)) +
#'   geom_raster() +
#'   immissionscale("PM2.5")
#'
#' @export
immissionscale <- function(parameter) {
  switch(parameter,
    NO2 = immission_colorscale(
      limits = c(0, 50), breaks = seq(0, 50, 10), name = "NO2\n(\u00b5g/m3)"
    ),
    `O3_max_98p_m1` = immission_colorscale(
      limits = c(0, 180), breaks = seq(0, 180, 30),
      name = paste0("O3\n", longparameter("O3_max_98p_m1"), "\n(\u00b5g/m3)")
    ),
    `O3_peakseason_mean_d1_max_mean_h8gl` = immission_colorscale(
      limits = c(0, 110), breaks = seq(10, 110, 20),
      name = paste0(
        "O3\n", longparameter("O3_peakseason_mean_d1_max_mean_h8gl"), "\n(\u00b5g/m3)"
      )
    ),
    PM10 = immission_colorscale(
      limits = c(0, 34), breaks = c(seq(0, 30, 10), 34), name = "PM10\n(\u00b5g/m3)"
    ),
    PM2.5 = immission_colorscale(
      limits = c(0, 17), breaks = c(seq(0, 15, 2.5), 17), name = "PM2.5\n(\u00b5g/m3)"
    ),
    eBC = immission_colorscale(
      limits = c(0, 1.5), breaks = seq(0, 1.5, 0.3), name = "eBC\n(\u00b5g/m3)"
    ),
    # nitrogen uses the sequential magma ramp, reversed: dark is high
    NH3 = scale_fill_capped(
      palette = "Magma", direction = -1, na.value = NA,
      limits = c(1, 7), breaks = seq(1, 7, 2), name = "NH3\n(\u00b5g/m3)"
    ),
    Ndep = scale_fill_capped(
      palette = "Magma", direction = -1, na.value = NA,
      limits = c(0, 30), breaks = seq(0, 30, 5), name = "Ndep > CLN\n(kgN/ha/Jahr)"
    ),
    cli::cli_abort(c(
      "x" = "No scale defined for parameter {.val {parameter}}.",
      "i" = "Available: {.val {immissionscale_parameters()}}"
    ))
  )
}

#' Parameters that [immissionscale()] knows
#'
#' @return Character vector of parameter codes.
#'
#' @examples
#' immissionscale_parameters()
#'
#' @export
immissionscale_parameters <- function() {
  c(
    "NO2", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl",
    "PM10", "PM2.5", "eBC", "NH3", "Ndep"
  )
}

#' Colour palettes for emission figures
#'
#' Sequential ramps per emission sector group, so a sector keeps its colour
#' family across figures.
#'
#' @param n Number of colours.
#' @param name Palette name: `"Gray"`, `"Purple"`, `"Blue"`, `"Green"`, `"Gold"`
#'   or `"natural"`.
#'
#' @return Character vector of `n` colours.
#'
#' @examples
#' pal_emissions(4, "Green")
#'
#' @export
pal_emissions <- function(n, name = c("Gray", "Purple", "Blue", "Green", "Gold", "natural")) {
  name <- rlang::arg_match(name)
  colours <- switch(name,
    "Gray" = c("gray10", "gray90"),
    "Purple" = c("#3c096c", "#5a189a", "#7b2cbf", "#9d4edd", "#c77dff"),
    "Blue" = c("#293961", "#2c497f", "#8897bd", "#e3e4fa"),
    "Green" = c("#354f52", "#52796f", "#84a98c", "#cad2c5"),
    "Gold" = c("#a67c00", "#ffbf00", "#ffd447"),
    "natural" = c("#636b2f", "#d4de94")
  )

  grDevices::colorRampPalette(colours)(n)
}
