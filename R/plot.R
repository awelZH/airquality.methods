
#' Provide official colors for plotting air quality raster data
#'
#' @param ...
#'
#' @keywords internal
immission_colorscale <- function(...) {
  cols <- c("#004DA8", "#005ce6", "#0070ff", "#00c5ff", "#47d9fa", "#56f9fb", "#2e9c6b", "#38bd00", "#56d900",
            "#51f551", "#ffff00", "#ffd400", "#ffa300", "#ff5200", "#ff0000", "#ff0094", "#de00a1", "#c500ba")
  return(rOstluft.plot::scale_fill_gradientn_squished(..., colors = cols, na.value = NA))
}



#' Wrapper to supply pollutant-specific raster data color scales for ggplot2
#'
#' @param parameter
#'
#' @keywords internal
immissionscale <- function(parameter) {
  switch(parameter,
         NO2 = immission_colorscale(limits = c(0,50), breaks = seq(0,50,10), name = "NO2\n(µg/m3)"),
         `O3_max_98p_m1` = immission_colorscale(limits = c(0,180), breaks = seq(0,180,30), name = paste0("O3\n",longparameter("O3_max_98p_m1"),"\n(µg/m3)")),
         `O3_peakseason_mean_d1_max_mean_h8gl` = immission_colorscale(limits = c(0,110), breaks = seq(10,110,20), name = paste0("O3\n",longparameter("O3_peakseason_mean_d1_max_mean_h8gl"),"\n(µg/m3)")),
         PM10 = immission_colorscale(limits = c(0,34), breaks = c(seq(0,30,10), 34), name = "PM10\n(µg/m3)"),
         PM2.5 = immission_colorscale(limits = c(0,17), breaks = c(seq(0,15,2.5), 17), name = "PM2.5\n(µg/m3)"),
         eBC = immission_colorscale(limits = c(0,1.5), breaks = seq(0,1.5,0.3), name = "eBC\n(µg/m3)"),
         NH3 = rOstluft.plot::scale_fill_viridis_squished(name = "NH3\n(µg/m3)", limits = c(1, 7), breaks = seq(1, 7, 2), direction = -1,  option = "A", na.value = NA),
         Ndep = rOstluft.plot::scale_fill_viridis_squished(name = "Ndep > CLN\n(kgN/ha/Jahr)", limits = c(0, 30), breaks = seq(0, 30, 5), direction = -1, option = "A", na.value = NA)
  )
}


#' Provides custom color scales for emissions
#'
#' @param n
#' @param name
#'
#' @keywords internal
pal_emissions <- function(n, name) {
  pal <- switch(name,
                "Gray" = colorRampPalette(c("gray10","gray90")),
                "Purple" = colorRampPalette(c("#3c096c","#5a189a","#7b2cbf","#9d4edd","#c77dff")),
                "Blue" = colorRampPalette(c("#293961","#2c497f","#8897bd","#e3e4fa")),
                "Green" = colorRampPalette(c("#354f52","#52796f","#84a98c","#cad2c5")),
                "Gold" = colorRampPalette(c("#a67c00","#ffbf00","#ffd447")),
                "natural" = colorRampPalette(c("#636b2f","#d4de94"))
  )
  return(pal(n))
}

theme_legend_inside <- function(pos = c(0.975, 0.95), just = c(1, 1), mar = margin(4, 4, 4, 4)) {

  theme(
    legend.position  = "inside",
    legend.position.inside = pos,   # x, y: 0–1 within panel
    legend.justification.inside = just,    # anchor corner of the legend box
    legend.background = element_rect(fill = alpha("white", 0.6), color = "gray50"),
    legend.margin = mar
  )

}


theme_custom <- function(type = "timeseries", linecolor = "gray30") {

  default <- theme_get()
  default <-
    default +
    theme(
      plot.title = element_text(size = rel(0.8)),
      plot.subtitle = element_text(size = rel(0.8)),
      axis.line = element_line(color = linecolor),
      axis.ticks = element_line(color = linecolor),
      strip.text = element_text(size = rel(0.8), hjust = 0),
      legend.spacing.y = unit(0,"mm"),
      plot.caption = element_text(hjust = 0, size = rel(0.5), color = "gray50")
    )

  if (type == "timeseries") {

    default +
      theme(
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )

  } else if (type == "scatter") {

    default +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

  } else if (type == "flipped") {

    default +
      theme(
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_blank()
      )

  } else if (type == "map") {

    default +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(size = rel(0.8), hjust = 0.5)
      )

  } else {

    default

  }

}
