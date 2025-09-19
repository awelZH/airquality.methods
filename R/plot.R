
#' Plot timeseries yearly using bars
#'
#' @param data
#' @param mapping
#' @param ylims
#' @param ybreaks
#' @param titlelab
#' @param captionlab
#' @param pointshape
#' @param pointsize
#' @param threshold
#' @param theme
#'
#' @keywords internal
ggplot_timeseries <- function(data, mapping = ggplot2::aes(x = year, y = concentration, color = siteclass), ylims = c(0,NA), ybreaks = waiver(), titlelab = NULL, captionlab = NULL, pointshape = 19, pointsize = 2,
                              threshold = list(value = NA, color = "gray30", label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                              theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    ggplot2::geom_point(size = pointsize, shape = pointshape) +
    # ggiraph::geom_point_interactive(mapping = ggplot2::aes(data_id = site, tooltip = round_off(value, 1)), size = pointsize, shape = pointshape) +
    ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = ylims, breaks = ybreaks, expand = c(0.01,0.01)) +
    titlelab +
    captionlab +
    theme

  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = rep(min(data$year), length(threshold$value)), y = threshold$value, label = threshold$labels)
    plot <-
      plot +
      ggplot2::geom_hline(yintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = y, label = label), size = threshold$labelsize,
                         hjust = 0, vjust = 0, nudge_y = pmax(0, 0.01 * max(ylims), na.rm = TRUE), inherit.aes = FALSE)
  }

  # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 4)

  return(plot)
}


#' Plot timeseries using lines
#'
#' @param data
#' @param mapping
#' @param ylims
#' @param ybreaks
#' @param titlelab
#' @param captionlab
#' @param pointshape
#' @param pointsize
#' @param threshold
#' @param theme
#'
#' @keywords internal
ggplot_timeseries_lines <- function(data, mapping = ggplot2::aes(x = year, y = population_weighted_mean), ylims = c(0,NA), ybreaks = waiver(), titlelab = NULL, captionlab = NULL,
                                    theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    # ggplot2::geom_bar(stat = "identity", color = NA, fill = "#50586C") +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = seq(1990,2100,5), expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = ylims, breaks = ybreaks, expand = c(0.01,0.01)) +
    colorspace::scale_color_discrete_sequential(name = "Kanton", palette = "Viridis") +
    titlelab +
    captionlab +
    theme

  # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 4)

  return(plot)
}



#' Plot bar timeseries for yearly population-weighted mean concentration or health-outcome data using ggplot2
#'
#' @param data
#' @param mapping
#' @param ylims
#' @param ybreaks
#' @param titlelab
#' @param captionlab
#' @param pointshape
#' @param pointsize
#' @param threshold
#' @param theme
#'
#' @keywords internal
ggplot_timeseries_bars <- function(data, mapping = ggplot2::aes(x = year, y = population_weighted_mean, fill = scenario), ylims = c(NA,NA), ybreaks = waiver(), titlelab = NULL, captionlab = NULL,
                                   theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = 0, color = "gray30", linetype = 2) +
    ggplot2::scale_x_continuous(breaks = seq(1990,2100,5), expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = ylims, breaks = ybreaks, expand = c(0.01,0.01)) +
    scale_fill_manual(name = "Szenario", values = c("#50586C", "#DCE2F0")) +
    titlelab +
    captionlab +
    theme

  # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 4)

  return(plot)
}





#' Plot exposition distribution histogram using ggplot2
#'
#' @param data
#' @param x
#' @param y
#' @param barwidth
#' @param xlims
#' @param xbreaks
#' @param titlelab
#' @param captionlab
#' @param xlabel
#' @param threshold
#' @param fill_scale
#' @param theme
#'
#' @keywords internal
ggplot_expo_hist <- function(data, x, y, barwidth = 1, xlims = c(0,NA), xbreaks = waiver(), titlelab = NULL, captionlab = NULL, xlabel = NULL,
                             threshold = list(value = NA, label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                             fill_scale = NULL, theme = ggplot2::theme_minimal()) {

  if (is.null(fill_scale)) {
    mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))
    bars <- ggplot2::geom_bar(stat = "identity", color = NA, width = barwidth, fill = "#50586C")
  } else {
    mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), fill = !!rlang::sym(x))
    bars <- ggplot2::geom_bar(stat = "identity", color = NA, width = barwidth)
  }

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    bars +
    ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'", scientific = FALSE)) +
    fill_scale +
    xlabel +
    titlelab +
    captionlab +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_text())

  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = threshold$value, label = threshold$labels)
    plot <-
      plot +
      ggplot2::geom_vline(xintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = 0, label = label), size = threshold$labelsize,
                         hjust = 0, vjust = 0, angle = 90, nudge_x = pmin(0, -0.01 * max(xlims), na.rm = TRUE), inherit.aes = FALSE)
  }

  return(plot)
}




### function to
#' Plot relative cumulative exposition distribution using ggplot2
#'
#' @param data
#' @param x
#' @param y
#' @param linewidth
#' @param xlims
#' @param xbreaks
#' @param titlelab
#' @param captionlab
#' @param xlabel
#' @param threshold
#' @param theme
#'
#' @keywords internal
ggplot_expo_cumulative <- function(data, x, y, linewidth = 1, xlims = c(0,NA), xbreaks = waiver(), titlelab = NULL, captionlab = NULL, xlabel = NULL,
                                   threshold = list(value = NA, label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                                   theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
    ggplot2::geom_line(linewidth = linewidth, color = "#50586C") +
    ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0.01,0.01), labels = scales::percent_format()) +
    xlabel +
    titlelab +
    captionlab +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_text())

  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = threshold$value, label = threshold$labels)
    plot <-
      plot +
      ggplot2::geom_vline(xintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = 0, label = label), size = threshold$labelsize,
                         hjust = 0, vjust = 0, angle = 90, nudge_x = pmin(0, -0.01 * max(xlims), na.rm = TRUE), inherit.aes = FALSE)
  }

  return(plot)
}




#' Plot emission budget data employing structured coloring using ggplot2 ... this is not ideal, but the best I can do
#'
#' @param data
#' @param cols
#' @param relative
#' @param pos
#' @param width
#' @param theme
#'
#' @keywords internal
ggplot_emissions <- function(data, relative = FALSE, pos = "stack", width = 0.8, theme = ggplot2::theme_minimal()) {

  pollutant <- unique(as.character(data$pollutant))
  metric <- unique(as.character(data$metric))
  unit <- unique(as.character(data$unit))

  if (relative) {
    yscale <- ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0,0))
    sub <- openair::quickText(paste0(pollutant, ", ", metric, " nach Quellgruppen (relativ)"))
  } else {
    yscale <- ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = "'"), expand = c(0,0))
    sub <- openair::quickText(paste0(pollutant, ", ", metric, " nach Quellgruppen (", unit, ")"))
  }

  data <- dplyr::mutate(data, subsector_new2 = paste0(sector, " / ", subsector_new))
  lvls <-
    data |>
    dplyr::distinct(subsector_new2, order) |>
    dplyr::arrange(order) |>
    dplyr::pull(subsector_new2)
  data <- dplyr::mutate(data, subsector_new2 = factor(subsector_new2, levels = !!lvls))

  # order <-
  #   data |>
  #   dplyr::group_by(sector, subsector_new2) |>
  #   dplyr::summarise(emission = mean(emission)) |>
  #   dplyr::ungroup() |>
  #   dplyr::arrange(sector, emission) |>
  #   dplyr::ungroup()
  #
  # data <-
  #   data |>
  #   dplyr::mutate(subsector_new2 = factor(subsector_new2, levels = order$subsector_new2)) |>
  #   dplyr::mutate(rootcol = dplyr::recode(sector, !!!cols)) |>
  #   dplyr::arrange(sector, dplyr::desc(emission))
  #
  # cols <-
  #   data |>
  #   dplyr::distinct(subsector_new, rootcol) |>
  #   dplyr::group_by(rootcol) |>
  #   dplyr::mutate(col = pal_emissions(n = length(.data$rootcol), name = unique(.data$rootcol))) |>
  #   dplyr::ungroup()
  #
  # data <- dplyr::left_join(data, cols, by = c("subsector_new", "rootcol"))

  plot <-
    data |>
    ggplot2::ggplot(aes(x = year, y = emission, fill = subsector_new2)) +
    ggplot2::geom_bar(stat = "identity", position = pos, width = width) +
    # ggiraph::geom_bar_interactive(mapping = ggplot2::aes(data_id = subsector_new, tooltip = round_off(emission, 1)), stat = "identity", position = pos, width = width) +
    yscale +
    ggplot2::scale_fill_manual(values = setNames(data$col, data$subsector_new2)) +
    theme +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::ggtitle(
      label = openair::quickText(paste0("Luftschadstoff-Emissionen ", longpollutant(pollutant))),
      subtitle = sub
    ) +
    ggplot2::labs(caption = "Daten: Ostluft, Grundlage: EMIS Schweiz")

  # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 3)

  return(plot)
}




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


#' Wrapper to supply pollutant-specific list of parameters for timeseries plotting
#'
#' @param parameter
#'
#' @keywords internal
timeseriespars <- function(parameter) {
  switch(parameter,
         NO2 = list(ylim = c(0,70), ybreaks = seq(0,70,10), metric = "Jahresmittel", thresh = extract_threshold(immission_threshold_values, pollutant = "NO2")),
         PM10 = list(ylim = c(0,35), ybreaks = seq(0,35,5), metric = "Jahresmittel", thresh = extract_threshold(immission_threshold_values, pollutant = "PM10")),
         PM2.5 = list(ylim = c(0,20), ybreaks = seq(0,20,4), metric = "Jahresmittel", thresh = extract_threshold(immission_threshold_values, pollutant = "PM2.5")),
         eBC = list(ylim = c(0,4), ybreaks = seq(0,4,0.5), metric = "Jahresmittel", thresh = list(value = NA)),
         `O3_max_98p_m1` = list(ylim = c(0,210), ybreaks = seq(0,210,30), metric = "höchstes 98%-Perzentil der Halbstundenmittel eines Monats", thresh = extract_threshold(immission_threshold_values, pollutant = "O3", metric = "typische Spitzenbelastung", source = "LRV Grenzwert")),
         O3_peakseason_mean_d1_max_mean_h8gl = list(ylim = c(0,130), ybreaks = seq(0,120,20), metric = "mittlere tägliche max. 8-Stundenmittel während der Sommersaison", thresh = extract_threshold(immission_threshold_values, pollutant = "O3", metric = "mittlere Sommertagbelastung", source = "WHO Richtwert"))
  )
}



#' Wrapper to supply pollutant-specific list of parameters for exposition plotting
#'
#' @param parameter
#'
#' @keywords internal
expositionpars <- function(parameter) {
  wscale <- 0.9
  switch(parameter,
         NO2 = list(barwidth = 1 * wscale, xbreaks = seq(0,55,5), aggregation = "y1", metric = "mean"),
         `O3_max_98p_m1` = list(barwidth = 2 * wscale, xbreaks = seq(0,180,20), aggregation = "m1", metric = "monthly 98%-percentile of ½ hour mean values ≤ 100 µg/m3"),
         `O3_peakseason_mean_d1_max_mean_h8gl` = list(barwidth = 2 * wscale, xbreaks = seq(0,120,10), aggregation = "peak-season", metric = "mean of daily maximum 8-hour mean concentration in the six consecutive months with the highest six-month running-mean concentration"),
         PM10 = list(barwidth = 0.5 * wscale, xbreaks = seq(0,24,2), aggregation = "y1", metric = "mean"),
         PM2.5 = list(barwidth = 0.5 * wscale, xbreaks = seq(0,18.5,1), aggregation = "y1", metric = "mean"),
         eBC = list(barwidth = 0.05 * wscale, xbreaks = seq(0,2.2,0.2), aggregation = "y1", metric = "mean"),
         Ndep = list(barwidth = 1 * wscale, xlim = c(-5,90), xbreaks = seq(-5,45,5), aggregation = "y1", metric = "sum")
  )
}


#' Wrapper to plot timeseries yearly data using ggplot2 providing pollutant-specific list
#'
#' @param data
#' @param parameters
#' @param cap
#'
#' @keywords internal
plot_pars_monitoring_timeseries <- function(data, parameters, cap = "Datenabdeckung: Kanton Zürich, Daten: Ostluft & NABEL (BAFU & Empa)") {

  plots <-
    lapply(setNames(parameters, parameters), function(parameter) {

      data_plot <- dplyr::filter(data, parameter == !!parameter)
      pollutant <- unique(data_plot$pollutant)
      unit <- unique(data_plot$unit)
      metric <- unique(data_plot$metric)

      ggplot_timeseries(data_plot,
                        ylims = timeseriespars(parameter)$ylim, ybreaks = timeseriespars(parameter)$ybreaks,
                        titlelab = ggplot2::ggtitle(
                          label = openair::quickText(paste0("Luftqualitätsmesswerte ",longpollutant(pollutant))),
                          subtitle = openair::quickText(paste0(pollutant, ", ", metric," (", unit, ")"))
                        ),
                        captionlab = ggplot2::labs(caption = cap),
                        pointsize = pointsize, theme = theme_ts, threshold = timeseriespars(parameter)$thresh
      ) +
        scale_color_siteclass

    })

  return(plots)
}



#' Plot yearly nitrogen deposition timeseries using ggplot2
#'
#' @param data
#' @param xlim
#' @param xbreaks
#' @param linewidth
#' @param color
#' @param title
#'
#' @keywords internal
plot_timeseries_ndep_bars <- function(data, xlim = NULL, xbreaks = waiver(), linewidth = 1, color = "red3", title = "Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme") {

  cln <-
    data |>
    dplyr::distinct(site, ecosystem_category, critical_load_min, critical_load_single, critical_load_max) |>
    tidyr::gather(cln, deposition, -site, -ecosystem_category) |>
    dplyr::filter(cln == "critical_load_single") # decided to show only single value, but still keep option for range display

  plot <-
    data |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = deposition, fill = component)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(data = cln, mapping = ggplot2::aes(yintercept = deposition, linetype = cln, group = site), color = color, linewidth = linewidth, show.legend = FALSE) +
    ggplot2::scale_linetype_manual(values = c("critical_load_single" = 1, "critical_load_min" = 2, "critical_load_max" = 2)) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(expand = c(0.01,0.01)) +
    ggplot2::scale_fill_manual(values = c("aus NH3-Quellen" = "khaki3", "aus NOx-Quellen" = "khaki4")) +
    theme_ts +
    ggplot2::theme(
      strip.text = ggplot2::element_text(hjust = 0),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::ggtitle(
      label = openair::quickText(title),
      subtitle = expression("Stickstoffeintrag (kgN " * ha^-1 * Jahr^-1 * ")")
    )

  return(plot)
}



#' Wrapper to plot pollutant-specific population exposition histograms in a nested list by pollutant & year
#'
#' @param parameter
#' @param data
#'
#' @keywords internal
plot_all_expo_hist <- function(parameter, data, sub = "im Kanton Zürich") {

  data <- dplyr::filter(data, parameter == !!parameter)
  years_exposition <- setNames(unique(data$year), as.character(unique(data$year)))
  plots <- lapply(years_exposition, function(year) {

    data_plot <- dplyr::filter(data, year == !!year & parameter == !!parameter)
    pollutant <- unique(data_plot$pollutant)
    metric <- unique(data_plot$metric)
    thresh <- extract_threshold(immission_threshold_values, pollutant, metric)

    ggplot_expo_hist(
      data = data_plot, x = "concentration", y = "population", barwidth = expositionpars(parameter)$barwidth,
      xlims = range(expositionpars(parameter)$xbreaks), xbreaks = expositionpars(parameter)$xbreaks, threshold = thresh,
      xlabel = ggplot2::xlab(openair::quickText(paste0(pollutant, " ", metric, " (µg/m3)"))),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText(paste0("Bevölkerungsexposition ", longpollutant(pollutant))),
        subtitle = paste0("Anzahl Personen, Wohnbevölkerung ",sub," im Jahr ",year)
      ),
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      # fill_scale = immissionscale(parameter),
      theme = theme_ts
    ) +
      ggplot2::theme(legend.position = "none")

  })

  return(plots)
}



#' Wrapper to plot pollutant-specific cumulated population exposition distribution in a nested list by pollutant & year
#'
#' @param parameter
#' @param data
#'
#' @keywords internal
plot_all_expo_cumul <- function(parameter, data, sub = "im Kanton Zürich") {

  data <- dplyr::filter(data, parameter == !!parameter)
  years_exposition <- setNames(unique(data$year), as.character(unique(data$year)))
  plots <- lapply(years_exposition, function(year) {

    data_plot <- dplyr::filter(data, year == !!year & parameter == !!parameter)
    pollutant <- unique(data_plot$pollutant)
    metric <- unique(data_plot$metric)
    thresh <- extract_threshold(immission_threshold_values, pollutant, metric)
    ggplot_expo_cumulative(
      data = data_plot, x = "concentration", y = "population_cum_rel", linewidth = 1,
      xlims = range(expositionpars(parameter)$xbreaks), xbreaks = expositionpars(parameter)$xbreaks, threshold = thresh,
      xlabel = ggplot2::xlab(openair::quickText(paste0(pollutant," ",metric," (µg/m3)"))),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText(paste0("Bevölkerungsexposition ",longpollutant(parameter))),
        subtitle = openair::quickText(paste0("relativer Anteil (kumuliert), Wohnbevölkerung ",sub," im Jahr ",year))
      ),
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      theme = theme_ts
    )

  })

  return(plots)
}


#' Wrapper to plot timeseries of population-weighted mean concentration data using ggplot2 providing pollutant-specific list
#'
#' @param data
#' @param parameters
#' @param version
#' @param id_subareas
#' @param y
#'
#' @keywords internal
plot_pars_popmean_timeseries <- function(data, parameters, version = "overall", id_subareas = NULL, y = "population_weighted_mean") {

  if (version == "overall") {

    data <-
      data |>
      dplyr::mutate(delta_base = pmin(population_weighted_mean - population_weighted_mean_base, 0)) |>
      dplyr::select(year, pollutant, parameter, delta_base, population_weighted_mean, base_year) |>
      tidyr::gather(scenario, population_weighted_mean, -year, -pollutant, -parameter, -base_year) |>
      dplyr::mutate(scenario = dplyr::recode(scenario, population_weighted_mean = "tatsächliche Belastung", delta_base = paste0("vermindert vs. ",na.omit(unique(.data$base_year)))))

    plots <-
      lapply(setNames(parameters, parameters), function(parameter) {

        data_plot <- dplyr::filter(data, parameter == !!parameter)
        pollutant <- unique(data_plot$pollutant)
        ggplot_timeseries_bars(data_plot,
                               mapping = ggplot2::aes(x = year, y = !!rlang::sym(y), fill = scenario),
                               titlelab = ggplot2::ggtitle(
                                 label = openair::quickText(paste0("Bevölkerungsgewichtete Schadstoffbelastung ",longpollutant(pollutant))),
                                 subtitle = openair::quickText(paste0(pollutant,", mittlere Schadstoffbelastung pro Einwohner/in (µg/m3)"))
                               ),
                               captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
                               theme = theme_ts
        )

      })

  }

  if (version == "subareas") {

    data <-
      data |>
      dplyr::select(!!id_subareas, year, pollutant, parameter, population_weighted_mean) |>
      tidyr::gather(scenario, population_weighted_mean, -!!id_subareas, -year, -pollutant, -parameter)

    plots <-
      lapply(setNames(parameters, parameters), function(parameter) {

        data_plot <- dplyr::filter(data, parameter == !!parameter)
        pollutant <- unique(data_plot$pollutant)
        ggplot_timeseries_lines(data_plot,
                                mapping = ggplot2::aes(x = year, y = !!rlang::sym(y), color = !!rlang::sym(id_subareas)),
                                titlelab = ggplot2::ggtitle(
                                  label = openair::quickText(paste0("Bevölkerungsgewichtete Schadstoffbelastung ",longpollutant(pollutant))),
                                  subtitle = openair::quickText(paste0(pollutant,", mittlere Schadstoffbelastung pro Einwohner/in (µg/m3)"))
                                ),
                                captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
                                theme = theme_ts
        )


      })
  }

  return(plots)
}


#' Wrapper to plot timeseries of health-outcome preliminary deaths using ggplot2 providing pollutant-specific list
#'
#' @param data
#' @param parameters
#' @param relative
#'
#' @keywords internal
plot_pars_prelim_deaths_timeseries <- function(data, parameters, relative = FALSE) {

  plots <-
    lapply(setNames(parameters, parameters), function(parameter) {

      data <-
        data |>
        dplyr::filter(parameter == !!parameter & outcome_type == "vorzeitige Todesfälle") |>
        dplyr::mutate(
          covid = ifelse(year %in% 2020:2022, "Covid-19", "normal")
        )

      if (relative) {
        mppng <- ggplot2::aes(x = year, y = outcome / population * 10^5, fill = scenario, alpha = covid)
        sub <- "Anzahl vorzeitige Todesfälle pro 100'000 Einwohner/innen pro Jahr"
        uncertainty <- ggplot2::geom_linerange(ggplot2::aes(ymin = outcome_lower / population * 10^5, ymax = outcome_upper / population * 10^5 + outcome_delta_min_conc / population * 10^5), color = "gray20")
      } else {
        mppng <- ggplot2::aes(x = year, y = outcome, fill = scenario, alpha = covid)
        sub <- "Anzahl vorzeitige Todesfälle pro Jahr"
        uncertainty <- ggplot2::geom_linerange(ggplot2::aes(ymin = outcome_lower, ymax = outcome_upper + outcome_delta_min_conc), color = "gray20")
      }

      plot <-
        data |>
        ggplot_timeseries_bars(
          mapping = mppng,
          titlelab = ggplot2::ggtitle(
            label = openair::quickText(paste0("Vorzeitige Todesfälle durch ",longpollutant(parameter))),
            subtitle = sub
          ),
          captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS & Statistisches Amt Kanton Zürich"),
          theme = theme_ts
        ) +
        uncertainty +
        ggplot2::scale_alpha_manual(name = "Aussergewöhnliches", values = c("normal" = 1, "Covid-19" = 0.25))

      return(plot)
    })

  return(plots)
}


#' Wrapper to plot pollutant-specific population weighted mean maps by municipality in a nested list by pollutant & year
#'
#' @param parameter
#' @param data
#' @param data_canton
#'
#' @keywords internal
plot_all_popweighmean_maps <- function(parameter, data, data_canton) {

  data <- dplyr::filter(data, parameter == !!parameter)
  years_exposition <- setNames(unique(data$year), as.character(unique(data$year)))
  plots <- lapply(years_exposition, function(year) {

    canton <- round_off(dplyr::pull(dplyr::filter(data_canton, year == !!year & parameter == !!parameter), "population_weighted_mean"), 1)
    data_plot <- dplyr::filter(data, year == !!year & parameter == !!parameter)
    pollutant <- unique(data_plot$pollutant)

    plot <-
      data_plot |>
      ggplot2::ggplot(ggplot2::aes(fill = population_weighted_mean)) +
      ggplot2::geom_sf() +
      # ggiraph::geom_sf_interactive(mapping = ggplot2::aes(data_id = gemeindename, tooltip = paste0(gemeindename, ", ", round_off(population_weighted_mean, 1)))) +
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      immissionscale(parameter) +
      theme_map +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::ggtitle(
        label = openair::quickText(paste0("Bevölkerungsgewichtete Schadstoffbelastung ",longpollutant(pollutant))),
        subtitle = openair::quickText(paste0("Mittlere ",pollutant,"-Belastung pro Einwohner/in im Jahr ",year,"\ngesamt = ",canton," µg/m3"))
      ) +
      ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")

    # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 5, options = list(ggiraph::opts_hover_inv(css = "opacity:0.5;")))

    return(plot)
  })

  return(plots)
}



#' Wrapper to plot sensitive ecosystem nitrogen deposition exposition histograms in a list by year
#'
#' @param data
#' @param threshold_ndep
#'
#' @keywords internal
plot_all_expo_hist_ndep <- function(data, threshold_ndep, sub = "im Kanton Zürich") {

  years_exposition <- setNames(unique(data$year), as.character(unique(data$year)))

  plots <- lapply(years_exposition, function(year) {

    ggplot_expo_hist(
      data = dplyr::filter(data, year == !!year), x = "ndep_exmax", y = "n_ecosys", barwidth = expositionpars("Ndep")$barwidth,
      xlims = range(expositionpars("Ndep")$xbreaks), xbreaks = expositionpars("Ndep")$xbreaks, threshold = threshold_ndep,
      xlabel = ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge"),
        subtitle = paste0("Anzahl empfindlicher Ökosysteme ",sub," im Jahr ", year)
      ),
      captionlab = ggplot2::labs(caption = "Daten: BAFU"),
      # fill_scale = immissionscale("Ndep"),
      theme = theme_ts
    )

  })

  return(plots)
}



#' Wrapper to plot sensitive ecosystem nitrogen deposition cumulative exposition distribution in a list by year
#'
#' @param data
#' @param threshold_ndep
#'
#' @keywords internal
plot_all_expo_cumul_ndep <- function(data, threshold_ndep, sub = "im Kanton Zürich") {

  years_exposition <- setNames(unique(data$year), as.character(unique(data$year)))

  plots <- lapply(years_exposition, function(year) {

    ggplot_expo_cumulative(
      data = dplyr::filter(data, year == !!year), x = "ndep_exmax", y = "n_ecosys_cum_rel", linewidth = 1,
      xlims = range(expositionpars("Ndep")$xbreaks), xbreaks = expositionpars("Ndep")$xbreaks, threshold = threshold_ndep,
      xlabel = ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge"),
        subtitle = paste0("relativer Anteil empfindlicher Ökosysteme (kumuliert) ",sub," im Jahr ", year)
      ),
      captionlab = ggplot2::labs(caption = "Daten: BAFU"),
      theme = theme_ts
    )

  })

  return(plots)
}



#' Merge air pollution dataset with corresponding threshold limit values dataset
#'
#' @param data
#' @param threshold_values
#'
#' @keywords internal
combine_thresholds <- function(data, threshold_values) {

  data <-
    threshold_values |>
    dplyr::select(source, pollutant, metric_description, interval, threshold) |>
    dplyr::rename(metric = metric_description) |>
    tidyr::spread(source, threshold) |>
    dplyr::right_join(data, by = c("pollutant", "metric")) |>
    dplyr::select(year, site, pollutant, metric, parameter, interval, unit, concentration, siteclass, `LRV Grenzwert`, `WHO Richtwert`, source)

  return(data)
}



#' Restructures list of plots to a tibble including plots
#'
#' @param plotlist
#' @param type
#' @param source
#'
#' @keywords internal
plotlist_to_tibble <- function(plotlist, type, source) {

  if (!is.na(extract_year(names(plotlist[[1]][1]))) | names(plotlist[[1]][1]) == "alle") {

    plottibble <-
      plotlist |>
      names() |>
      purrr::map(function(x) {
        plotlist[[x]] |>
          tibble::enframe(name = "pollutant", value = "plot") |>
          dplyr::mutate(
            pollutant = x,
            type = !!type,
            source = !!source,
            year = names(plotlist[[x]])
          )
      }) |>
      dplyr::bind_rows()

  } else {

    plottibble <-
      plotlist |>
      tibble::enframe(name = "pollutant", value = "plot") |>
      dplyr::mutate(
        type = !!type,
        source = !!source,
        year = "various"
      )

  }

  return(plottibble)
}



#' Plot pre-compiled relative trends of emissions and immissions vs. a reference year for various pollutants
#'
#' @param data_trends
#' @param detailed
#' @param pt_size
#' @param linewdth
#' @param facet_ncol
#' @param facet_scale
#' @param theme
#' @param titlelab
#' @param captionlab
#'
#' @keywords internal
plot_timeseries_trend_relative <- function(data_trends, detailed = FALSE,
                                           pt_size = 1.5, linewdth = 1, facet_ncol = NULL, facet_scale = "free_y", theme = ggplot2::theme_minimal(),
                                           titlelab = NULL, captionlab = NULL
) {

  plot <-
    data_trends |>
    ggplot(aes(x = year, y = `relative Immission` - 1, color = type)) +
    geom_hline(yintercept = 0, color = "gray80", linetype = 2) +
    geom_vline(data = . %>% dplyr::distinct(pollutant, reference_year), mapping = aes(xintercept = reference_year), color = "gray80", linetype = 2)

  if (detailed) {

    plot <-
      plot +
      geom_point(data = . %>% dplyr::filter(type != "Median Trend"), mapping = aes(size = type, shape = type), fill = "white") +
      geom_line(data = . %>% dplyr::filter(type != "Median Messwerte"), mapping = aes(linewidth = type, group = site))
    # geom_point(mapping = aes(size = n), shape = 21, fill = "white") +
    # scale_size_binned(name = "Anzahl\nMessorte", breaks = c(-Inf,4,6,8,Inf), range = c(0.25,3)) +

  } else {

    plot <-
      plot +
      geom_line(mapping = aes(linewidth = type))

  }

  plot <-
    plot  +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0.01,0.01)) +
    scale_color_manual(name = "Grundlage", values = c("Median Trend" = "steelblue", "Median Messwerte" = "gold3", "Trend pro Standort" = "gray80")) +
    scale_shape_manual(values = c("Median Messwerte" = 21, "Trend pro Standort" = 19)) +
    scale_size_manual(values = c("Median Messwerte" = pt_size, "Trend pro Standort" = pt_size * 0.75)) +
    scale_linewidth_manual(values = c("Median Trend" = linewdth, "Median Messwerte" = linewdth * 0.5, "Trend pro Standort" = linewdth * 0.5)) +
    guides(shape = "none", size = "none", linewidth = "none") +
    facet_wrap(pollutant~., axes = "all", ncol = facet_ncol, scales = facet_scale) +
    theme +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(hjust = 0),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    titlelab +
    captionlab

  return(plot)
}
