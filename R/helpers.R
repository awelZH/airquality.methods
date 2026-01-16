
#' Get entry from ressources.csv by internal id
#'
#' @param ressources
#' @param internal_id
#'
#' @keywords internal
filter_ressources <- function(ressources, internal_id) {

  filters <- paste0("INTERNAL_ID == ", internal_id)
  ressource <- dplyr::filter(ressources, eval(rlang::parse_expr(filters)))
  ressource <- dplyr::pull(ressource, get)

  return(ressource)
}


#' Extract target threshold values from threshold table for plotting
#'
#' @param threshold_values
#' @param pollutant
#' @param aggregation
#' @param metric
#' @param unit
#' @param source
#'
#' @keywords internal
extract_threshold <- function(threshold_values, pollutant = NULL, metric = "Jahresmittel", interval = "y1", unit = "µg/m3",
                              source = c("LRV Grenzwert", "WHO Richtwert")) {

  thresholds <-
    threshold_values |>
    dplyr::filter(
      source %in% !!source &
        pollutant == !!pollutant & interval == !!interval &
        metric_description == !!metric & unit == !!unit
    ) |>
    dplyr::arrange(source)

  thresholds <-
    list(
      value = thresholds$threshold,
      color = thresholds$col,
      labels = thresholds$source,
      labelsize = thresholds$lbsz,
      linetype = thresholds$lty,
      linesize = thresholds$lsz
    )

  return(thresholds)
}



#' Recode short pollutant/parameter string to long string
#'
#' @param x
#'
#' @keywords internal
longpollutant <- function(x) {

  long <- dplyr::case_when(
    x == "PM10" ~ "Feinstaub PM10",
    x == "PM2.5" ~ "Feinstaub PM2.5",
    x == "NO2" ~ "Stickstoffdioxid",
    x == "NO2 | NOx" ~ "Stickstoxide NO2 | NOx",
    x == "NMVOC" ~ "nicht-Methan Kohlenwasserstoffe",
    x == "NH3" ~ "Ammoniak",
    x == "NHx" ~ "reduzierter Stickstoff",
    x == "NHx | NH3" ~ "reduzierter Stickstoff NHx | NH3",
    x == "Nr" ~ "reaktiver Stickstoff",
    x == "CO" ~ "Kohlenstoffmonoxid",
    x == "SO2" ~ "Schwefeldioxid",
    x == "NOx" ~ "Stickoxide",
    x == "eBC" ~ "Russ im Feinstaub",
    x == "O3" ~ "Ozon",
    x == "O3_max_98p_m1" ~ "Ozon",
    x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "Ozon",
    x == "O3_nb_h1>120" ~ "Ozon",
    x == "N-Eintrag" ~ "Stickstoffeintrag in empfindliche Ökosysteme",
    x == "Ndep" ~ "Stickstoffeintrag in empfindliche Ökosysteme",
    TRUE ~ x
  )

  return(long)
}



#' Recode parameter string to short pollutant string
#'
#' @param x
#'
#' @keywords internal
shortpollutant <- function(x) {

  long <- dplyr::case_when(
    x == "O3_max_98p_m1" ~ "O3",
    x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "O3",
    x == "O3_nb_h1>120" ~ "O3",
    x == "O3_max_h1" ~ "O3",
    TRUE ~ x
  )

  return(long)
}


#' Recode short pollutant/parameter string to long metric string
#'
#' @param x
#'
#' @keywords internal
longmetric <- function(x, interval = "y1") {

  if (interval == "y1") {

    long <- dplyr::case_when(
      x == "PM10" ~ "Jahresmittel",
      x == "PM2.5" ~ "Jahresmittel",
      x == "NO2" ~ "Jahresmittel",
      x == "NH3" ~ "Jahresmittel",
      x == "NHx" ~ "Jahresmittel",
      x == "Nr" ~ "Jahresmittel",
      x == "NMVOC" ~ "Jahresmittel",
      x == "NH3" ~ "Jahresmittel",
      x == "CO" ~ "Jahresmittel",
      x == "SO2" ~ "Jahresmittel",
      x == "NOx" ~ "Jahresmittel",
      x == "eBC" ~ "Jahresmittel",
      x == "N-Eintrag" ~ "Jahressumme",
      x == "Ndep" ~ "Jahressumme",
      x == "O3" ~ "Jahresmittel",
      x == "O3_max_98p_m1" ~ "typische Spitzenbelastung",
      x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "mittlere Sommertagbelastung",
      # x == "O3_max_98p_m1" ~ "höchstes monatl. 98%-Perzentil der ½-Stundenmittel",
      # x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "mittlere sommerliche Tagesbelastung",
      x == "O3_nb_h1>120" ~ "Anzahl Stundenmittel > 120 μg/m3",
      TRUE ~ x
    )

  }

  if (interval == "d1") {

    long <- dplyr::case_when(
      x == "PM10" ~ "Tagesmittel",
      x == "PM2.5" ~ "Tagesmittel",
      x == "NO2" ~ "Tagesmittel",
      x == "NH3" ~ "Tagesmittel",
      x == "NMVOC" ~ "Tagesmittel",
      x == "NH3" ~ "Tagesmittel",
      x == "CO" ~ "Tagesmittel",
      x == "SO2" ~ "Tagesmittel",
      x == "NOx" ~ "Tagesmittel",
      x == "eBC" ~ "Tagesmittel",
      x == "O3" ~ "Tagesmittel",
      x == "O3_max_h1" ~ "höchstes Stundenmittel",
      TRUE ~ x
    )

  }

  return(long)
}


#' Recode short parameter string to long metric string
#'
#' @param x
#'
#' @keywords internal
longparameter <- function(x) {

  long <- dplyr::case_when(
    x == "O3_max_98p_m1" ~ "max. monatl. 98%-Perz.",
    x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "Sommersaison",
    x == "O3_max_h1" ~ "höchstes Stundenmittel",
    TRUE ~ "Jahresmittel"
  )

  return(long)
}


#' Extract numeric year from string
#'
#' @param string
#'
#' @keywords internal
extract_year <- function(string) {as.numeric(stringr::str_extract(string, "(1|2)[0-9]{3}"))}


#' Return function to bin concentration depending on parameter
#'
#' @param pollutant
#'
#' @keywords internal
bin_fun <- function(pollutant) {

  fun <- function(x) {floor(x) + 0.5} # default, e.g. NO2: abgerundet auf 1, Klassenmitte
  if (pollutant == "O3_max_98p_m1") {fun <- function(x) {floor(x * 0.5) / 0.5 + 1}} # abgerundet auf 2, Klassenmitte
  if (pollutant == "O3_peakseason_mean_d1_max_mean_h8gl") {fun <- function(x) {floor(x * 0.5) / 0.5 + 1}} # abgerundet auf 2, Klassenmitte
  if (pollutant == "PM10") {fun <- function(x) {floor(x * 2) / 2 + 0.25}} # abgerundet auf 0.5, Klassenmitte
  if (pollutant == "PM2.5") {fun <- function(x) {floor(x * 2) / 2 + 0.25}} # abgerundet auf 0.5, Klassenmitte
  if (pollutant == "eBC") {fun <- function(x) {floor(x * 20) / 20 + 0.025}} # abgerundet auf 0.05, Klassenmitte

  return(fun)
}



#' Write local *csv
#'
#' @param data
#' @param file
#' @param delim
#' @param na
#'
#' @keywords internal
write_local_csv <- function(data, file, delim = ";", na = "NA", append = FALSE){

  readr::write_delim(data, file, delim = delim, na = na, append = append)

}


#' Round to nearest whole number (kaufmännisches Runden)
#'
#' @param x
#' @param digits
#'
#' @keywords internal
round_off <- function (x, digits = 0) {

  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits

  return(z)
}


#' Extract polutant from BAFU data.geo.admin.ch ressource string
#'
#' @param id
#'
#' @keywords internal
extract_pollutant <- function(id) {

  pollutant <- dplyr::case_when(
    stringr::str_detect(id, "feinstaub_pm2_5") ~ "pm25",
    stringr::str_detect(id, "feinstaub_pm10") ~ "pm10",
    stringr::str_detect(id, "ozon") ~ "mp98",
    stringr::str_detect(id, "schwefeldioxid") ~ "so2",
    stringr::str_detect(id, "stickstoffdioxid") ~ "no2",
    stringr::str_detect(id, "stickstoff_kritischer_eintrag") ~ "ndep_exmax",
    TRUE ~ NA
  )

  return(pollutant)
}


#' Check previously analysed years and determine which new years should be analysed
#'
#' @param yearmax
#'
#' @keywords internal
get_years <- function(read_all_raster, yearmax, base_scenario_year) {

  if (all(read_all_raster & !any(is.numeric(read_all_raster)))) {

    years <- list(PM2.5 = 2015:yearmax, PM10 = 2010:yearmax, NO2 = 2010:yearmax, O3 = 2010:yearmax, ndep_exmax = 1990:yearmax, all = 2010:yearmax, base_analysed = FALSE) # since statpop raster data are only available from 2010 on and new data are usually published end of year for preceeding year

  } else if (all(!read_all_raster & !any(is.numeric(read_all_raster)))) {

    years <-
      read_local_csv("inst/extdata/output/data_exposition_weighted_means_canton.csv") |>
      dplyr::distinct(year, pollutant) |>
      dplyr::mutate(analysed = TRUE)

    if (base_scenario_year %in% years$year) {

      years <-
        years |>
        dplyr::mutate(
          analysed = ifelse(year == !!base_scenario_year, NA, analysed)
        )

    } else {

      years <- # make sure, base scenario year is always included to be downloaded
        tibble::tibble(
          pollutant = c("NO2", "O3", "PM10", "PM2.5"),
          year = base_scenario_year,
          analysed = NA
        ) |>
        dplyr::bind_rows(years)

    }

    years <-
      read_local_csv("inst/extdata/output/data_exposition_distribution_ndep.csv") |>
      dplyr::distinct(year) |>
      dplyr::mutate(
        pollutant = "ndep_exmax",
        analysed = TRUE
      ) |>
      dplyr::bind_rows(years)

    years <-
      years |>
      dplyr::group_by(pollutant) |>
      tidyr::expand(year = min(year):!!yearmax) |>
      dplyr::ungroup() |>
      dplyr::left_join(years, by = c("pollutant", "year")) |>
      dplyr::filter(is.na(analysed)) |>
      dplyr::select(-analysed) |>
      unstack(year ~ pollutant)

    years$all <- unique(unlist(years[names(years) != "ndep_exmax"]))
    years$base_analysed <- base_scenario_year %in% dplyr::distinct(read_local_csv("inst/extdata/output/data_exposition_weighted_means_canton.csv"), year)$year

  } else if (any(is.numeric(read_all_raster))) {

    years <- list(PM2.5 = read_all_raster, PM10 = read_all_raster, NO2 = read_all_raster, O3 = read_all_raster, ndep_exmax = read_all_raster, all = read_all_raster, base_analysed = FALSE) # since statpop raster data are only available from 2010 on and new data are usually published end of year for preceeding year

  }

  return(years)
}




#' Do site- and parameter-specific statistical meteo-normalisation based on package 'rmweather' from pre-compiled monitoring data in order to derive the meteo-normalised time-trend
#'
#' @param data
#' @param trend_vars
#' @param frac_train
#' @param ntrees
#' @param nsamples
#' @param verbose
#' @param minimal
#' @param coverage
#'
#' @export
rf_meteo_normalisation <- function(data, trend_vars, frac_train = 0.8, ntrees = 300, nsamples = 300, verbose = TRUE, minimal = TRUE, coverage = 0.8) {

  # see example at https://github.com/skgrange/rmweather
  # prepare, grow/train a random forest model and then create a meteorological normalised trend
  print(paste0("processing ", unique(data$site)))

  list_normalised <-
    data |>
    rmweather::rmw_prepare_data(na.rm = TRUE, fraction = frac_train) |>
    rmweather::rmw_do_all(
      variables = c("date_unix", "day_julian", "weekday", trend_vars),
      n_trees = ntrees,
      n_samples = nsamples,
      verbose = verbose
    )

  # Check model object's performance
  model_stats <- rmweather::rmw_model_statistics(list_normalised$model)

  # Plot variable importances
  imp <- rmweather::rmw_model_importance(list_normalised$model)

  # normalised trend and observations based in original interval
  data <-
    data |>
    dplyr::select(date, site, parameter, value) |>
    dplyr::left_join(list_normalised$normalised, by = "date") |>
    dplyr::rename(
      gemessen = value,
      Trend = value_predict
    ) |>
    tidyr::gather(type, value, -date, -site, -parameter) |>
    dplyr::mutate(type = factor(type, levels = c("gemessen", "Trend")))

  print(paste0("R2 = ", round_off(model_stats$r_squared,2)))
  print(imp)

  # normalised trend and observations based on yearly interval
  data_y1 <-
    data |>
    dplyr::group_by(year = lubridate::year(date), site, parameter, type) |>
    dplyr::summarise(
      n = sum(!is.na(value)),
      value = mean(value, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      value = ifelse(is.nan(value), NA, value),
      value = ifelse(n < 365 * !!coverage, NA, value)
    )

  if (!minimal) {

    # Check if model has suffered from overfitting
    pred_obs <-
      rmweather::rmw_predict_the_test_set(
        model = list_normalised$model,
        df = list_normalised$observations
      )

    # Investigate partial dependencies, if variable is NA, predict all
    partialdep <-
      rmweather::rmw_partial_dependencies(
        model = list_normalised$model,
        df = list_normalised$observations,
        variable = NA
      )

    results <- list(data = data, data_y1 = data_y1, model_stats = model_stats, imp = imp, partialdep = partialdep)

  } else {

    results <- list(data = data, data_y1 = data_y1, model_stats = model_stats, imp = imp)

  }

  return(results)
}



#' Wrapper to derive site-specific statistical meteo-normalisation trends per parameter from pre-compiled monitoring data including aggregation to yearly trend values
#'
#' @param data_trends
#' @param parameter
#' @param reference_year_fun
#' @param yearmin_per_site
#' @param frac_train
#' @param ntrees
#' @param nsamples
#' @param verbose
#' @param minimal
#' @param coverage
#'
#' @export
derive_trends_per_parameter <- function(data_trends, parameter, reference_year_fun, yearmin_per_site = 4, frac_train = 0.8, ntrees = 300, nsamples = 300, verbose = TRUE, minimal = TRUE, coverage = 0.8) {

  # how many data, reference year included?
  data_trends_agg <-
    data_trends |>
    dplyr::filter(parameter %in% !!c(parameter, trend_vars) & !is.na(value)) |>
    dplyr::group_by(year = lubridate::year(starttime), site, parameter) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n >= 365 * !!coverage)

  sites_trends_refyears <-
    data_trends_agg |>
    dplyr::mutate(reference_year = reference_year_fun(parameter)) |>
    dplyr::filter(year == reference_year & parameter == !!parameter) |>
    dplyr::distinct(site)  |>
    dplyr::pull(site) |>
    as.character()

  data_trends_agg <-
    data_trends_agg |>
    dplyr::group_by(site, parameter) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::filter(n >= !!yearmin_per_site) |>
    dplyr::filter(site %in% !!sites_trends_refyears) |>
    tidyr::spread(parameter, n)

  sites_trends <-
    data_trends_agg |>
    dplyr::select(site, !!parameter, !!trend_vars) |>
    na.omit() |>
    dplyr::pull(site) |>
    as.character()

  print(sites_trends)

  data_analysis <-
    data_trends |>
    dplyr::filter(site %in% !!sites_trends) |>
    dplyr::select(starttime, site, parameter, value)

  data_analysis <-
    data_analysis |>
    dplyr::filter(parameter %in% !!trend_vars) |>
    tidyr::spread(parameter, value) |>
    dplyr::right_join(dplyr::filter(data_analysis, !(parameter %in% !!trend_vars) & parameter == !!parameter), by = c("starttime", "site")) |>
    dplyr::rename(date = starttime) |>
    dplyr::group_split(site)

  results <- purrr::map(data_analysis, function(x) rf_meteo_normalisation(x, trend_vars, frac_train = frac_train,
                                                                          ntrees = ntrees, nsamples = nsamples, verbose = verbose,
                                                                          minimal = minimal, coverage = coverage))
  results_y1 <-
    results |>
    purrr::map(function(x) x$data_y1) |>
    dplyr::bind_rows()

  return(results_y1)
}


