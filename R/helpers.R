
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
    x == "NMVOC" ~ "nicht-Methan Kohlenwasserstoffe",
    x == "NH3" ~ "Ammoniak",
    x == "NHx" ~ "reduzierter Stickstoff",
    x == "Nr" ~ "reaktiver Stickstoff",
    x == "CO" ~ "Kohlenstoffmonoxid",
    x == "SO2" ~ "Schwefeldioxîd",
    x == "NOx" ~ "Stickoxide",
    x == "eBC" ~ "Russ",
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
    TRUE ~ x
  )

  return(long)
}


#' Recode short pollutant/parameter string to long metric string
#'
#' @param x
#'
#' @keywords internal
longmetric <- function(x) {

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


