#' Merge RSD data with corresponding metadata
#'
#' @param data
#' @param meta
#'
#' @keywords internal
merge_restructure_rsd <- function(data, meta) {

  meta <-
    meta |>
    dplyr::filter(is.na(as.numeric(vehicle_euronorm))) |>
    dplyr::select(-source, -remark) |>
    tidyr::spread(parameter, value)

  data <-
    data |>
    dplyr::mutate(vehicle_euronorm = dplyr::recode(vehicle_euronorm, !!!c("Euro5a" = "Euro5", "Euro5b" = "Euro5"))) |> # merge both sub-Euro5 norms since they are quite similar
    dplyr::left_join(meta, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) |>
    dplyr::mutate(
      vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
      vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel"))
    )

  return(data)
}


#' Filter RSD dataset using filter criteria
#'
#' @param data
#' @param filters
#'
#' @keywords internal
filter_rsd <- function(data, filters) {

  data <-
    data |>
    dplyr::filter(
      vehicle_model_year %in% filters$min[filters$parameter == "vehicleyears"]:filters$max[filters$parameter == "vehicleyears"] &
        (acceleration >= filters$min[filters$parameter == "accelerationrange"] & acceleration <= filters$max[filters$parameter == "accelerationrange"]) &
        (velocity >= filters$min[filters$parameter == "velocityrange"] & velocity <= filters$max[filters$parameter == "velocityrange"]) &
        (vehicle_specific_power >= filters$min[filters$parameter == "vsprange"] & vehicle_specific_power <= filters$max[filters$parameter == "vsprange"]) &
        vehicle_unloaded_weight <= filters$max[filters$parameter == "weightmax"] &
        !is.na(value)
    ) |>
    tidyr::spread(parameter, value) |>
    dplyr::filter(!is.na(NO + CO2 + CO + HC)) # all concentrations are nessecary for NOx emission calculation

  return(data)
}


#' Add calculated vehicle specific power to RSD dataset
#'
#' @param data
#'
#' @keywords internal
prep_vehicle_specific_power <- function(data){

  data_vsp <-
    data |>
    dplyr::filter(parameter %in% c("acceleration", "velocity") & !is.na(value)) |>
    dplyr::select(id, site_roadgrade, parameter, value) |>
    tidyr::spread(parameter, value) |>
    dplyr::mutate(vehicle_specific_power = calc_vsp(velocity * 1000 / 60^2, acceleration * 1000 / 60^2, site_roadgrade)) |>  # also convert velocity from km/h into m/s and acceleration from km/h/s into m/s2
    dplyr::select(id, acceleration, velocity, vehicle_specific_power) # vehicle_specific_power in kW/t

  return(data_vsp)
}


#' Calculate vehicle specific power following Jiménez
#'
#' @param speed
#' @param accel
#' @param slope
#' @param vsp.a
#' @param vsp.b
#' @param vsp.c
#' @param vsp.g
#'
#' @keywords internal
calc_vsp <- function(speed, accel, slope, # speed in m/s, accel in m/s/s, slope as ratio, mass = 3.5 in t
                     vsp.a = 1.1, vsp.b = 0.132, vsp.c = 0.000302, vsp.g = 9.81) {

  vsp <- speed * (vsp.a * accel + (vsp.g * slope) + vsp.b) + (vsp.c * speed^3)

  return(vsp)
}


#' Calculate RSD NOx emissions in g/kg fuel
#'
#' @param NO
#' @param p
#' @param CO2
#' @param CO
#' @param HC
#'
#' @keywords internal
calc_rsd_nox_emission <- function(NO, p, CO2, CO, HC) { # all concentrations in mixing ratios as percent

  Q <- CO / CO2
  Q1 <- HC / CO2
  Q2 <- NO / CO2
  NO_emission <- 30 * Q2 * 860 / ((1 + Q + 6 * Q1) * 12)
  NOx_emission <- NO_emission * 46 / (30 * (1 - p))

  return(NOx_emission)
}


#' Average stars raster data to another stars grid as mean values
#'
#' @param data
#' @param grid
#' @param method
#' @param na_val
#'
#' @keywords internal
average_to_grid <- function(data, grid, method = "average", na_val = -999) {

  parameter <- names(data)
  data <- stars::st_warp(data, grid, method = method, use_gdal = TRUE, no_data_value = na_val)
  names(data) <- parameter

  return(data)
}


#' Average stars raster data to the grid of BFS statpop dataset
#'
#' @param x
#' @param y
#'
#' @keywords internal
average_to_statpop <- function(x, y) {

    grid <- dplyr::select(x, RELI)
    data_avg <- purrr::map(y, function(data) average_to_grid(data, grid))

  return(data_avg)
}


#' Convert rasterdata into long format tibble
#'
#' @param data
#'
#' @keywords internal
simplify_aq_rasterdata <- function(data) {

  data <- purrr::map(names(data), function(pollutant) tibble::as_tibble(data[[pollutant]]))
  data <-
    data |>
    dplyr::bind_rows() |>
    tidyr::gather(pollutant, concentration, -x, -y) |>
    dplyr::filter(!is.na(concentration))

  return(data)
}


#' Merge air quality and statpop rasterdata with municipilty boundaries and convert to a common tibble
#'
#' @param data_raster
#' @param data_subareas
#' @param join_by
#' @param id_subareas
#'
#' @keywords internal
merge_statpop_with_subareas <- function(data_raster, data_subareas, join_by = "bfs", id_subareas = "gemeindename") {

  subareas_raster <-
    data_subareas |>
    dplyr::select(!!join_by) |>
    stars::st_rasterize(data_raster)
  #TODO: terra::rasterize(..., cover = TRUE, touches = TRUE)

  data <-
    dplyr::left_join(
      tibble::as_tibble(subareas_raster),
      tibble::as_tibble(data_raster),
      by = c("x","y")) |>
    dplyr::filter(!is.na(RELI) & RELI != 0 & !is.na(population))

  data <-
    data_subareas |>
    sf::st_drop_geometry() |>
    dplyr::select(!!join_by, !!id_subareas) |>
    dplyr::right_join(data, by = join_by)

  if (join_by == "bfs") {data <- dplyr::rename(data, bfsnr = bfs)}

  return(data)
}


#' calculate specific health outcome
#'
#' @param conc_increment
#' @param crf_per_concunit
#' @param deathrate_per_person
#' @param population
#'
#' @keywords internal
calc_outcome <- function(conc_increment, crf, crf_conc_increment, cases) {

  # see:
  # Castro, A., Kutlar Joss, M., Röösli, M. (2023). Quantifizierung des Gesundheitsnutzens der neuen
  # Luftqualitätsleitlinien der Weltgesundheitsorganisation in der Schweiz. Im Auftrag vom Bundesamt für Umwelt.

  CB <- conc_increment
  C0 <- 0 # set to 0 here since concentration increment is already directly provided by function input
  CA <- crf_conc_increment
  EEA <- crf
  GD <- cases

  EEB <- exp(log(EEA) * (CB - C0) / CA)

  A <- GD * (1 - 1 / EEB)

  return(A)
}


#' Calculate range of health outcomes from input-dataset (most likely value, lower and upper confidence intervals crf)
#'
#' @param data
#' @param conc_threshold
#'
#' @keywords internal
calculate_all_outcomes <- function(data, conc_threshold = "lower_conc_threshold") {

  data <-
    data |>
    dplyr::mutate(
      conc_incr = pmax(0, population_weighted_mean - !!rlang::sym(conc_threshold)),
      outcome = calc_outcome(conc_incr, crf, crf_conc_increment, number_of_deaths),
      outcome_lower = calc_outcome(conc_incr, crf_lower, crf_conc_increment, number_of_deaths),
      outcome_upper = calc_outcome(conc_incr, crf_upper, crf_conc_increment, number_of_deaths),
    ) |>
    dplyr::select(-conc_incr)

  return(data)
}

#' Get year of health-outcome base scenario: either provided year or a provided function
#'
#' @param base
#' @param ...
#'
#' @keywords internal
get_base_scenario_year <- function(base = "min", ...) {

  if (is.character(base)) {
    fun <- function(x) get(base)(x, ...)
  } else {
    fun <- function(x) base
  }

  return(fun)
}


