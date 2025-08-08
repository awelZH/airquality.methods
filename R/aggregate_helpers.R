
#' General function do aggregate some statistics from long data 
#'
#' @param data 
#' @param y 
#' @param groups 
#' @param nmin 
#' @param perc 
#'
#' @keywords internal
aggregate_groups <- function(data, y, groups, nmin = 3, perc = list(ymin = 0.05, lower = 0.25, middle = 0.5, upper = 0.75, ymax = 0.95)) {
  
  data <-
    data |>
    dplyr::group_by_at(dplyr::vars(groups)) |>
    dplyr::summarise(
      n = length(na.omit(!!rlang::sym(y))),
      minimum = min(!!rlang::sym(y), na.rm = TRUE),
      minlower = quantile(!!rlang::sym(y), perc$ymin, na.rm = TRUE),
      lower = quantile(!!rlang::sym(y), perc$lower, na.rm = TRUE),
      middle = quantile(!!rlang::sym(y), perc$middle, na.rm = TRUE),
      upper = quantile(!!rlang::sym(y), perc$upper, na.rm = TRUE),
      maxupper = quantile(!!rlang::sym(y), perc$ymax, na.rm = TRUE),
      maximum = max(!!rlang::sym(y), na.rm = TRUE),
      mean = mean(!!rlang::sym(y), na.rm = TRUE),
      standarddeviation = sd(!!rlang::sym(y), na.rm = TRUE),
      medianabsolutedeviation = mad(!!rlang::sym(y)),
      standarderror = standarddeviation / sqrt(n),
      sum = sum(!!rlang::sym(y), na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  
  data_all <-
    data |> 
    dplyr::select(tidyr::all_of(groups)) |> 
    dplyr::distinct_all() |> 
    tidyr::expand(tidyr::crossing(!!!rlang::syms(groups)))
  
  data <- dplyr::left_join(data_all, data, by = groups) 
  data <- dplyr::mutate_at(data, c("minimum", "minlower", "lower", "middle", "upper", "maxupper", "maximum", "mean", "standarddeviation", "standarderror", "medianabsolutedeviation", "sum"), list(~ifelse(n < nmin, NA, .)))
  data <- dplyr::mutate_at(data, c("minimum", "minlower", "lower", "middle", "upper", "maxupper", "maximum", "mean", "standarddeviation", "standarderror", "medianabsolutedeviation", "sum"), list(~ifelse(is.nan(.), NA, .)))
  data <- dplyr::mutate_at(data, c("minimum", "minlower", "lower", "middle", "upper", "maxupper", "maximum", "mean", "standarddeviation", "standarderror", "medianabsolutedeviation", "sum"), list(~ifelse(is.infinite(.), NA, .)))
  data$n <- ifelse(is.na(data$n), 0, data$n)
  
  return(data)
}


#' Merge groups with small emissions in emission budget datasets
#'
#' @param data 
#' @param threshold_fraction 
#' @param first 
#'
#' @keywords internal
groups_emission_subsector <- function(data, threshold_fraction = 0.05, first = 1:2) {
  
  # mean emissions per category over all years
  group_vars <- c("pollutant", "sector", "subsector")
  groups <- 
    data |> 
    aggregate_groups(y = "emission", groups = group_vars, nmin = 1) |> 
    dplyr::rename(emission = mean) |> 
    dplyr::select(tidyr::all_of(c(group_vars,"emission"))) |> 
    dplyr::filter(!is.na(emission))
  
  # recode subsectors with mean emissions less than the "big x" into subsector "sonstige"
  groups <- 
    groups |> 
    dplyr::group_by(pollutant, sector) |> 
    dplyr::mutate(
      subsector_new = dplyr::case_when(
        emission < min(sort(emission, decreasing = TRUE)[first]) ~ "verschiedene",
        TRUE ~ subsector
      ),
      subsector_new = paste0(sector, " / ", subsector_new)
    ) |> 
    dplyr::arrange(pollutant, sector, dplyr::desc(emission))
  
  # aggregate emissions accordingly
  group_vars <- c("pollutant", "sector", "subsector_new")
  groups2 <- 
    groups |> 
    aggregate_groups(y = "emission", groups = group_vars, nmin = 1) |> 
    dplyr::rename(emission = sum) |> 
    dplyr::select(tidyr::all_of(c(group_vars,"emission"))) |> 
    dplyr::filter(!is.na(emission))
  
  # second iteration: if subsector emission < overall emission * threshold fraction, then also recode into "sonstige"
  groups2 <- 
    groups2 |> 
    dplyr::group_by(pollutant) |> 
    dplyr::mutate(
      emission_fraction = emission / sum(emission),
      subsector_new = dplyr::case_when(
        !stringr::str_detect(subsector_new, "/ verschiedene") & emission_fraction < threshold_fraction ~ paste0(sector, " / ", "verschiedene"),
        TRUE ~ subsector_new
      )
    ) |> 
    dplyr::ungroup()
  
  # aggregate accordingly
  group_vars <- c("pollutant", "subsector_new")
  groups2 <- 
    groups2 |> 
    aggregate_groups(y = "emission", groups = group_vars, nmin = 1) |> 
    dplyr::rename(emission = sum) |> 
    dplyr::select(tidyr::all_of(c(group_vars,"emission"))) |> 
    dplyr::filter(!is.na(emission))
  
  # restructure and prepare as lookup table for use outside this function
  groups2 <- dplyr::filter(groups2, !stringr::str_detect(subsector_new, "verschiedene"))
  groups <-
    groups |> 
    dplyr::select(-emission) |> 
    dplyr::left_join(groups2, by = c("pollutant", "subsector_new")) |> 
    dplyr::arrange(pollutant, sector, dplyr::desc(emission)) |> 
    dplyr::mutate(
      subsector_new = dplyr::case_when(is.na(emission) ~ paste0(sector, " / ", "verschiedene"), TRUE ~ subsector_new),
      subsector_new = factor(subsector_new, levels = unique(.data$subsector_new))
    ) |> 
    dplyr::select(-emission)
  
  return(groups)
}


#' Aggregate emissions from RSD dataset using some statistical functions
#'
#' @param data 
#' @param meta 
#' @param y 
#' @param groups 
#' @param nmin 
#'
#' @keywords internal
aggregate_rsd <- function(data, meta, y = "nox_emission", groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"), nmin = 50) {

  if ("year" %in% groups) {
    
    data <-
      data |>
      dplyr::mutate(vehicle_fuel_type = "all") |>
      dplyr::bind_rows(data) |>
      dplyr::mutate(year = lubridate::year(date_measured))
    
  }
  
  data <-
    data |>
    aggregate_groups(y = y, groups = groups, nmin = nmin) |>
    dplyr::rename(!!y := mean) |>
    dplyr::mutate(
      unit = "g/kg fuel",
      source = "Kanton Zürich/AWEL"
    )
  
  if (is.null(meta)) {
    
    data <- dplyr::select(data, !!c(groups, y, "unit", "n", "standarderror", "source"))
    
  } else {
    
    if ("vehicle_model_year" %in% groups) {
      
      meta <-
        meta |>
        dplyr::filter(!is.na(as.numeric(vehicle_euronorm))) |>
        dplyr::mutate(vehicle_euronorm = as.numeric(vehicle_euronorm)) |>
        dplyr::rename(vehicle_model_year = vehicle_euronorm)
      
    }
    
    data <-
      data |>
      dplyr::left_join(meta, by = groups) |>
      dplyr::select(!!c(groups, y, "unit", "n", "standarderror", "nox_emission_threshold_g_per_kg_fuel", "source"))
  }

  return(data)
}


#' Merge components of nitrogen deposition to broader source categories in nitrogen deposition dataset
#'
#' @param data 
#'
#' @keywords internal
simplify_nitrogen_parameters <- function(data) {
  
  data <-
    data |>
    dplyr::mutate(
      parameter = dplyr::case_when(
        stringr::str_detect(parameter, "NO3") | stringr::str_detect(parameter, "NO2") ~ "aus NOx-Quellen",
        stringr::str_detect(parameter, "NH3") | stringr::str_detect(parameter, "NH4") ~ "aus NH3-Quellen",
        TRUE ~ parameter
      ),
      parameter =  factor(parameter, levels = c("aus NOx-Quellen", "aus NH3-Quellen", "N-Deposition")),
      # ecosystem_category = paste0("empfindliches Ökosystem: ", ecosystem_category),
      site_short = stringr::str_remove(site_short, "_Wald"),
      site = stringr::str_remove(site, "_Wald"),
      site = stringr::str_replace(site, "_", "-")
    ) |> 
    dplyr::rename(
      site_long = site,
      site = site_short
    )
  
  return(data)
}


#' Calculate concentration bins for exposition assessment
#'
#' @param data 
#'
#' @keywords internal
bin_concentration <- function(data) {
  
  fun <- bin_fun(unique(data$parameter))
  data <- dplyr::mutate(data, concentration = fun(concentration))
  
  return(data)
}


#' Calculate population weighted mean concentration for exposition assessment
#'
#' @param concentration 
#' @param population 
#'
#' @keywords internal
calc_population_weighted_mean <- function(concentration, population) {sum(concentration * population, na.rm = TRUE) / sum(population, na.rm = TRUE)}

