#' Prepare data from ressources.csv for use in scripts 
#'
#' @param ressources 
#'
#' @export
prepare_ressources <- function(ressources) {
  
  ressources <- 
    ressources |> 
    dplyr::mutate(
      get = dplyr::case_when(
        stringr::str_detect(DOWNLOAD_URL, "inst/extdata") ~ paste(DOWNLOAD_URL, DATASET_NAME, sep = "/"),
        DOWNLOAD_SOURCE == "swisstopo" ~ DATASET_NAME,
        TRUE ~ DOWNLOAD_URL
      )
    )
  
  return(ressources)
}


#' Prepares emission dataset for appropriate aggregation
#'
#' @param data 
#' @param filter_args 
#'
#' @export
prepare_emmissions <- function(data, 
                               filter_args = canton == 'ZH' & year <= lubridate::year(Sys.Date()) & emission != 0 & !(subsector %in% c('Weitere Punktquellen OL', 'Rheinschifffahrt', 'Flugverkehr Genf'))
){

  filter_args <- rlang::enquo(filter_args)
  data_prep <- 
    data |> 
    dplyr::rename(
      year = jahr,
      pollutant = substanz,
      sector = hauptgruppe,
      subsector = untergruppe,
      canton = kanton,
      municipality = gemeinde, 
      unit = einheit
    ) |> 
    dplyr::mutate(
      pollutant = ifelse(pollutant == "BC", "eBC", pollutant)
    ) |> 
    dplyr::filter(!!filter_args)
  
  return(data_prep)
}


#' Merge RSD metadate with dataset, apply filters, calculate vehicle specific power, calculate NOx-emissions and prepare dataset for aggregation
#'
#' @param data 
#' @param rsd_auxiliary 
#'
#' @export
prepare_rsd <- function(data, rsd_auxiliary){
  
  rsd_meta <- rsd_auxiliary$meta
  rsd_filters <- rsd_auxiliary$filters
  rsd_filters$max[which(rsd_filters$parameter == "vehicleyears")] <- lubridate::year(Sys.Date()) # include most recent vehicle model years
  
  # calculate vehicle specific power from measurement data subset and merge with RSD dataset
  data_vsp <- prep_vehicle_specific_power(data)
  data_rsd <-
    data |>
    dplyr::select(-unit) |> 
    dplyr::filter(!(parameter %in% c("acceleration", "velocity"))) |>
    dplyr::left_join(data_vsp, by = "id") 
  
  # apply data filters for a meaningful analysis
  data_rsd <- filter_rsd(data_rsd, rsd_filters)
  
  # restructure and merge with Euronorm metadata 
  data_rsd <- merge_restructure_rsd(data_rsd, rsd_meta)
  
  # calculate NOx emissions
  data_rsd <- dplyr::mutate(data_rsd, nox_emission = calc_rsd_nox_emission(NO = NO / 10^4, p = fraction_no2_hbefa, CO2 = CO2, CO = CO, HC = HC / 10^4)) # input: concentrations all in percent; originally: NO in ppm, CO2 in %, CO in %, HC in ppm; output: NOx emissions in g/kg fuel;  add HBEFA-derived NO2 and use that for NOx emission calculation rather than measured NO2 since that has only been available since RSD-model 4500
  
  return(data_rsd)
}


#' Spatially average base-year (min(year)) scenario pollutant raster data to the grid of statpop data of the respective year
#'
#' @param data_raster_bfs 
#' @param data_raster_aq 
#' @param years 
#'
#' @export
prepare_rasterdata_aq_base <- function(data_raster_bfs, data_raster_aq, base_year) {
  
  years <- as.numeric(names(data_raster_aq))
  other_years <- years[years != base_year]
  data_raster_aq_base <- data_raster_aq[[as.character(base_year)]]
  data_raster_aq_base <- setNames(rep(list(data_raster_aq_base), length(other_years)), other_years)
  data_raster_aq_base <- purrr::map2(data_raster_bfs[as.character(other_years)], data_raster_aq_base, average_to_statpop)
  
  return(data_raster_aq_base)
}


#' Convert BFS statpop spatial raster rasterdata as well as air quality spatial rasterdata to combined tibble dataset for use in script
#'
#' @param data_raster_bfs 
#' @param data_raster_aq 
#' @param years 
#'
#' @export
prepare_exposition <- function(data_raster_bfs, data_raster_aq, years) {
  
  # => convert pollutant and statpop data into a common tibble
  data_statpop <-
    years |> 
    as.character() |> 
    purrr::map(function(year) dplyr::mutate(tibble::as_tibble(data_raster_bfs[[year]]), year = as.numeric(year))) |> 
    dplyr::bind_rows() |> 
    dplyr::filter(!is.na(population) & population > 0)
  
  data_aq <-
    years |> 
    as.character() |> 
    purrr::map(function(yr) { 
      simplify_aq_rasterdata(data_raster_aq[[yr]]) |> 
        dplyr::mutate(year = as.numeric(yr))
    }) |> 
    dplyr::bind_rows()
  
  data <- dplyr::full_join(data_statpop, data_aq, by = c("x","y","year"))
  data <-
    data |> 
    dplyr::filter(!is.na(population) & !is.na(concentration)) |> 
    dplyr::mutate(
      parameter = dplyr::recode(pollutant,
                                "no2" = "NO2",
                                "pm25" = "PM2.5",
                                "pm10" = "PM10",
                                "bc" = "eBC",
                                "mp98" = "O3_max_98p_m1"
      ),
      pollutant = shortpollutant(parameter),
      metric = longmetric(parameter),
      unit = "µg/m3",
      source = "BAFU & BFS"
    ) |> 
    dplyr::select(x, y, RELI, year, population, pollutant, metric, parameter, concentration, unit, source)
  
  return(data)
}


#' Merge municipaity boundaries with BFS statpop spatial rasterdata, convert respective data as well as air quality spatial rasterdata to combined tibble dataset for use in script to calculate population weighted means
#'
#' @param data_raster_bfs 
#' @param data_raster_aq 
#' @param years 
#' @param boundaries 
#'
#' @export
prepare_weighted_mean <- function(data_raster_bfs, data_raster_aq, years, boundaries) {
  
  data_statpop_municip <- 
    years |> 
    as.character() |> 
    purrr::map(function(yr) {
      merge_statpop_with_municipalities(data_raster_bfs[[yr]], boundaries) |> 
        dplyr::mutate(year = as.numeric(yr))
    }) |> 
    dplyr::bind_rows()
  
  data_aq <-
    years |> 
    as.character() |> 
    purrr::map(function(yr) { 
      simplify_aq_rasterdata(data_raster_aq[[yr]]) |> 
        dplyr::mutate(year = as.numeric(yr))
    }) |> 
    dplyr::bind_rows()
  
  data <- dplyr::right_join(data_statpop_municip, data_aq, by = c("x", "y", "year"))
  data <-
    data |> 
    dplyr::filter(!is.na(population)) |> 
    dplyr::mutate(
      parameter = dplyr::recode(pollutant,
                                "no2" = "NO2",
                                "pm25" = "PM2.5",
                                "pm10" = "PM10",
                                "bc" = "eBC",
                                "mp98" = "O3_max_98p_m1"
      ),
      pollutant = shortpollutant(parameter),
      metric = longmetric(parameter),
      unit = "µg/m3",
      source = "BAFU & BFS"
    ) |> 
    dplyr::select(x, y, RELI, bfsnr, gemeindename, year, population, pollutant, metric, parameter, concentration, unit, source)
  
  return(data)
}


#' Prepare mortality data
#' 
#' @param data_mortality 
#'
#' @export
prepare_mortality <- function(data_mortality) {
  
  data_mortality <- 
    data_mortality |> 
    dplyr::mutate(alterkat = ifelse(alterkat == 290, 291, alterkat)) |> #FIXME once original dataset is adjusted
    dplyr::filter(tukat == "krankheitsbedingt" & alterkat != 290) # 290 = younger than 30 years 
  
  data_freq_na <- 
    data_mortality |> 
    dplyr::filter(alterkat == 291) |>
    dplyr::group_by(jahr, geschlecht) |> 
    dplyr::summarise(freq_na = sum(anzahl)) |> 
    dplyr::ungroup()
  
  data_freq_na <- 
    data_mortality |> 
    dplyr::filter(alterkat == 291) |>
    dplyr::group_by(jahr, geschlecht) |> 
    dplyr::summarise(freq_na = sum(anzahl)) |> 
    dplyr::ungroup()
  
  data_freq_na <- 
    data_mortality |> 
    dplyr::filter(alterkat != 291) |> 
    dplyr::group_by(jahr, geschlecht) |> 
    dplyr::summarise(na = sum(is.na(anzahl))) |> 
    dplyr::ungroup() |> 
    dplyr::full_join(data_freq_na, by = c("jahr", "geschlecht"))
  
  data_mortality <- 
    data_mortality |> 
    dplyr::filter(alterkat != 291) |> 
    left_join(data_freq_na, by = c("jahr", "geschlecht")) |>
    dplyr::mutate(
      anzahl = ifelse(is.na(anzahl), freq_na / na, anzahl), # when NA, then actually < 4 due to privacy protection. So, for better average accuracy distribute alterkat == 291 over all NA cases
      year_of_birth = jahr - alterkat,
      geschlecht = factor(geschlecht),
      source = "Statistisches Amt Kanton Zürich & BFS"
    ) |> 
    dplyr::rename(
      age = alterkat,
      frequency = anzahl,
      sex = geschlecht,
      year_of_death = jahr
    ) |> 
    dplyr::select(-tukat, -na, -freq_na)
  
  return(data_mortality)
}


#' Prepare preliminary deaths from population-weighted mean, mortality cases and outcome metadata and derive health outcomes
#' 
#' @param data_expo_weighmean 
#' @param data_mortality 
#' @param outcomes_meta 
#' @param conc_threshold 
#'
#' @export
prepare_preliminary_deaths <- function(data_expo_weighmean, data_mortality, outcomes_meta, conc_threshold = "lower_conc_threshold") {
  
  # aggregate mortality
  data_mortality <-
    data_mortality |> 
    dplyr::rename(year = year_of_death) |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(number_of_deaths = sum(frequency)) |> 
    dplyr::ungroup()
  
  # combine and wrangle all input data
  data <- 
    data_expo_weighmean |> 
    dplyr::filter(parameter %in% unique(outcomes_meta$parameter)) |>
    dplyr::select(-pollutant, -metric, -source, -unit, -concentration_max, -concentration_mean, -concentration_median) |> 
    tidyr::gather(scenario, population_weighted_mean, -year, -parameter, -base_year, -population, -concentration_min) |> 
    dplyr::left_join(outcomes_meta, by = "parameter") |> 
    dplyr::left_join(data_mortality, by = "year") |> 
    dplyr::mutate(
      scenario = dplyr::recode(scenario, population_weighted_mean = "tatsächliche Belastung", population_weighted_mean_base = paste0("vermieden vs. ",na.omit(unique(.data$base_year)))),
      concentration_min = ifelse(stringr::str_detect(scenario, "vermieden"), NA, concentration_min)
    ) |> 
    dplyr::select(-base_year) |> 
    dplyr::filter(!is.na(scenario))
  
  # calculate outcomes
  data <-
    data |> 
    dplyr::filter(scenario == "tatsächliche Belastung") |> 
    dplyr::mutate(min_conc_threshold = pmin(concentration_min, lower_conc_threshold)) |> 
    calculate_all_outcomes(conc_threshold = "min_conc_threshold") |> 
    dplyr::select(year, pollutant, metric, scenario, outcome_type, outcome) |>
    dplyr::rename(outcome_min_conc = outcome) |> 
    dplyr::right_join(data, by = c("year", "pollutant", "metric", "scenario", "outcome_type")) |> 
    calculate_all_outcomes() |> 
    dplyr::mutate(outcome_delta_min_conc = outcome_min_conc - outcome) |> 
    dplyr::select(year, pollutant, metric, parameter, population, scenario, outcome_type, outcome, outcome_lower, outcome_upper, outcome_delta_min_conc)
  
  # restructure dataset
  scen <- unique(data$scenario)[stringr::str_detect(unique(data$scenario), "vermieden")]
  data <-
    data |> 
    dplyr::select(year, pollutant, metric, parameter, scenario, outcome_type, outcome, population) |> 
    tidyr::spread(scenario, outcome) |> 
    dplyr::mutate(!!scen := pmin(0, `tatsächliche Belastung` - !!rlang::sym(scen))) |> 
    dplyr::select(-`tatsächliche Belastung`) |> 
    tidyr::gather(scenario, outcome, -year, -pollutant, -metric, -parameter, -outcome_type, -population) |> 
    dplyr::bind_rows(dplyr::filter(data, scenario == "tatsächliche Belastung")) |> 
    dplyr::arrange(pollutant, metric, scenario, year, outcome_type) |> 
    dplyr::filter(!is.na(outcome)) |> 
    dplyr::select(year, pollutant, metric, parameter, outcome_type,  population, scenario, outcome, outcome_lower, outcome_upper, outcome_delta_min_conc)
  
  return(data)
}


#' Converts list of stars raster data to tibble data (for BAFU raster data)
#'
#' @param rasterlist
#'
#' @keywords internal
bafu_rasterlist_to_tibble <- function(rasterlist) {
  
  years <- names(rasterlist)
  
  data <- 
    purrr::map(years, function(yr) 
      
      rasterlist[[yr]]$ndep_exmax |>
        tibble::as_tibble() |>
        na.omit() |>
        dplyr::mutate(
          year = as.numeric(yr),
          source = "BAFU"
        ) 
    ) |> 
    dplyr::bind_rows()
  
  return(data)
}


#' Wrangle life-expectancy data from *.px format into tibble()
#'
#' @param data
#'
#' @keywords internal
prepare_life_expectancy_data <- function(data) {
  
  # wrangle data
  data <- 
    data |> 
    tibble::as_tibble() |>
    dplyr::mutate(
      Alter = readr::parse_number(as.character(Alter)),
      Geburtsjahrgang = as.numeric(as.character(Geburtsjahrgang)),
      Geschlecht = dplyr::recode(Geschlecht, Frau = "weiblich", Mann = "männlich"),
      source = "BFS"
    ) |>
    dplyr::filter(stringr::str_detect(Beobachtungseinheit, "Lebensdauer")) |>
    tidyr::spread(Beobachtungseinheit, value) |>
    dplyr::rename(
      age = Alter,
      sex = Geschlecht,
      year_of_birth = Geburtsjahrgang,
      remaining_lifeyears = `Verbleibende Lebensdauer (ex)`
    ) |>
    dplyr::select(sex, year_of_birth, age, remaining_lifeyears, source)
  
  return(data)
}
