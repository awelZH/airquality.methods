#' Reads Swiss BFS inhabitant population raster data from official api
#'
#' @param year
#' @param destination_path
#' @param boundary
#' @param crs
#'
#' @export
read_statpop_raster_data <- function(year, destination_path, boundary, crs = 2056){
 
  # download zip
  download_statpop_data(year, destination_path, file_filter = paste0("STATPOP", year, "\\.csv"))
  
  # read and georeference file
  file_to_read <- list.files(
    destination_path, 
    pattern = paste0("STATPOP", year, "\\.csv"), 
    full.names = TRUE,
    recursive = TRUE
  )
  data_stars <- read_statpop_csv(file_to_read, year, crs = crs)
  
  # delete csv file
  unlink(file_to_read)
  
  # crop to boundary
  data_stars <- sf::st_crop(data_stars, boundary)
  
  return(data_stars)
}


#' Reads Swiss BFS life expectancy data ("Kohortensterbetafeln) from official api
#'
#' @param destination_path
#'
#' @export
read_bfs_life_expectancy_data <- function(destination_path = "inst/extdata"){
  
  # get downoad url from BFS api
  url <- get_bfs_metadata(bfs_nr = "px-x-0102020300_101")
  
  # download temp file from api
  temp <- download_file(download_url = url, destination_path = destination_path, file_ext = ".px")
  
  # read *.px
  data <- pxR::read.px(temp, encoding = "UTF-8")
  
  # delete temp *.px
  unlink(temp)
  
  return(data)
}


#' Reads raster data from official swisstopo api, used for BAFU data on air pollutants and sensitive ecosystem nitrogen deposition CLE exceedance
#'
#' @param id 
#' @param years_filter
#' @param boundary 
#' @param crs 
#'
#' @export
read_bafu_raster_data <- function(id, years_filter, boundary, crs = 2056){

  download_url <- get_geo_admin_metadata(id)
  years <- extract_year(download_url)
  download_url <- setNames(download_url, years)
  pollutant <- extract_pollutant(id)
  years <- years[which(years %in% years_filter)]

  # FIXME: read_stars returns a curvilinear LV95 grid in this case which creates problems later on (?)
  data <-
    setNames(as.character(years), as.character(years)) |> 
    purrr::map(function(yr) {
      
      data <- stars::read_stars(download_url[[yr]], proxy = FALSE) # |> 
        # sf::st_transform(crs = sf::st_crs(crs)) # needs a lot of time and RAM => not really necessary here
      
      # crop to boundary
      # FIXME regular grid workaround including pre-filtering to reduce large input dataset and speed / RAM problems: 
      bbox <- sf::st_bbox(boundary)
      margin <- 500 # m
      
      data <- 
        data |> 
        sf::st_coordinates() |> # for some reason, that's more memory friendly than as_tibble() straight away
        dplyr::mutate(
          value = as.numeric(sf::st_drop_geometry(data[[1]]))
        ) |>
        tibble::as_tibble() |> 
        dplyr::filter(x >= bbox$xmin - !!margin & x <= !!bbox$xmax + !!margin & y >= !!bbox$ymin - !!margin & y <= !!bbox$xmax + !!margin) |> 
        stars::st_as_stars() |> 
        sf::st_set_crs(value = crs) |> 
        sf::st_crop(boundary) #TODO: as_points = TRUE (default) => cells are interpreted as points, with FALSE, they are interpreted as cells (i.e., everything that touches the polygon is included ...)
    
      names(data) <- extract_pollutant(download_url[[yr]])
      
      return(setNames(list(data), pollutant))
      
    })
  
  return(data)
}


#' Reads datasets (*.csv, *.px) from url provided by opendata.swiss
#'
#' @param url 
#' @param source 
#' @param file_filter
#'
#' @export
read_opendataswiss <- function(url, source, file_filter = ".csv"){

  read_url <- get_opendataswiss_metadata(url, file_filter)
  if (stringr::str_detect(file_filter, ".csv")) {
    data <- purrr::map_df(read_url, function(x) readr::read_delim(x, delim = ","))
  }
  if (stringr::str_detect(file_filter, "api/v1/de")) {
    # TODO ...
  }
  data <- dplyr:: mutate(data, source = source)
  
  return(data)
}


#' Reads local *.csv file
#'
#' @param file 
#' @param delim 
#' @param locale 
#' @param ... 
#'
#' @export
read_local_csv <- function(file, delim = ";", locale = readr::locale(encoding = "latin1", tz = "Etc/GMT-1"), ...){
  
  data <- readr::read_delim(file, delim = delim, locale = locale, ...)
  
  return(data)
}


#' Reads spatial polygon data from Canton Zurich geolion wfs api
#'
#' @param apiurl 
#' @param version 
#' @param crs 
#'
#' @export
read_geolion_wfs <- function(apiurl, version = "2.0.0", crs = 2056){
  
  request <- get_geolion_wfs_metadata(apiurl, version = version, crs = crs)
  data <- 
    request |> 
    sf::read_sf(type = 6) |> 
    sf::st_transform(crs = sf::st_crs(crs))
  
  return(data)
}


#' Reads spatial air quality raster data from Canton Zurich geolion wcs api
#' 
#' @param cov_stack
#' @param layer_names
#' @param boundary
#' @param na_value
#' 
#' @export
#' # read_geolion_wcs_stack <- function(cov_stack, layer_names, boundary, na_value = c(0, -999)){#   #   cov_stack_filtered <- cov_stack[sapply(cov_stack, function(x) x$CoverageId %in% layer_names)]
#   data_list <- lapply(cov_stack_filtered, function(x) read_single_pollutant_wcs(x, na_value))
#   
#   list_names <- gsub("pm-", "pm", layer_names)
#   list_names <- gsub("jahre-", "", list_names)
#   names(data_list) <- list_names
#   
#   return(data_list)
# }



#' Reads and combines all spatial air quality raster data
#'
#' @param ressources
#' @param years 
#' @param boundary 
#' 
#' @export
read_all_raster_data <- function(ressources, years, boundary) {
  
  print("get PM2.5")
  data_raster_pm25 <- read_bafu_raster_data(filter_ressources(ressources, 15), years_filter = years$PM2.5, boundary) 
  print("get PM10")
  data_raster_pm10 <- read_bafu_raster_data(filter_ressources(ressources, 14), years_filter = years$PM10, boundary)
  print("get NO2")
  data_raster_no2 <- read_bafu_raster_data(filter_ressources(ressources, 13), years_filter = years$NO2, boundary) # NO2 may take a while since data from 2020 on are in highres
  print("get O3mp98")
  data_raster_o3mp98 <- read_bafu_raster_data(filter_ressources(ressources, 16), years_filter = years$O3, boundary)
  print("combine")
  years$all <- sort(unique(as.numeric(names(c(data_raster_pm25, data_raster_pm10, data_raster_no2, data_raster_o3mp98)))))
  data_raster_aq <- combine_raster_aq(years, data_raster_pm25, data_raster_pm10, data_raster_no2, data_raster_o3mp98)
  
  return(data_raster_aq)
}





