#' Read *.csv from BFS statpop
#'
#' @param file 
#' @param year 
#' @param crs 
#'
#' @keywords internal
read_statpop_csv <- function(file, year, crs = 2056) {

  var <- ifelse(as.numeric(year) > 2022, "BBTOT", paste0("B", year %% 100, "BTOT")) #FIXME: derive from data itself 
  delim <- ifelse(as.numeric(year) > 2019, ";", ",") #FIXME: derive from data itself
  
  data <- 
    readr::read_delim(
      file,
      delim = delim, 
      col_select = c(RELI, E_KOORD, N_KOORD, !!var),
      locale = readr::locale(encoding = "UTF-8")
    ) |> 
      dplyr::rename(population = !!var)
  
  dim <- diff(sort(unique(dplyr::pull(data, E_KOORD))))[1]
  
  data_stars <- 
    data |> 
    dplyr::mutate( # BFS coordinate points represent lower left corner of rastercell and rasterize assumes centre
      E_KOORD = E_KOORD + dim / 2,
      N_KOORD = N_KOORD + dim / 2
    ) |>
    sf::st_as_sf(
      coords = c("E_KOORD", "N_KOORD"), 
      dim = "XY",
      crs = sf::st_crs(crs)
    ) |>
    stars::st_rasterize()
  
  return(data_stars)
}


#' Read *.shp from swisstopo / BAFU
#'
#' @param file 
#'
#' @keywords internal
read_bafu_shp <- function(file){
  
  file_to_read <- list.files(
    destination_path, 
    pattern = "\\.shp$", 
    full.names = TRUE,
    recursive = TRUE
  )
  data_sf <- sf::read_sf(file)
  
  return(data_sf)
}


#' read wcs layer from geolion
#'
#' @param coverage 
#' @param na_value 
#'
#' @keywords internal
# read_single_pollutant_wcs <- function(coverage, na_value){
# 
#   data <- 
#     coverage$getCoverage() |>  
#     stars::st_as_stars() |> 
#     sf::st_set_crs(value = 2056)
#   
#   divisor <- ifelse(stringr::str_detect(names(data), "jahre"), 1, 10)
#   divisor <- ifelse(stringr::str_detect(names(data), "eBC") & !stringr::str_detect(names(data), "jahre"), 100, divisor)
#   
#   data <- setNames(data, "value")
#   data <-
#     data |> 
#     dplyr::mutate(
#       value = ifelse(value %in% na_value, NA, value),
#       value = value / divisor
#     )
#   
#   name <- gsub("\\d{4}|jahre|-", "",coverage$CoverageId)
#   
#   data <- setNames(data, name)
#   
#   return(data)
# }


#' Convert wcs geolion coverage list to a data.frame
#'
#' @param cov_stack 
#'
#' @keywords internal
# to_stack_df <- function(cov_stack){
#   
#   df <- purrr::map_df(cov_stack, ~data.frame(
#     pollutant = gsub("-jahre", "", gsub("^(.*)-.*", "\\1", .x$CoverageId)),
#     year = gsub(".*-(\\d{4})$", "\\1", .x$CoverageId),
#     layer_name = .x$CoverageId
#   )) |> 
#     dplyr::mutate(pollutant = gsub("pm-", "pm", pollutant))
#   
#   return(df)
# }


#' Get dataset metadata from geo.admin api
#'
#' @param id 
#'
#' @keywords internal
get_geo_admin_metadata <- function(id, stac_version = "0.9"){
  
  metadata_url <- paste0("https://data.geo.admin.ch/api/stac/v",stac_version,"/collections/",id,"/items")
  metadata <- rjson::fromJSON(file = metadata_url)
  url <- unlist(purrr::map(metadata$features, function(x) x$assets[which(grepl("tiff", x$assets))][[1]]$href))
  
  return(url)
}


#' Get dataset download url from BFS api based on BFS dataset id
#'
#' @param bfs_nr
#'
#' @keywords internal
get_bfs_metadata <- function(bfs_nr){
  
  # derive dataset url
  base_url <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets"
  
  # Use withr::with_envvar to set no_proxy environment variable
  withr::with_envvar(
    new = c("no_proxy" = "dam-api.bfs.admin.ch"),
    code = {
      # Build the request URL with the order number as a query parameter
      response <- httr2::request(base_url) |> 
        httr2::req_url_query(orderNr = bfs_nr) |> 
        httr2::req_headers(
          "accept" = "application/json",      # Ensure we accept JSON
          "Content-Type" = "application/json" # Request content type is JSON
        ) |> 
        httr2::req_perform()
      
      # Parse the JSON response body into a list
      data <- httr2::resp_body_json(response)
    }
  )
  
  links <- data[["data"]][[1]][["links"]]
  download_url <- links[sapply(links, function(x) grepl("master", x$href))][[1]]$href
  
  return(download_url)
}


#' Wrapper to get statpop dataset download url based on year
#'
#' @param year 
#'
#' @keywords internal
get_bfs_statpop_metadata <- function(year){
  
  bfs_nr <- paste0("ag-b-00.03-vz", year, "statpop")
  download_url <- get_bfs_metadata(bfs_nr)
  
  return(download_url)
}


#' Get dataset metadata from opendata.swiss api
#'
#' @param apiurl 
#'
#' @keywords internal
get_opendataswiss_metadata <- function(apiurl, file_filter = ".csv", useragent = "Amt für Abfall, Wasser, Energie und Luft, Kanton Zürich"){

  req <- httr2::request(apiurl)
  req <- httr2::req_user_agent(req, useragent)
  req_data <- httr2::req_perform(req)
  metadata <- httr2::resp_body_json(req_data)$result        
  links <- unlist(purrr::map(metadata$resources, function(x) x$url))
  download_link <- links[stringr::str_detect(links, file_filter)]
  
  if (any(stringr::str_detect(download_link, "ostluft_emissionsbilanzen"))) { # since this dataset may contain different files from various submissions => use only the latest one
    download_link <- download_link[which.max(extract_year(download_link))]
  }
  
  return(download_link)
}


#' Get wfs dataset metadata from Canton Zurich geolion api
#'
#' @param apiurl 
#' @param type 
#' @param version 
#' @param crs 
#'
#' @keywords internal
get_geolion_wfs_metadata <- function(apiurl, type = "ms:gem_grenzen", version = "2.0.0", crs = 2056){
  
  url <- httr2::url_parse(apiurl)
  url$query <- list(service = "wfs",
                    version = version,
                    request = "GetFeature",
                    typename = type, # "ms:gem_seen_grenzen",
                    srsName = paste0("EPSG:", crs)
  )
  request <- httr2::url_build(url)
  
  return(request)
}


#' Get wcs dataset metadata from Canton Zurich geolion api
#'
#' @param wcs_stack 
#' @param version 
#'
#' @keywords internal
# get_geolion_wcs_metadata <- function(wcs_stack, version = "2.0.1"){
#   
#   client <- ows4R::WCSClient$new(wcs_stack, serviceVersion = version)
#   cap <- client$getCapabilities()
#   cov <- cap$getCoverageSummaries()
#   cov_ids <- sapply(cov, function(x) x$CoverageId)
#   cov_list <- lapply(cov_ids, function(x) cap$findCoverageSummaryById(x))
#   
#   return(cov_list)
# }


#' Download file into tempfile
#'
#' @param download_url 
#' @param destination_path 
#' @param file_ext
#'
#' @keywords internal
download_file <- function(download_url, destination_path, file_ext){
  
  temp <- tempfile(tmpdir = destination_path, fileext = file_ext)
  
  # if (!dir.exists(destination_path)) {dir.create(destination_path, recursive = TRUE)}
  
  # op <- options(timeout = 1000,
  #               download.file.method="curl",
  #               download.file.extra = paste0('--noproxy "*"'))
  # 
  # on.exit(options(op))
  
  command <- paste0("curl ", download_url, " --output ", temp)
  system(command, intern = TRUE)
  
  return(temp)
}


#' Download *.zip into tempfile & unzip & delete
#'
#' @param download_url 
#' @param destination_path 
#' @param file_filter 
#' @param file_ext
#'
#' @keywords internal
download_zip <- function(download_url, destination_path, file_filter = NULL, file_ext = ".zip"){
  
  temp <- download_file(download_url, destination_path, file_ext)
  
  # which files are in there?
  files <- unzip(temp, list = TRUE)
  file <- files$Name[stringr::str_detect(files$Name, file_filter)]
  
  # unzip specific file
  unzip(temp, files = file, exdir = destination_path, junkpaths = TRUE)
  
  # delete temp zip
  unlink(temp)
}


#' Download statpop *.zip from BFS api
#'
#' @param year 
#' @param destination_path 
#' @param file_filter 
#'
#' @keywords internal
download_statpop_data <- function(year, destination_path, file_filter = NULL){
  
  download_url <- get_bfs_statpop_metadata(year)
  download_zip(download_url, destination_path, file_filter)
  
}


#' just to bring ist into required structure
#'
#' @param years
#' @param data_raster_pm25
#' @param data_raster_pm10
#' @param data_raster_no2
#' @param data_raster_o3mp98
#' @param data_raster_ndep
#'
#' @keywords internal
combine_raster_aq <- function(years, data_raster_pm25, data_raster_pm10, data_raster_no2, data_raster_o3mp98, data_raster_ndep) {
  
  data_raster_aq <- 
    setNames(years$all, years$all) |> 
    purrr::map(function(year) list(
      pm25 = data_raster_pm25[[as.character(year)]]$pm25, 
      pm10 = data_raster_pm10[[as.character(year)]]$pm10,
      no2 = data_raster_no2[[as.character(year)]]$no2,
      mp98 = data_raster_o3mp98[[as.character(year)]]$mp98
    )) |> 
    purrr::map(function(x) x[which(!sapply(x, is.null))])
  
  return(data_raster_aq )
}

