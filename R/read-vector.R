# Readers for vector data from the cantonal geolion services.

#' Build a WFS GetFeature request url
#'
#' @param apiurl Base url of the WFS service.
#' @param type Feature type name, for example `"ms:gem_grenzen"`.
#' @param version WFS version.
#' @param crs EPSG code requested from the service.
#'
#' @return The request url as a string.
#'
#' @keywords internal
get_geolion_wfs_metadata <- function(apiurl,
                                     type = "ms:gem_grenzen",
                                     version = "2.0.0",
                                     crs = 2056) {
  url <- httr2::url_parse(apiurl)
  url$query <- list(
    service = "wfs",
    version = version,
    request = "GetFeature",
    typename = type,
    srsName = paste0("EPSG:", crs)
  )

  httr2::url_build(url)
}

#' Read polygon data from the Canton of Zurich geolion WFS
#'
#' @param apiurl Base url of the WFS service.
#' @param type Feature type name, for example `"ms:gem_grenzen"` (municipality
#'   boundaries) or `"ms:gem_seen_grenzen"` (boundaries including lakes).
#' @param version WFS version.
#' @param crs Coordinate reference system of the result.
#'
#' @return An `sf` object in `crs`.
#'
#' @examplesIf interactive()
#' municipalities <- read_geolion_wfs("https://maps.zh.ch/wfs/GemZHWFS")
#'
#' @export
read_geolion_wfs <- function(apiurl, type = "ms:gem_grenzen", version = "2.0.0", crs = 2056) {
  get_geolion_wfs_metadata(apiurl, type = type, version = version, crs = crs) |>
    sf::read_sf(type = 6) |>
    sf::st_transform(crs = sf::st_crs(crs))
}
