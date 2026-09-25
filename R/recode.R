# Recoding short pollutant and parameter codes into the long German labels used
# in figures and tables.

#' Long pollutant name for a short code
#'
#' Unknown codes are returned unchanged, so the function can be applied to a
#' whole column without losing values.
#'
#' @param x Character vector of pollutant or parameter codes.
#'
#' @return Character vector of long names.
#'
#' @examples
#' longpollutant(c("PM10", "NO2", "unknown"))
#'
#' @export
longpollutant <- function(x) {
  dplyr::recode_values(
    x,
    "PM10" ~ "Feinstaub PM10",
    "PM2.5" ~ "Feinstaub PM2.5",
    "NO2" ~ "Stickstoffdioxid",
    "NO2 | NOx" ~ "Stickstoxide NO2 | NOx",
    "NMVOC" ~ "nicht-Methan Kohlenwasserstoffe",
    "NH3" ~ "Ammoniak",
    "NHx" ~ "reduzierter Stickstoff",
    "NHx | NH3" ~ "reduzierter Stickstoff NHx | NH3",
    "Nr" ~ "reaktiver Stickstoff",
    "CO" ~ "Kohlenstoffmonoxid",
    "SO2" ~ "Schwefeldioxid",
    "NOx" ~ "Stickoxide",
    "eBC" ~ "Russ im Feinstaub",
    c("O3", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl",
      "O3_nb_h1>120", "O3_nb_d1_max_h1>120") ~ "Ozon",
    c("N-Eintrag", "Ndep") ~ "Stickstoffeintrag in empfindliche \u00d6kosysteme",
    default = x
  )
}

#' Short pollutant code for a parameter code
#'
#' Collapses the ozone metrics onto the pollutant they describe.
#'
#' @param x Character vector of parameter codes.
#'
#' @return Character vector of pollutant codes.
#'
#' @examples
#' shortpollutant(c("O3_max_98p_m1", "NO2"))
#'
#' @export
shortpollutant <- function(x) {
  dplyr::recode_values(
    x,
    c("O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl", "O3_nb_h1>120",
      "O3_max_h1", "O3_nb_d1_max_h1>120") ~ "O3",
    default = x
  )
}

#' Long metric name for a pollutant or parameter code
#'
#' @param x Character vector of pollutant or parameter codes.
#' @param interval Averaging interval: `"y1"` (yearly) or `"d1"` (daily).
#'
#' @return Character vector of metric names.
#'
#' @examples
#' longmetric(c("PM10", "Ndep", "O3_max_98p_m1"))
#' longmetric("PM10", interval = "d1")
#'
#' @export
longmetric <- function(x, interval = c("y1", "d1")) {
  interval <- rlang::arg_match(interval)

  if (interval == "d1") {
    return(dplyr::recode_values(
      x,
      c("PM10", "PM2.5", "NO2", "NH3", "NMVOC", "CO", "SO2", "NOx", "eBC", "O3") ~ "Tagesmittel",
      "O3_max_h1" ~ "h\u00f6chstes Stundenmittel",
      default = x
    ))
  }

  dplyr::recode_values(
    x,
    c("PM10", "PM2.5", "NO2", "NH3", "NHx", "Nr", "NMVOC", "CO", "SO2", "NOx", "eBC", "O3") ~
      "Jahresmittel",
    c("N-Eintrag", "Ndep") ~ "Jahressumme",
    "O3_max_98p_m1" ~ "typische Spitzenbelastung",
    "O3_peakseason_mean_d1_max_mean_h8gl" ~ "mittlere Sommertagbelastung",
    "O3_nb_h1>120" ~ "Anzahl Stundenmittel > 120 \u00b5g/m3",
    "O3_nb_d1_max_h1>120" ~ "Anzahl Tage h\u00f6chstes Stundenmittel > 120 \u00b5g/m3",
    default = x
  )
}

#' Short metric label for a parameter code
#'
#' The compact variant used inside plot annotations, where space is tight.
#' Anything that is not an explicit ozone metric is an annual mean.
#'
#' @param x Character vector of parameter codes.
#'
#' @return Character vector of short labels.
#'
#' @examples
#' longparameter(c("O3_max_98p_m1", "NO2"))
#'
#' @export
longparameter <- function(x) {
  dplyr::recode_values(
    x,
    "O3_max_98p_m1" ~ "max. monatl. 98%-Perz.",
    "O3_peakseason_mean_d1_max_mean_h8gl" ~ "Sommersaison",
    "O3_max_h1" ~ "h\u00f6chstes Stundenmittel",
    "O3_nb_d1_max_h1>120" ~ "Anzahl Tage h\u00f6chstes Stundenmittel > 120 \u00b5g/m3",
    default = "Jahresmittel"
  )
}

#' Write pollutants and units as markdown for ggtext
#'
#' The markdown counterpart of [openair::quickText()] for text that ggtext renders
#' (e.g. a legend title with `ggtext::element_markdown()`): pollutant formulas get
#' subscripts (NO2, NOx, SO2, CO2, NH3, O3, PM10, PM2.5, PM1), square and cubic
#' metres a superscript (m2, m3), line breaks become `<br>`. Unlike plotmath this
#' works in text over several lines. Angle brackets and ampersands in the text are
#' escaped first, so they are not taken for HTML.
#'
#' @param x Character vector.
#'
#' @return `x` as markdown; `NA` stays `NA`.
#'
#' @examples
#' markdown_text(c("NO2", "PM2.5 and PM10", "O3 (ug/m3)"))
#'
#' @export
markdown_text <- function(x) {
  x |>
    stringr::str_replace_all(c("&" = "&amp;", "<" = "&lt;", ">" = "&gt;")) |>
    stringr::str_replace_all("\\bPM(10|2\\.5|1)\\b", "PM<sub>\\1</sub>") |>
    stringr::str_replace_all("\\b(NO|SO|CO|NH)([0-9x])\\b", "\\1<sub>\\2</sub>") |>
    stringr::str_replace_all("\\bO3\\b", "O<sub>3</sub>") |>
    stringr::str_replace_all("\\bm([23])\\b", "m<sup>\\1</sup>") |>
    stringr::str_replace_all("\n", "<br>")
}
