# Classes of the nitrogen deposition analysis (Ostluft conventions): ecosystem categories, site
# classes by the agricultural pressure around a site, classes of the estimated part of the
# deposition and the source category of a deposition parameter. Vector in, vector out, so they fit
# into any data layout. Labels are German, as in the reports.

#' Recode ecosystem types into the categories of the nitrogen deposition analysis
#'
#' Forest types (Mischwald, Laubwald, Nadelwald) become "Wald", "Feuchtgebiet" becomes
#' "Flachmoor", settlements ("Siedlungen") count as "kein empf. Ökosys.". Other types become `NA`,
#' with a warning.
#'
#' @param ecosys Character vector of ecosystem types.
#' @param levels Categories in plot order.
#'
#' @return Factor with `levels`.
#'
#' @examples
#' recode_ecosystems(c("Mischwald", "Feuchtgebiet", "Hochmoor", "Siedlungen"))
#'
#' @export
recode_ecosystems <- function(ecosys,
                              levels = c("Wald", "Hochmoor", "Flachmoor", "Trockenrasen",
                                         "kein empf. \u00d6kosys.")) {
  recoded <- dplyr::case_when(
    stringr::str_detect(ecosys, "wald") ~ "Wald",
    ecosys == "Feuchtgebiet" ~ "Flachmoor",
    ecosys == "Siedlungen" ~ "kein empf. \u00d6kosys.",
    .default = ecosys
  )
  unknown <- setdiff(stats::na.omit(recoded), levels)
  if (length(unknown) > 0) {
    cli::cli_warn("Unknown ecosystem type{?s} {.val {unknown}} set to NA.")
  }
  factor(recoded, levels = levels)
}


#' Ostluft site class by the agricultural pressure within 5 km
#'
#' "tief": fertilisation <= 50 and livestock <= 5000; "hoch": fertilisation > 100 or livestock >
#' 10000; "mittel" in between. Thresholds of the Ostluft site classification.
#'
#' @param gve Livestock units within 5 km (GVE).
#' @param n_fertilization Nitrogen fertilisation within 5 km (kg N/ha/a).
#' @param gve_breaks,n_fertilization_breaks Lower and upper threshold.
#'
#' @return Factor with levels "hoch", "mittel", "tief".
#'
#' @examples
#' classify_ostluft_siteclass(gve = c(1000, 8000, 12000), n_fertilization = c(10, 60, 10))
#'
#' @export
classify_ostluft_siteclass <- function(gve, n_fertilization, gve_breaks = c(5000, 10000),
                                       n_fertilization_breaks = c(50, 100)) {
  siteclass <- dplyr::case_when(
    n_fertilization <= n_fertilization_breaks[1] & gve <= gve_breaks[1] ~ "tief",
    n_fertilization > n_fertilization_breaks[2] | gve > gve_breaks[2] ~ "hoch",
    n_fertilization <= n_fertilization_breaks[2] & gve <= gve_breaks[2] ~ "mittel"
  )
  factor(siteclass, levels = c("hoch", "mittel", "tief"))
}


#' NH3 emission class of a site (emissions within 1 km)
#'
#' @param emission NH3 emission within 1 km.
#' @param breaks,labels Class boundaries and labels; the lower bound of the first class is included.
#'
#' @return Factor "tief", "mittel", "hoch"; `NA` where `emission` is `NA`.
#'
#' @examples
#' classify_nh3_emission(c(5, 20, 40))
#'
#' @export
classify_nh3_emission <- function(emission, breaks = c(0, 10, 30, Inf), labels = c("tief", "mittel", "hoch")) {
  classify(emission, breaks, labels)
}


#' Class of the estimated (modelled) part of the nitrogen deposition
#'
#' @param estimated Estimated deposition in kg N/ha/a.
#' @inheritParams classify_nh3_emission
#'
#' @return Factor "<5 kg-N", "5-12 kg-N", ">12 kg-N".
#'
#' @examples
#' classify_estimated(c(2, 8, 15))
#'
#' @export
classify_estimated <- function(estimated, breaks = c(0, 5, 12, Inf), labels = c("<5 kg-N", "5-12 kg-N", ">12 kg-N")) {
  classify(estimated, breaks, labels)
}


#' Class of the estimated fraction of the nitrogen deposition
#'
#' @param frac_estimated Estimated part divided by the total deposition (0-1), not the estimated
#'   deposition itself.
#' @inheritParams classify_nh3_emission
#'
#' @return Factor "<33%", "33-66%", ">66%".
#'
#' @examples
#' classify_frac_estimated(c(0.1, 0.5, 0.9))
#'
#' @export
classify_frac_estimated <- function(frac_estimated, breaks = c(0, 0.33, 0.66, 1), labels = c("<33%", "33-66%", ">66%")) {
  classify(frac_estimated, breaks, labels)
}


#' Source category of a nitrogen deposition parameter
#'
#' @param parameter Parameter names, e.g. "NO2-N_ddep" or "NH4-N_bdep".
#'
#' @return "aus NOx-Quellen" (names containing "NO"), "aus NH3-Quellen" (names containing "NH")
#'   or `NA`.
#'
#' @examples
#' derive_source_category(c("NO2-N_ddep", "NH4-N_bdep"))
#'
#' @export
derive_source_category <- function(parameter) {
  dplyr::case_when(
    stringr::str_detect(parameter, "NO") ~ "aus NOx-Quellen",
    stringr::str_detect(parameter, "NH") ~ "aus NH3-Quellen"
  )
}


# values into labelled classes, the lower bound of the first class included
classify <- function(x, breaks, labels) {
  cut(x, breaks = breaks, include.lowest = TRUE, labels = labels)
}
