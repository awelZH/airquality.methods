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

