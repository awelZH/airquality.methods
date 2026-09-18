# Generic summary statistics for long data.

#' Summarise a value column per group
#'
#' Produces the set of statistics the air quality figures draw from: counts,
#' percentiles for box and ribbon plots, mean, spread and sum.
#'
#' Two deliberate properties:
#' * Group combinations that do not occur in the data are added with `n = 0` and
#'   `NA` statistics, so a gap stays visible in a figure instead of silently
#'   closing.
#' * Groups with fewer than `nmin` observations get `NA` statistics, because a
#'   percentile of two values is not a percentile.
#'
#' @param data A data frame in long format.
#' @param y Name of the value column, as a string.
#' @param groups Names of the grouping columns, as a character vector.
#' @param nmin Smallest group size for which statistics are reported.
#' @param perc Percentiles used for `minlower`, `lower`, `middle`, `upper` and
#'   `maxupper`.
#'
#' @return A tibble with one row per group combination.
#'
#' @examples
#' data <- data.frame(site = rep(c("A", "B"), each = 5), value = 1:10)
#' aggregate_groups(data, "value", "site")
#'
#' @export
aggregate_groups <- function(data,
                             y,
                             groups,
                             nmin = 3,
                             perc = list(ymin = 0.05, lower = 0.25, middle = 0.5,
                                         upper = 0.75, ymax = 0.95)) {
  check_names(names(data), c(y, groups), "data")

  statistics <- c(
    "minimum", "minlower", "lower", "middle", "upper", "maxupper", "maximum",
    "mean", "standarddeviation", "standarderror", "medianabsolutedeviation", "sum"
  )

  # min() and max() warn and return +/-Inf on an all-NA group. The Inf would be
  # blanked below anyway, but the warning would reach the user for nothing.
  empty_safe <- function(values, fun, ...) {
    values <- values[!is.na(values)]
    if (length(values) == 0) NA_real_ else fun(values, ...)
  }
  quantile_of <- function(values, p) {
    empty_safe(values, \(v) unname(stats::quantile(v, p)))
  }

  summarised <- data |>
    dplyr::summarise(
      n = sum(!is.na(.data[[y]])),
      minimum = empty_safe(.data[[y]], min),
      minlower = quantile_of(.data[[y]], perc$ymin),
      lower = quantile_of(.data[[y]], perc$lower),
      middle = quantile_of(.data[[y]], perc$middle),
      upper = quantile_of(.data[[y]], perc$upper),
      maxupper = quantile_of(.data[[y]], perc$ymax),
      maximum = empty_safe(.data[[y]], max),
      mean = empty_safe(.data[[y]], mean),
      standarddeviation = empty_safe(.data[[y]], stats::sd),
      medianabsolutedeviation = empty_safe(.data[[y]], stats::mad),
      standarderror = .data$standarddeviation / sqrt(.data$n),
      sum = sum(.data[[y]], na.rm = TRUE),
      .by = dplyr::all_of(groups)
    )

  summarised |>
    tidyr::complete(!!!rlang::syms(groups)) |>
    dplyr::mutate(
      n = dplyr::coalesce(.data$n, 0L),
      # too few observations, or an empty group: no statistic is meaningful.
      # NaN and Inf arise from min()/mean() on nothing, and are not results either.
      dplyr::across(
        dplyr::all_of(statistics),
        \(value) dplyr::if_else(.data$n < nmin | !is.finite(value), NA_real_, value)
      )
    ) |>
    dplyr::relocate(dplyr::all_of(c(groups, "n", statistics)))
}
