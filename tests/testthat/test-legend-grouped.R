# Unit tests for the grouped legend (R/legend-grouped.R). All inputs are synthetic; no network.

skip_if_not_installed("legendry")

# all text labels drawn in a grob tree (legend titles, key labels, ...)
grob_texts <- function(grob) {
  texts <- if (inherits(grob, "text") || inherits(grob, "titleGrob")) as.character(grob$label) else character()
  children <- c(if (inherits(grob, "gTree")) grob$children else list(), if (inherits(grob, "gtable")) grob$grobs else list())
  c(texts, unlist(purrr::map(children, grob_texts)))
}

legend_texts <- function(plot) {
  gt <- withr::with_pdf(NULL, ggplot2::ggplotGrob(plot))
  boxes <- gt$grobs[grepl("^guide-box", gt$layout$name)]
  unlist(purrr::map(boxes, grob_texts))
}

make_grouped_data <- function() {
  tibble::tibble(
    year = rep(2000:2001, each = 5),
    sector = rep(c("Verkehr", "Verkehr", "Haushalte", "Industrie", "Industrie"), 2),
    subsector = rep(c("Strasse", "verschiedene", "Feuerungen", "Lösungsmittel", "verschiedene"), 2),
    order = rep(c(4, 5, 1, 2, 3), 2),
    emission = 1:10
  )
}

# ---- grouped_key() ---------------------------------------------------------------

test_that("grouped_key() makes one unique key per group and element, ordered by `order`", {
  data <- make_grouped_data()

  key <- grouped_key(data$sector, data$subsector, data$order)

  expect_s3_class(key, "factor")
  expect_equal(levels(key), c("Haushalte::Feuerungen", "Industrie::Lösungsmittel", "Industrie::verschiedene",
                              "Verkehr::Strasse", "Verkehr::verschiedene"))
  expect_equal(as.character(key[1]), "Verkehr::Strasse")
})

test_that("grouped_key() keeps the order of appearance without `order`", {
  key <- grouped_key(c("B", "A", "B"), c("x", "y", "z"))

  expect_equal(levels(key), c("B::x", "B::z", "A::y"))
})

test_that("grouped_key() moves groups with `group_order`, keeping the order within groups", {
  data <- make_grouped_data()

  key <- grouped_key(data$sector, data$subsector, data$order, group_order = c("Verkehr", "Haushalte", "Industrie"))

  expect_equal(levels(key), c("Verkehr::Strasse", "Verkehr::verschiedene", "Haushalte::Feuerungen",
                              "Industrie::Lösungsmittel", "Industrie::verschiedene"))
})

test_that("grouped_key() stops if a name contains the separator", {
  expect_error(grouped_key("A::B", "x"), "::")
})

# ---- add_grouped_legend() ----------------------------------------------------------

make_grouped_plot <- function() {
  data <- make_grouped_data()
  data$key <- grouped_key(data$sector, data$subsector, data$order)
  colours <- c("Haushalte::Feuerungen" = "green", "Industrie::Lösungsmittel" = "blue", "Industrie::verschiedene" = "lightblue",
               "Verkehr::Strasse" = "black", "Verkehr::verschiedene" = "gray")
  ggplot2::ggplot(data, ggplot2::aes(year, emission, fill = key)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = colours)
}

test_that("add_grouped_legend() shows group titles and the elements without the group", {
  plot <- add_grouped_legend(make_grouped_plot())

  texts <- legend_texts(plot)
  expect_s3_class(plot, "ggplot")
  expect_contains(texts, c("Haushalte", "Industrie", "Verkehr"))
  expect_contains(texts, c("Feuerungen", "Lösungsmittel", "Strasse"))
  expect_equal(sum(texts == "verschiedene"), 2)
  expect_false(any(grepl("::", texts)))
})

test_that("add_grouped_legend() draws the group titles like the legend labels (not bold, same size)", {
  plot <- add_grouped_legend(make_grouped_plot() + ggplot2::theme_minimal(base_size = 11))

  gt <- withr::with_pdf(NULL, ggplot2::ggplotGrob(plot))
  text_grobs <- function(grob) {
    own <- if (inherits(grob, "text")) list(grob) else list()
    children <- c(if (inherits(grob, "gTree")) grob$children else list(), if (inherits(grob, "gtable")) grob$grobs else list())
    c(own, unlist(purrr::map(children, text_grobs), recursive = FALSE))
  }
  texts <- unlist(purrr::map(gt$grobs[grepl("^guide-box", gt$layout$name)], text_grobs), recursive = FALSE)
  style <- function(label) {
    grob <- purrr::detect(texts, \(g) identical(as.character(g$label), label))
    c(fontsize = grob$gp$fontsize, font = grob$gp$font %||% 1)
  }
  expect_equal(style("Haushalte"), style("Feuerungen"))
})

test_that("add_grouped_legend() puts the keys of a block without gaps, as in a ggplot legend", {
  plot <- add_grouped_legend(make_grouped_plot() + ggplot2::theme_minimal())

  spacing <- ggplot2::calc_element("legend.key.spacing.y", ggplot2::complete_theme(plot$theme))
  expect_equal(as.numeric(spacing), 0)
})

test_that("add_grouped_legend() keeps the colours of the plot's scale", {
  plot <- add_grouped_legend(make_grouped_plot())

  built <- ggplot2::ggplot_build(plot)
  expect_setequal(unique(built$data[[1]]$fill), c("green", "blue", "lightblue", "black", "gray"))
})

test_that("add_grouped_legend() keeps a normal ggplot: later theme changes still apply to the legend", {
  plot <- add_grouped_legend(make_grouped_plot()) + ggplot2::theme(legend.position = "bottom")

  gt <- withr::with_pdf(NULL, ggplot2::ggplotGrob(plot))
  bottom <- gt$grobs[[which(gt$layout$name == "guide-box-bottom")]]
  expect_s3_class(bottom, "gtable")
  expect_contains(legend_texts(plot), "Haushalte")
})

test_that("add_grouped_legend() stops if the aesthetic has no scale", {
  expect_error(add_grouped_legend(make_grouped_plot(), aesthetic = "colour"), "colour")
})
