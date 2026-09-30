# Report metadata that travels with the plot but is never drawn.

plain <- function() ggplot2::ggplot(data.frame(x = 1, y = 1),
                                    ggplot2::aes(x, y)) + ggplot2::geom_point()

test_that("fig_meta() stores the texts and returns the plot", {
  p <- fig_meta(plain(), title = "Titel", caption = "Ein Satz.",
                note = "Methodisches")

  expect_true(ggplot2::is_ggplot(p))
  expect_equal(fig_title(p), "Titel")
  expect_equal(fig_caption(p), "Ein Satz.")
  expect_equal(fig_note(p), "Methodisches")
})

test_that("the alt text is generated from title and caption, and is ggplot2's", {
  p <- fig_meta(plain(), title = "Titel", caption = "Ein Satz.")

  expect_equal(fig_alt(p), "Titel. Ein Satz.")
  # not a second copy: it is written into the label ggplot2 and knitr read
  expect_equal(ggplot2::get_alt_text(p), "Titel. Ein Satz.")
  # ... and an explicit alt wins
  expect_equal(fig_alt(fig_meta(plain(), title = "T", alt = "eigener Text")),
               "eigener Text")
})

test_that("nothing of it is drawn", {
  p <- fig_meta(plain(), title = "Titel", caption = "Ein Satz.",
                note = "Methodisches")
  labels <- grob_labels(plot_gtable(p))

  expect_false(any(grepl("Titel|Ein Satz|Methodisches", labels)))
})

test_that("the metadata survives the layers, scales and themes added after it", {
  p <- fig_meta(plain(), title = "Titel", caption = "Ein Satz.") +
    ggplot2::labs(x = "x") + ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~x)

  expect_equal(fig_title(p), "Titel")
  expect_equal(fig_caption(p), "Ein Satz.")
})

test_that("a second call adds to the first instead of replacing it", {
  p <- fig_meta(plain(), title = "Titel")
  p <- fig_meta(p, caption = "Ein Satz.")

  expect_equal(fig_title(p), "Titel")
  expect_equal(fig_caption(p), "Ein Satz.")
})

test_that("a missing field is an error, not a silently empty caption", {
  p <- fig_meta(plain(), title = "Titel")

  expect_error(fig_caption(p), "carries no caption")
  expect_equal(fig_caption(p, default = ""), "")
})

test_that("fig_meta() refuses anything that is not a plot", {
  # The trap it exists for: `|>` binds tighter than `+`, so
  # `ggplot() + theme() |> fig_meta()` would hand over the theme.
  expect_error(fig_meta(ggplot2::theme_minimal(), title = "T"),
               "must be a ggplot")
  expect_error(fig_meta(plain(), title = c("zwei", "Titel")),
               "single string")
})

test_that("fig_index() tables the figures of a script", {
  ps <- list(p1 = fig_meta(plain(), title = "A", caption = "erste."),
             p2 = fig_meta(plain(), title = "B"))
  idx <- fig_index(ps)

  expect_equal(idx$figure, c("p1", "p2"))
  expect_equal(idx$title, c("A", "B"))
  expect_equal(idx$caption, c("erste.", ""))
  expect_identical(idx, fig_index(p1 = ps$p1, p2 = ps$p2))
  expect_error(fig_index(fig_meta(plain(), title = "A")), "must be named")
})

test_that("the texts survive a trip through the plot catalog", {
  # The two are meant to be used together: the catalog carries the plot to the
  # page, the page asks the plot for its caption.
  p <- fig_meta(plain(), title = "Titel", caption = "Ein Satz.")
  catalog <- plot_catalog(list(NO2 = p), "map", names_to = "parameter")

  expect_equal(fig_caption(get_plot(catalog, "map", parameter = "NO2")), "Ein Satz.")
})
