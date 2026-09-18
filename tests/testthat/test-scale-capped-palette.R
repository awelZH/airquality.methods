# One palette argument accepts four different kinds of thing. The point of
# these tests is that each kind resolves to the colours the user would get
# from the source it names -- compared against that source, not against a
# hard-coded list of hex codes that says nothing when it breaks.

test_that("the default palette is magma, by every spelling of it", {
  ref <- scales::pal_viridis(option = "A")(5)

  expect_equal(.capped_palette()$n_pal(5), ref)
  expect_equal(.capped_palette("Magma")$n_pal(5), ref)
  expect_equal(.capped_palette("magma")$n_pal(5), ref)
  expect_equal(.capped_palette("A")$n_pal(5), ref)
})

test_that("the other viridis maps resolve too, including the ones hcl.pals lacks", {
  expect_equal(.capped_palette("Viridis")$n_pal(4), scales::pal_viridis(option = "D")(4))
  expect_equal(.capped_palette("Rocket")$n_pal(4),  scales::pal_viridis(option = "F")(4))
  expect_equal(.capped_palette("Turbo")$n_pal(4),   scales::pal_viridis(option = "H")(4))

  # magma, rocket, mako, turbo and cividis are not in hcl.pals() at all --
  # that is exactly why the viridis lookup comes first
  expect_false("Magma" %in% grDevices::hcl.pals())
})

test_that("a grDevices palette name is passed through to hcl.colors", {
  expect_equal(.capped_palette("Zissou 1")$n_pal(6),
               grDevices::hcl.colors(6, palette = "Zissou 1"))
  expect_equal(.capped_palette("YlOrRd")$n_pal(3),
               grDevices::hcl.colors(3, palette = "YlOrRd"))
})

test_that("a colour vector is used as given, and interpolated when it must be", {
  cols <- c("white", "steelblue", "black")
  pal  <- .capped_palette(cols)

  expect_equal(pal$n_pal(3), cols)                       # exact length: as-is
  expect_length(pal$n_pal(7), 7)                          # otherwise a ramp
  expect_equal(pal$ramp(c(0, 1)),
               grDevices::colorRampPalette(cols, space = "Lab")(2))
})

test_that("a palette function is called, and repaired if it returns the wrong length", {
  expect_equal(.capped_palette(scales::pal_viridis())$n_pal(4),
               scales::pal_viridis()(4))

  # RColorBrewer-style: never returns fewer than three colours
  stubborn <- function(n) rep(c("#000000", "#888888", "#FFFFFF"), length.out = max(n, 3L))
  expect_length(.capped_palette(stubborn)$n_pal(2), 2L)

  # and one that errors on small n is caught rather than propagated
  fragile <- function(n) if (n < 3) stop("too few") else rep("#123456", n)
  expect_length(.capped_palette(fragile)$n_pal(1), 1L)
})

test_that("direction = -1 reverses the colours, not just the labels", {
  fwd <- .capped_palette("Magma")
  rev <- .capped_palette("Magma", direction = -1)

  expect_equal(rev$n_pal(5), base::rev(fwd$n_pal(5)))
  expect_equal(rev$ramp(0), fwd$ramp(1))
  expect_error(.capped_palette("Magma", direction = 0), "direction")
})

test_that("values shifts the ramp without changing its ends", {
  plain <- .capped_palette(c("white", "black"))
  bent  <- .capped_palette(c("white", "grey50", "black"), values = c(0, 0.9, 1))

  expect_equal(bent$ramp(0), plain$ramp(0))
  expect_equal(bent$ramp(1), plain$ramp(1))
  # the middle colour now sits at 0.9, so 0.5 is still on the light side
  expect_false(identical(bent$ramp(0.5), plain$ramp(0.5)))
})

test_that("a single colour still yields a usable ramp", {
  pal <- .capped_palette("red")
  expect_equal(pal$n_pal(1), "red")
  expect_equal(pal$ramp(c(0, 0.5, 1)), rep(grDevices::rgb(1, 0, 0), 3))
})

test_that("anything else is refused with a message that lists the options", {
  expect_error(.capped_palette(list("red", "blue")), "colour vector")
  expect_error(.capped_palette(42), "colour vector")
})
