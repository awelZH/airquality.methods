test_that("longpollutant translates the known codes", {
  expect_equal(longpollutant("PM10"), "Feinstaub PM10")
  expect_equal(longpollutant("NO2"), "Stickstoffdioxid")
  expect_equal(longpollutant("eBC"), "Russ im Feinstaub")
})

test_that("longpollutant maps every ozone metric onto ozone", {
  ozone <- c("O3", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl",
             "O3_nb_h1>120", "O3_nb_d1_max_h1>120")

  expect_equal(longpollutant(ozone), rep("Ozon", length(ozone)))
})

test_that("longpollutant passes unknown codes through unchanged", {
  expect_equal(longpollutant(c("PM10", "unknown")), c("Feinstaub PM10", "unknown"))
  expect_equal(longpollutant(character(0)), character(0))
})

test_that("shortpollutant reduces ozone metrics to the pollutant", {
  expect_equal(shortpollutant("O3_max_98p_m1"), "O3")
  expect_equal(shortpollutant("O3_max_h1"), "O3")
  expect_equal(shortpollutant("NO2"), "NO2")
})

test_that("longmetric distinguishes yearly and daily intervals", {
  expect_equal(longmetric("PM10"), "Jahresmittel")
  expect_equal(longmetric("PM10", interval = "d1"), "Tagesmittel")
  expect_equal(longmetric("O3_max_h1", interval = "d1"), "höchstes Stundenmittel")
})

test_that("longmetric sums nitrogen deposition instead of averaging it", {
  expect_equal(longmetric(c("Ndep", "N-Eintrag")), rep("Jahressumme", 2))
})

test_that("longmetric rejects an unknown interval", {
  expect_error(longmetric("PM10", interval = "h1"), "y1")
})

test_that("longparameter falls back to the annual mean", {
  expect_equal(longparameter("NO2"), "Jahresmittel")
  expect_equal(longparameter("O3_max_98p_m1"), "max. monatl. 98%-Perz.")
  expect_equal(longparameter("O3_peakseason_mean_d1_max_mean_h8gl"), "Sommersaison")
})

test_that("the recoders are vectorised and keep the input length", {
  x <- c("PM10", "NO2", "O3_max_98p_m1", "unknown")

  expect_length(longpollutant(x), 4)
  expect_length(shortpollutant(x), 4)
  expect_length(longmetric(x), 4)
  expect_length(longparameter(x), 4)
})

test_that("markdown_text writes pollutants with subscripts and cubic metres with a superscript", {
  expect_equal(markdown_text("NO2"), "NO<sub>2</sub>")
  expect_equal(markdown_text("PM2.5 und PM10"), "PM<sub>2.5</sub> und PM<sub>10</sub>")
  expect_equal(markdown_text("O3, NH3, NOx, SO2"), "O<sub>3</sub>, NH<sub>3</sub>, NO<sub>x</sub>, SO<sub>2</sub>")
  expect_equal(markdown_text("NO2\n(µg/m3)"), "NO<sub>2</sub><br>(µg/m<sup>3</sup>)")
})

test_that("markdown_text leaves other words alone and escapes angle brackets", {
  expect_equal(markdown_text("Ndep > CLN\n(kgN/ha/Jahr)"), "Ndep &gt; CLN<br>(kgN/ha/Jahr)")
  expect_equal(markdown_text("NO2x PM25 m3s NOx2"), "NO2x PM25 m3s NOx2")
  expect_equal(markdown_text(c("NO2", NA)), c("NO<sub>2</sub>", NA))
})
