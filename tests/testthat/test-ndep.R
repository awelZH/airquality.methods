test_that("recode_ecosystems() merges forest types and wetlands and orders the levels", {
  result <- recode_ecosystems(c("Mischwald", "Laubwald", "Nadelwald", "Wald", "Feuchtgebiet", "Flachmoor",
                                "Hochmoor", "Trockenrasen", "kein empf. Ökosys."))

  expect_s3_class(result, "factor")
  expect_equal(levels(result), c("Wald", "Hochmoor", "Flachmoor", "Trockenrasen", "kein empf. Ökosys."))
  expect_equal(as.character(result), c("Wald", "Wald", "Wald", "Wald", "Flachmoor", "Flachmoor",
                                       "Hochmoor", "Trockenrasen", "kein empf. Ökosys."))
})

test_that("recode_ecosystems() counts settlements as not sensitive", {
  expect_equal(as.character(recode_ecosystems("Siedlungen")), "kein empf. Ökosys.")
})

test_that("recode_ecosystems() warns about unknown ecosystems, which become NA", {
  expect_warning(result <- recode_ecosystems(c("Wald", "Gletscher")), "Gletscher")
  expect_equal(as.character(result), c("Wald", NA))
  expect_no_warning(recode_ecosystems(c("Wald", NA)))
})

test_that("classify_ostluft_siteclass() follows the Ostluft thresholds (livestock units, fertilisation)", {
  gve <-   c(5000, 5000,  5001, 10000, 10001,  100, NA)
  nfert <- c(  50,   51,   100,   100,    10,  101, 10)

  result <- classify_ostluft_siteclass(gve, nfert)

  expect_equal(levels(result), c("hoch", "mittel", "tief"))
  expect_equal(as.character(result), c("tief", "mittel", "mittel", "mittel", "hoch", "hoch", NA))
})

test_that("classify_nh3_emission() cuts at 10 and 30 (lower bounds included)", {
  result <- classify_nh3_emission(c(0, 10, 10.1, 30, 31, NA))

  expect_equal(levels(result), c("tief", "mittel", "hoch"))
  expect_equal(as.character(result), c("tief", "tief", "mittel", "mittel", "hoch", NA))
})

test_that("classify_estimated() and classify_frac_estimated() cut the estimated part", {
  expect_equal(as.character(classify_estimated(c(0, 5, 6, 12, 13))),
               c("<5 kg-N", "<5 kg-N", "5-12 kg-N", "5-12 kg-N", ">12 kg-N"))
  expect_equal(as.character(classify_frac_estimated(c(0, 0.33, 0.5, 0.66, 1))),
               c("<33%", "<33%", "33-66%", "33-66%", ">66%"))
})

test_that("the class boundaries and labels can be set", {
  result <- classify_nh3_emission(c(1, 3), breaks = c(0, 2, Inf), labels = c("low", "high"))

  expect_equal(as.character(result), c("low", "high"))
})

test_that("derive_source_category() assigns NOx and NH3 sources by the parameter name", {
  result <- derive_source_category(c("NO2-N_ddep", "NO3-N_bdep", "NH4-N_bdep", "NH3-N_ddep", "other"))

  expect_equal(result, c("aus NOx-Quellen", "aus NOx-Quellen", "aus NH3-Quellen", "aus NH3-Quellen", NA))
})
