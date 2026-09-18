test_that("as_bbox returns NULL for NULL", {
  expect_null(as_bbox(NULL))
})

test_that("as_bbox turns a plain numeric vector into an sf bbox in LV95", {
  bb <- as_bbox(c(2670000, 1230000, 2700000, 1260000))

  expect_s3_class(bb, "bbox")
  expect_equal(sf::st_crs(bb), sf::st_crs(2056))
  expect_equal(unname(bb[["xmin"]]), 2670000)
  expect_equal(unname(bb[["ymax"]]), 1260000)
})

test_that("as_bbox reorders named input and rejects unknown names", {
  bb <- as_bbox(c(ymin = 1230000, xmax = 2700000, xmin = 2670000, ymax = 1260000))
  expect_equal(unname(bb[["xmin"]]), 2670000)
  expect_equal(unname(bb[["ymin"]]), 1230000)

  expect_error(as_bbox(c(left = 1, bottom = 2, right = 3, top = 4)), "xmin")
})

test_that("as_bbox rejects malformed extents", {
  expect_error(as_bbox(c(1, 2, 3)), "xmin, ymin, xmax, ymax")
  expect_error(as_bbox(c(1, 2, NA, 4)), "xmin, ymin, xmax, ymax")
  expect_error(as_bbox("2670000,1230000"), "xmin, ymin, xmax, ymax")
  # xmin must be left of xmax
  expect_error(as_bbox(c(2700000, 1230000, 2670000, 1260000)), "invalid")
  expect_error(as_bbox(c(2670000, 1260000, 2700000, 1230000)), "invalid")
})

test_that("as_bbox catches degrees passed for a projected crs", {
  # a classic mistake: WGS84 coordinates handed to an LV95 function
  expect_error(as_bbox(c(8.4, 47.3, 8.7, 47.5)), "degrees")
})

test_that("as_bbox takes the extent of sf, sfc, bbox and stars objects", {
  poly <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = 2670000, ymin = 1230000, xmax = 2700000, ymax = 1260000),
    crs = sf::st_crs(2056)
  ))

  expect_equal(as_bbox(poly), sf::st_bbox(poly))
  expect_equal(as_bbox(sf::st_sf(geometry = poly)), sf::st_bbox(poly))
  expect_equal(as_bbox(sf::st_bbox(poly)), sf::st_bbox(poly))
})

test_that("as_bbox transforms objects that carry a different crs", {
  poly_wgs <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = 8.4, ymin = 47.3, xmax = 8.7, ymax = 47.5),
    crs = sf::st_crs(4326)
  ))

  bb <- as_bbox(poly_wgs, crs = 2056)

  expect_equal(sf::st_crs(bb), sf::st_crs(2056))
  # roughly the city of Zurich in LV95
  expect_true(bb[["xmin"]] > 2650000 && bb[["xmin"]] < 2690000)
  expect_true(bb[["ymin"]] > 1230000 && bb[["ymin"]] < 1260000)
})

test_that("transform_bbox encloses the curved edges, not just the corners", {
  bb <- sf::st_bbox(
    c(xmin = 2600000, ymin = 1180000, xmax = 2800000, ymax = 1300000),
    crs = sf::st_crs(2056)
  )

  densified <- transform_bbox(bb, 4326)
  corners_only <- sf::st_bbox(sf::st_transform(
    sf::st_sfc(sf::st_multipoint(rbind(
      c(bb[["xmin"]], bb[["ymin"]]), c(bb[["xmax"]], bb[["ymin"]]),
      c(bb[["xmax"]], bb[["ymax"]]), c(bb[["xmin"]], bb[["ymax"]])
    )), crs = sf::st_crs(2056)),
    4326
  ))

  # densifying can only widen the box, never shrink it
  expect_lte(densified[["xmin"]], corners_only[["xmin"]])
  expect_gte(densified[["xmax"]], corners_only[["xmax"]])
  expect_lte(densified[["ymin"]], corners_only[["ymin"]])
  expect_gte(densified[["ymax"]], corners_only[["ymax"]])
})

test_that("the package ships the documented default extents", {
  expect_named(bbox_zh_lv95, c("xmin", "ymin", "xmax", "ymax"))
  expect_named(bbox_ch_lv95, c("xmin", "ymin", "xmax", "ymax"))
  # Zurich lies inside Switzerland
  expect_true(bbox_zh_lv95[["xmin"]] >= bbox_ch_lv95[["xmin"]])
  expect_true(bbox_zh_lv95[["xmax"]] <= bbox_ch_lv95[["xmax"]])
})
