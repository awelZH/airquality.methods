# read_bafu_raster_data warns as deprecated

    Code
      x <- read_bafu_raster_data("ch.bafu.luftreinhaltung-feinstaub_pm2_5", 2020,
        bbox_zh_lv95)
    Condition
      Warning:
      `read_bafu_raster_data()` was deprecated in airquality.methods 0.4.0.
      i Use read_geo_admin() or read_collection_rasters(); see vignette('geodata').

