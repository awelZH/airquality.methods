# airquality.methods

Dieses Repository enthält Funktionen zu systematischen Auswertungen zu Luftschadstoffen im Kanton
Zürich durch das kantonale [Amt für Abfall, Wasser, Energie und
Luft](https://www.zh.ch/de/baudirektion/amt-fuer-abfall-wasser-energie-luft.html).

Die Datensammlung, Auswertungen und Darstellungen, welche die Funktionen verwenden, werden im
GitHub-Repository ["awelZH/airquality"](https://github.com/awelZH/airquality) durchgeführt und
dokumentiert.

## Installation

```r
pak::pak("awelZH/airquality.methods")
```

## Überblick

### Geodaten von data.geo.admin.ch

Mehrere Collections einlesen, auf ein gemeinsames Raster bringen und als Würfel auswerten:

```r
library(airquality.methods)

data <- read_geo_admin(
  c("ch.bfs.statistik-bevoelkerung_haushalte",
    "ch.bafu.luftreinhaltung-feinstaub_pm2_5"),
  years = 2015:2025
)

cube <- data |>
  align_to_grid(make_reference_grid(bbox_zh_lv95, cellsize = 250)) |>
  stack_years()

cube$cube[[1]]
```

| Funktion | Zweck |
|---|---|
| `get_geo_admin_assets()` | Assets einer STAC-Collection abfragen |
| `resolve_assets()` | Dateihierarchie auf ein Asset pro Jahr auflösen |
| `read_asset_stars()`, `read_collection_rasters()` | einzelne Assets bzw. eine Collection einlesen |
| `read_geo_admin()` | mehrere Collections, ein Tibble mit `stars`-Listenspalte |
| `read_statpop_ha()`, `subtract_noloc()` | STATPOP-Hektarraster inkl. Sammelpixel-Korrektur |
| `align_to_reference()`, `align_to_grid()` | auf ein gemeinsames Gitter normalisieren |
| `stack_years()`, `tibble_to_cube()` | Würfel bilden und aus einer Tabelle zurückbauen |
| `table_to_stars()`, `make_reference_grid()` | Tabellen rastern, Referenzgitter erzeugen |
| `collection_spec()`, `geo_admin_specs()` | Collections beschreiben und ergänzen |

### Weitere Datenquellen

`read_opendataswiss()`, `read_local_csv()`, `read_geolion_wfs()`, `write_local_csv()`

### Auswertung und Darstellung

`aggregate_groups()`, `immissionscale()`, `scale_fill_capped()`, `pal_emissions()`,
`theme_custom()`, `theme_legend_inside()`, `longpollutant()`, `longmetric()`, `round_off()`

## Vignetten

```r
vignette("geodata")  # Collections -> Würfel -> Auswertung
vignette("statpop")  # Hektarraster und Sammelpixel
vignette("scales")   # gekappte Farbskalen
```

## Migration von Version 0.3.x

Die bisherigen Raster-Leser bleiben als Deprecated-Wrapper erhalten und geben weiterhin die alten
Strukturen zurück:

| bisher | neu |
|---|---|
| `read_bafu_raster_data()` | `read_geo_admin()`, `read_collection_rasters()` |
| `read_statpop_raster_data()` | `read_statpop_ha()` |
| `get_geo_admin_metadata()`, `get_bfs_statpop_metadata()` | `get_geo_admin_assets()` |
| `download_file()`, `download_zip()` | `download_geo_admin_asset()` |
| `rOstluft.plot::scale_fill_*_squished()` | `scale_fill_capped()` |

Zwei Verhaltensänderungen: `read_statpop_raster_data()` zieht neu die Sammelpixel ab (die
Einwohnersummen sind entsprechend tiefer), und Downloads werden unter `geo_admin_cache_dir()`
zwischengespeichert statt in ein übergebenes Verzeichnis.

Die folgenden Funktionen sind nach `awelZH/airquality` gezogen, weil sie analysespezifisch sind:
`get_years()`, `filter_ressources()`, `extract_threshold()`, `extract_year()`, `bin_fun()`,
`extract_pollutant()`, `rf_meteo_normalisation()`, `derive_trends_per_parameter()`,
`load_packages()`.
