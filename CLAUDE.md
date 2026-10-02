# Aim and context of project

`airquality.methods` is the shared **function library** for systematic air quality analyses of the
Canton of Zurich (AWEL). It provides building blocks — readers for public data sources, spatial
raster handling, statistical helpers, plotting scales — that are consumed by analysis repositories,
above all [`awelZH/airquality`](https://github.com/awelZH/airquality) (data compilation, evaluation,
reporting), and potentially by `ndep.ostluft` and `ufp25`.

Design goal: **modular, flexible functions plus a small number of clear wrappers for recurring
tasks**, in a tidy file structure, properly documented.

## What this repo is

An R package (version 0.10.0, GPL >= 3, renv-managed, R >= 4.2). It is *not* an analysis repo: no
report logic, no hard-coded project paths, no dataset-specific pipelines. Everything that only makes
sense inside one specific analysis belongs to `airquality`.

Layout of `R/`:

| File(s) | Content |
|---|---|
| `geo-admin-stac.R` | STAC client: request, pagination, parsing, asset resolution |
| `geo-admin-cache.R` | download, checksum validation, zip extraction, tabular reading |
| `geo-admin-raster.R` | bounding boxes, `table_to_stars()`, `read_asset_stars()` |
| `geo-admin-statpop.R` | STATPOP hectare grid, collector pixel correction |
| `geo-admin-collections.R` | `collection_spec()`, `geo_admin_specs()`, `read_geo_admin()` |
| `raster-cube.R` | `stack_years()`, `tibble_to_cube()`, grid descriptions |
| `raster-align.R` | GDAL resampling, temporal matching, `align_to_reference/grid()` |
| `read-tabular.R`, `read-vector.R` | opendata.swiss (`get_opendataswiss_resources()`: resources with `modified`, `byte_size`, `id` and `name`; `select_opendataswiss_resource()`: exactly one resource by name; `download_opendataswiss_resource()`: cached, fetched again only for a new version), local CSV, geolion WFS |
| `recode.R` | pollutant and metric labels; `markdown_text()` (subscripts and superscripts for ggtext) |
| `aggregate.R` | `aggregate_groups()` |
| `scale-capped*.R` | capped ggplot2 colour scales (moved from `ufp25`) |
| `scales.R`, `theme.R` | pollutant scales and figure themes |
| `legend-grouped.R` | `grouped_key()`, `add_grouped_legend()`: one legend block per group (`legendry`, Suggests) |
| `stat-distribution.R` | `stat_distribution()`, `scale_distribution()`: percentile bands and centre lines of a distribution, their legend built by ggplot2 (replaced `band_key()`, 0.9.0) |
| `fig-meta.R` | `fig_meta()`: title, caption, note and alt text carried on the plot object, never drawn; `fig_title/caption/note/alt()`, `fig_index()` (moved from `ufp25`) |
| `polar-raster*.R` | polar plots by wind speed and direction on a Cartesian panel: `polar_bin()`, `polar_sector()` (binning, no smoothing), `polar_raster()`, `polar_plot()` (`openair::polarPlot()` smoothing, Suggests), `polar_data()`, `polar_grid()`, `theme_polar()`, `polar_statfun()` (moved from `ufp25`) |
| `polar-key.R` | `polar_key()`: the key rose of a facetted figure, as a plot of its own (moved from `ufp25`) |
| `polar-map*.R` | `polar_map()`: roses at their true LV95 coordinates, one shared ruler; `polar_map_decor()`, `theme_polar_map()` (moved from `ufp25`) |
| `basemap-swisstopo.R` | `bbox_lv95()`, `basemap_swisstopo()` (one georeferenced PNG from the swisstopo WMS, cached, pinnable with world file; `png`, Suggests), `annotation_basemap()` (moved from `ufp25`) |
| `annotation-scalebar.R` | `annotation_scalebar()`: the scale bar of any LV95 ggplot, also drawn by `polar_map()` (moved from `ufp25`) |
| `zzz.R` | `.onLoad()`: registers the theme element `polar.grid` |
| `municipalities.R` | geolion municipality map: `drop_foreign_enclaves()`, `assign_municipalities()`; STATPOP collector pixels back to their municipality: `noloc_from_aligned()`, `redistribute_noloc()` |
| `ndep.R` | classes of the nitrogen deposition analysis (Ostluft conventions): `recode_ecosystems()`, `classify_ostluft_siteclass()`, `classify_nh3_emission()`, `classify_estimated()`, `classify_frac_estimated()`, `derive_source_category()` |
| `plot-catalog.R` | plots of a Quarto report in one table, keyed by `parameter`, `year` or keys of the caller's own: `plot_catalog()`, `catalog_entries()`, `get_plot()` (error class `plot_catalog_error`); `print_tabset()` |
| `utils.R` | `check_names()` (exported, optional error `class`), `round_off()`, `write_local_csv()` (creates directories; appending to a new file writes the header) |
| `deprecated.R` | wrappers keeping `airquality` running during migration |

Vignettes: `geodata` (the whole chain), `statpop` (hectare grid and collector pixels), `scales`
(pollutant scales, themes, capped scales in one figure), `capped-scales` (every option of the capped
scale), `polar-plots` (polar plots from openair's `mydata`: smoothed, binned, sectors, grid, axis,
key rose), `polar-maps` (roses on LV95 coordinates, swisstopo basemap, pinning, key rose, scale
bar, attribution). The last three are the example scripts of `ufp25` (2026-10-01), kept about as
broad as they were — the examples live where the functions are looked up. Their openair parts run
only with openair installed, their basemap parts only online with `png`; basemap figures are
rendered as JPEG, which keeps `polar-maps` at ~1.4 MB instead of ~4.7 MB.

`R CMD build` installs the package to build the vignettes, and the `.Rprofile` would activate renv
in that temporary copy and miss every dependency: build with `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE`.

History that explains the state before 0.4.0: commit `98682a9` moved ~60 analysis-specific functions
to the `airquality` repo; `man/` was never cleaned up, `NAMESPACE` exported everything via
`exportPattern()`, and the reworked geodata code sat unintegrated in `R/new/`. 0.4.0 closes all of
that.

# Project Decision Log

## The one recurring lesson

**Generic here, specific there.** Every time a function encoded knowledge about one particular
analysis — a file path under `inst/extdata/`, a fixed list of pollutants, the sequence of steps of
one report — it had to be moved or rewritten later. A function earns its place in this package only
if a second analysis could call it without editing it.

By 0.4.0 the split is done: `get_years()`, `filter_ressources()`, `extract_threshold()`,
`extract_year()`, `bin_fun()`, `extract_pollutant()`, `rf_meteo_normalisation()`,
`derive_trends_per_parameter()` and `load_packages()` now live in `airquality/R/helpers.R`.

## Architecture decisions and why

**1. The unit of work is a tibble with a list column of `stars` objects — not a nested list.**
The old readers returned `list(year -> list(pollutant -> stars))` and every consumer had to walk it
with `lapply()`/`purrr::map2()`. The rewrite returns one row per collection × year with the raster
in a `stars` list column, so selection, filtering and joining are plain dplyr. Provenance
(`item`, `href`, `format`, `grid`, `res_x`/`res_y`) travels in the same table as the data.

**2. Read data.geo.admin.ch through the STAC API (v1), not through hard-coded URLs.**
The v0.9 endpoint used by the old `get_geo_admin_metadata()` is deprecated, and BFS/STATPOP moved
from a zipped-CSV download (`dam-api.bfs.admin.ch`) to a STAC collection with parquet assets. The
client paginates with `httr2::req_perform_iterative()`, validates checksums, and caches downloads
under `tools::R_user_dir()` so re-runs are offline and reproducible.

**3. Rasterise hectare tables by cell index, not by `st_rasterize()` on point geometries.**
`table_to_stars()` derives the grid origin from the data, verifies that every coordinate lies on the
grid, and writes values into the array by index. This is exact (no snapping surprises), fast, and it
*fails loudly* when an assumption about `cellsize`/`anchor` is wrong instead of producing a silently
shifted raster.

**4. Resampling is explicit per quantity type, and mass conservation is logged.**
Concentrations and rates use GDAL `average`, counts (inhabitants) use `sum`, declared once per
collection in `geo_admin_specs()`. `align_to_*()` writes a `sources` table per year recording source
year, weight, source resolution, method and the totals before/after resampling, so a population sum
that changes during alignment is visible rather than hidden.

**5. Temporal alignment is a deliberate choice, default `"exact"`.**
Nitrogen deposition exists for 1990/2000/2005/2010/2015/2020, pollutants and STATPOP are annual.
`year_match = "exact" | "nearest" | "linear"` (per collection, bounded by `max_year_diff`) makes the
interpolation visible in the `sources` log instead of being an undocumented side effect. It never
extrapolates.

**6. STATPOP collector pixels are corrected and accounted for, never silently dropped.**
Each municipality has one cell containing the inhabitants that cannot be located. `subtract_noloc()`
removes them per cell and returns a per-pixel audit (`subtracted`, `exceeds_cell`, `cell_missing`,
`outside_extent`), with the invariant `sum(noloc file) = noloc_subtracted + noloc_unmatched`.

**7. Round trip cube ⇄ tibble.** `as_tibble(cube)` gives the full dplyr vocabulary;
`tibble_to_cube()` puts the result back on the original grid, preserving CRS, grid and
years-as-points. That keeps analysis code in tidyverse idiom without giving up the spatial object.

**8. `geo_admin_specs()` is a function, not a stored list.** A top-level list capturing reader
functions is resolved at *build* time, and R's alphabetical collation put the spec file before the
file defining `read_statpop_ha()` — the package would not build. A function resolves its readers when
called, and users can extend it with `c(geo_admin_specs(), list(...))`.

**9. No GitHub-only dependencies.** `rOstluft.plot` was an undeclared dependency of the old
`plot.R`. Its two "squished" scales are replaced by `scale_capped()`, moved over from `ufp25`: values
outside the limits are squished onto the edge colour and the legend marks them with "≤"/"≥" and
whiskers at the true extremes, so a fixed-limit map shows no holes and hides nothing.

**10. The network boundary is one function.** Downloads go through `fetch_to_file()`, which exists
so the cache logic (checksums, `.part` files, reuse) can be tested without a network.

**11. A GeoTIFF without EPSG code is accepted only when two sources agree** (0.5.2, 2026-09-25).
The BAFU PM10 maps 1998–2001 (4 of 101 pollutant maps 1995–2025) are in LV95, but their WKT carries no
EPSG code and no datum shift (`+towgs84`), so `sf::st_crs(x) == sf::st_crs(2056)` is `FALSE` and
`read_tif_stars()` refused them. `resolve_tif_crs()` now reads such a file as the expected crs, with a
message, if the STAC metadata declare it (`proj:epsg`) **and** `same_projection()` finds the same
projection parameters (proj strings without `+towgs84`/`+no_defs`). A file without EPSG code in another
projection, or without `proj:epsg` in the metadata, still stops. Nothing is reprojected, so the missing
datum shift does not matter.

**12. Legend titles of the pollutant scales are markdown, rendered by ggtext** (0.5.3, 2026-09-25).
Plot titles use `openair::quickText()` (plotmath) for subscripts, but plotmath cannot break lines, and the
legend titles of `immissionscale()` have two or three lines ("NO2\n(µg/m3)"). `markdown_text()` writes
pollutants with `<sub>`, m2/m3 with `<sup>` and line breaks as `<br>` (angle brackets in the text
escaped); `capped_markdown()` gives the capped scale a guide whose own theme sets
`legend.title = ggtext::element_markdown()`, so it works whatever the theme of the plot and consumers
change nothing. `ggtext` is a new import.

**13. Report texts travel with the plot, not in it** (0.6.0, 2026-09-30, moved from `ufp25`).
`fig_meta()` stores title, caption and methodological note in the plot's `fig_meta` attribute
(it was `ufp_fig` in `ufp25`), which survives `+` and `ggsave()`, and writes one generated alt text
into ggplot2's own `alt` label, so knitr and screen readers read the same text the report quotes.
Nothing is drawn: a title in the panel repeats the Quarto caption and has to be re-flowed on every
resize. It refuses a non-plot, because `ggplot() + theme() |> fig_meta()` hands over the theme
(`|>` binds tighter than `+`). Together with the plot catalog: the catalog carries the plot to the
page, `fig_caption()`/`fig_alt()` give its texts there.

**14. A distribution's legend comes from its stat, not from a key plot** (0.9.0, 2026-10-02,
replaces `band_key()` of 0.6.0). A distribution panel draws median, mean and percentile bands. Up
to 0.8.0 these were four layers on four columns, and `band_key()` drew a made-up hump beside the
figure (patchwork) with the same four elements mapped *there*, so that ggplot2 would lay out a
legend -- which meant the alphas and line types stood twice, in the figure and in the key, and had
to be kept equal by hand. `stat_distribution()` computes the statistics itself and returns one row
per statistic, labelled in the computed variable `statistic`; the constructor maps that onto an
aesthetic of each part's geom (`alpha` for ribbons, `linewidth` for line ranges, `linetype` for
lines, `shape` for points), so the drawn figure and its legend come from one scale
(`scale_distribution()`). Decided along the way:

* **One call, two layers** (bands, centres), returned as a list for `+`: a layer has one geom, and
  the parts want different ones. `band_params`/`centre_params` reach one part only.
* **Two kinds of input, one internal format**: raw `y`, summarised by `quantile(type = 7)`,
  `median()`, `mean()` per `x`; or statistics computed beforehand (data too large to hand to a
  plot), mapped through ggplot2's boxplot aesthetics `ymin`/`lower`/`middle`/`upper`/`ymax` and `y`
  for the mean. The raw path computes exactly that wide row, so both paths draw the same (tested).
  At most two bands -- the boxplot vocabulary; more would be `ggdist`'s job.
* **Groups are split per statistic only for connecting geoms** (ribbon, path, polygon), where two
  bands of one group would otherwise become one polygon. Points and ranges keep the caller's groups,
  so dodging moves a group's bands and centres together.
* **Band labels are written from `probs`** (P10-P90 with an en dash), so a label cannot name
  another percentile than the one drawn; centre labels default to German, as `band_key()`'s did.
* **Two aesthetics on one part merge into one legend block only if their guides share `order`**
  (ggplot2 groups legends by order *and* hash). `linewidth` can tell the bands apart (box-like) or
  the centres (line width beside line type), so `scale_distribution(linewidth_part = )` says which;
  among the centres it carries no `override.aes`, because merging two of them warns at draw time.
  Found because `grob_labels()` is `unique()`: two blocks both reading "Median" passed a label
  test -- the test now counts the legends in the guide box.
* Without `scale_distribution()` ggplot2's default alpha scale warns about a discrete variable;
  the stat does not add scales itself, so a caller's own scale replaces nothing silently.
* `ggdist::stat_lineribbon()` was the model and was not taken: a heavy dependency, one centre
  only (not median *and* mean), no path for summarised input.

**15. Polar plots and maps are ggplot2 extensions with a few hard invariants** (0.7.0, 2026-09-30,
moved from `ufp25`, where the full reasoning is `docs/decisions/02_package_architecture.md`,
decisions 1–17). Each of them cost a debugging round there:

* Polar plots sit on a **Cartesian** panel with `coord_fixed()`, never `coord_polar()`: the cells
  are already in u/v, and `coord_polar()` would warp them and break `geom_raster()`.
* The polar grid is the registered theme element **`polar.grid`** (`zzz.R`), drawn by our own
  layers and resolved field by field in `.polar_grid_style()`. `panel.grid` would draw straight
  lines through the rose.
* **Binning, not smoothing**: `polar_bin()` (squares) and `polar_sector()` (ring segments); empty
  cells stay empty. Smoothing only through `polar_plot()` → openair, which keeps the grid in the
  `polar_data` attribute so recolouring does not recompute.
* **Attributes carry intent** (`polar_facets`, `polar_geom`, `polar_interpolate`); an empty
  `polar_facets` means "nothing recorded". dplyr verbs may drop attributes.
* **`geom_raster()` only for grids `.polar_grid_kind()` calls `regular`**; otherwise
  `geom_tile()` with a note. ggplot2 raises "uneven intervals" at draw time and then silently
  shifts cells — on a map, to wrong coordinates.
* **One shared ruler** on a map: `k = radius / r_max` is one number for all roses and the key
  rose, so a ring is the same wind speed everywhere. Default radius = the largest without overlap,
  rounded down and reported. Clipping (`r_max`) is **per cell**, never per row: sector data are one
  row per *vertex*.
* One function for each shared quantity: radial labels `.polar_axis_labels()`, scale bar
  `annotation_scalebar()` / `.scalebar_length()`, key rose `polar_key()` sharing `.polar_rings()`
  / `.polar_grid_layers()`.
* **Binning rounds half away from zero** with `round_off()`. Wind data come in fixed steps, so a
  value on a cell boundary is half the dataset, and `round()`'s half-to-even gives one cell of
  each pair three times the observations of the other. `ufp25` used its own
  `sign(x) * floor(abs(x) + 0.5)`; `round_off()` adds `sqrt(.Machine$double.eps)` against
  representation error and puts every figure of `ufp25` into the same cells.
* **The basemap is a PNG plus a world file, not a spatial object**: one WMS GetMap request,
  cached under `tools::R_user_dir("airquality.methods", "cache")`, pinnable with `file =`
  (`.pgw`, `.prj`, `.layer` sidecars), and a pinned file wins over `layer`/`px`/`res` — that is
  what makes a Quarto render reproducible offline. The response is checked for the PNG magic
  bytes because OGC errors arrive with HTTP 200. Classes: `swisstopo_basemap` and `lv95_bbox`
  (were `ufp_basemap`, `ufp_bbox`).
* **Attribution follows the basemap**: "Kartengrundlage: © swisstopo" rides on the basemap object
  and appears as caption whenever, and only when, one is drawn.
* `polar_statfun()` is exported so that an analysis condensing values per direction (`ufp25`'s
  `wd_extreme()`) means by `"median"` what the binning means.

**Moved code keeps its base `lapply`/`vapply`.** Code taken over from `ufp25` is not restyled to
purrr and `\(x)`: a restyle is a behaviour risk without a gain, and `ufp25` checks every one of its
figures for identity across a move. New code here follows the conventions below.

**16. The keys of a plot catalog are the caller's** (0.8.0, 2026-10-02, asked for by `ufp25`).
`plot_catalog(names_to = )` took only `"parameter"` and `"year"`, the two dimensions of the Ostluft
annual reports in `airquality`. A campaign report keys its figures by other things (site, time of
day, wind regime) or by nothing but the figure's name. Now `names_to` names any key: an own key
becomes a character column between `year` and `figure`, `get_plot()`/`catalog_entries()` filter on
it by name through `...` (`get_plot(cat, "rose", site = "A1")`), and `names_to = "plot"` takes the
plot names from the list (`plot_catalog(FIGURES, names_to = "plot")` for a script's flat list of
figures). `parameter` and `year` stay as columns of every catalog and as positional arguments, so
`airquality`'s calls (`get_plot(plots, "map", "NO2", 2021)`, `entries$year`) work unchanged. A key
in `...` that is no column, or is unnamed, is an error rather than a filter that matches nothing,
and the "which plots exist" error lists the combinations over the keys the plot actually has.
`dplyr::bind_rows()` of catalogs with different own keys puts the later keys after `figure`;
column order carries no meaning.

**17. An opendata.swiss resource is chosen by its name and fetched by its version** (0.10.0,
2026-10-02, asked for by `ufp25`, whose campaign data moved to opendata.swiss). The download urls of
the cantonal datasets are opaque (`KTZH_00003175_00006879.parquet`), so `get_opendataswiss_resources()`
now also returns the CKAN `id` and the resource `name` (German first, then en/fr/it).
`select_opendataswiss_resource()` takes a regular expression on the name and refuses both no match
and several matches, listing every resource: a renamed or an added resource stops a pipeline instead
of reading the wrong file. `download_opendataswiss_resource()` keeps the file under its own name with
a `.version` file beside it (`modified` and `byte_size` as the API states them -- opendata.swiss
publishes no checksum) and downloads again only when the API reports another version; the download
goes to a `.part` file, a stated size is checked, and only a complete file replaces the cached one.
The new columns are appended, so existing calls keep working.

## Scope decisions (deliberate, not technical limits)

* **No analysis logic, no reports, no data.** Exposure distributions, health outcomes, emission
  cadastre preparation and all plotting of concrete figures live in `airquality`. Building blocks
  that any analysis of the canton's grid data needs (municipality assignment, collector pixel
  redistribution, grouped legend) are not analysis logic and live here (user decision 2026-09-21).
  The same holds for the plot catalog and the tabset of Quarto reports and for the Ostluft classes
  of the nitrogen deposition, which `ndep.ostluft` uses as well (user decision 2026-09-25). The year
  slider of `airquality` stays there: it needs its own HTML/JS asset and German tab titles.
* **Raster only, for now.** The geodata stack reads GeoTIFF/COG and hectare tables (parquet, csv).
  Vector data stays with `sf::read_sf()` and the geolion WFS reader.
* **Cantonal default, national capability.** `bbox_zh_lv95` is the default extent; every function
  takes any `sf`/`bbox`/`stars` object, so Switzerland-wide use only costs memory.
* **Backwards compatibility is time-limited.** Superseded readers are thin deprecated wrappers so
  `airquality` keeps running; they are not maintained beyond that.

## Migration: old to new

| Removed / deprecated | Use instead |
|---|---|
| `read_bafu_raster_data()` | `read_geo_admin()`, `read_collection_rasters()` |
| `read_statpop_raster_data()` | `read_statpop_ha()` |
| `get_geo_admin_metadata()`, `get_assets()`, `check_for_more()` | `get_geo_admin_assets()` |
| `get_bfs_metadata()`, `get_bfs_statpop_metadata()` | `get_geo_admin_assets()` |
| `download_file()`, `download_zip()`, `download_statpop_data()` | `download_geo_admin_asset()` |
| `read_statpop_csv()` | `read_asset_table()` + `table_to_stars()` |
| `average_to_grid()`, `average_to_statpop()` (in `airquality`) | `align_to_reference()`, `align_to_grid()` |
| `combine_raster_aq()`, `bafu_rasterlist_to_tibble()` (in `airquality`) | `stack_years()`, `as_tibble(cube)` |
| `rOstluft.plot::scale_fill_*_squished()` | `scale_fill_capped()` |
| `grouped_key()`, `add_grouped_legend()` (in `airquality`, 2026-09-21) | same names, unchanged |
| `drop_foreign_enclaves()`, `assign_municipalities()`, `noloc_from_aligned()`, `redistribute_noloc()` (in `airquality`, 2026-09-21) | same names, unchanged; the collector pixel correction (subtract in `read_statpop_ha()`, give back in `redistribute_noloc()`) now lives in one package |
| `check_columns()` (in `airquality`, 2026-09-21) | `check_names(names(data), required, what, class = )`; `airquality` keeps a one-line wrapper for its error class |
| `append_log()` (in `airquality`, 2026-09-21) | `write_local_csv(append = TRUE)` |
| `recode_ecosystems()`, `classify_*()` (site, NH3 emission, estimated part), `derive_source_category()` (in `airquality`, 0.5.0, 2026-09-25) | same names, unchanged; replace the copies in `ndep.ostluft` (`recode_ecosys()`, `ostluft_siteclass()`, `cut_*()`, `derive_source_cat()`) |
| `plot_catalog()`, `catalog_entries()`, `get_plot()`, `print_tabset()` (in `airquality`, 0.5.0, 2026-09-25) | same names; errors now of class `plot_catalog_error` (was `airquality_plot_error`), `print_tabset(level = 5)` |
| `scale_capped()` and variants (in `ufp25`; its copy deleted 2026-09-30) | same names, unchanged |
| `fig_meta()`, `fig_title/caption/note/alt()`, `fig_index()` (in `ufp25`, 0.6.0, 2026-09-30) | same names; the attribute is `fig_meta` (was `ufp_fig`) |
| `band_key()` (0.6.0; removed in 0.9.0) | `stat_distribution()` + `scale_distribution()`: the legend is the figure's own, no key plot beside it |
| `polar_*()`, `theme_polar()`, `theme_polar_map()`, `bbox_lv95()`, `basemap_swisstopo()`, `annotation_basemap()`, `annotation_scalebar()` (in `ufp25`, 0.7.0, 2026-09-30) | same names; classes `swisstopo_basemap`/`lv95_bbox` (were `ufp_basemap`/`ufp_bbox`); basemap cache under `R_user_dir("airquality.methods")` (pinned files unaffected); `polar_statfun()` newly exported (was `ufp25:::.polar_statfun()`) |

Deprecated wrappers still return the **old** shapes, so `airquality` runs unchanged and emits
deprecation warnings. Two behavioural differences to know about:

* `read_statpop_raster_data()` now subtracts the collector pixels, so population totals are lower
  than before by the inhabitants that cannot be located.
* `read_bafu_raster_data()` streams COGs instead of downloading whole files, and caches them.

## Regression against the published series (measured 2026-09-18)

Reproduced `airquality/inst/extdata/output/data_exposition_weighted_means_canton.csv` for 2010–2024
with the new pipeline, same canton boundary (geolion WFS, `st_union |> st_boundary |> st_cast`),
`align_to_reference()` onto the STATPOP grid, run once with and once without the collector pixel
correction.

**Population-weighted concentrations agree to better than 0.15 %** — the analysis conclusions do not
change:

| Parameter | median deviation | range |
|---|---|---|
| NO2 | +0.08 % | +0.07 … +0.12 % |
| PM10 | +0.02 % | +0.01 … +0.04 % |
| PM2.5 (2015–) | +0.03 % | +0.02 … +0.03 % |
| O3 typ. Spitzenbelastung | −0.006 % | −0.009 … +0.002 % |

Not reproducible here, because the analysis repo derives them rather than reading them: O3
"mittlere Sommertagbelastung" (statistical relationship) and PM2.5 before 2015 (from PM10 ratios).

**Population is 1.1 % lower**, of which:

* **0.5 pp** is the collector pixel correction — deliberate, and isolated by the paired run.
* **0.6 pp** comes from the STATPOP data itself, not from the pipeline. It cannot be reconciled with
  the published figures, because **the old reader cannot read them any more**: BFS harmonised the
  hectare grid column from `B10BTOT`/`B20BTOT`/… to `BBTOT` across all years. `read_statpop_csv()`
  derives the old name from the year and finds nothing for every year up to 2022 — verified against
  the live API. The published series was computed from a file vintage that no longer exists, so this
  0.6 pp is a comparison across data vintages, not across pipelines.

That last point also settles whether the rewrite was optional: the old STATPOP path is dead against
today's API for every year before 2023.

**Correction from the `airquality` regression (same day, `airquality/tests/regression/`):** the
0.6 pp are *not* a data-vintage effect. The old `merge_statpop_with_subareas()` in `airquality`
joined municipality names by `bfs` onto the cells, and the geolion map has several features for some
`bfs` numbers (exclaves of Glattfelden and Mönchaltorf, three `bfs = 0` features: lakes, Kloster
Fahr), so those cells were counted two or three times. Emulating that double counting with
`correct_noloc = FALSE` reproduces the published population exactly for 2020–2024 (+0.04 % for
2010–2019, explained by border cells). The collector pixel correction accounts for ≈ 0.43 pp. The
bug is fixed in `airquality`.

## Behavioural differences, ranked by effect

1. **Collector pixels are removed** from the population raster (`subtract_noloc()`). The old pipeline
   kept them. This is the one change that will certainly move the numbers: in a 2023 test extent it
   was 2875 of 734'924 inhabitants, about 0.4 %. It shifts both the population total and the
   population-weighted mean, because those inhabitants used to carry the concentration of whichever
   cell they had been parked in.
2. **Target cells beyond the source extent become `NA`** (`source_overlap_mask()`). GDAL's `average`
   writes one cell row past the edge of the source when refining; the old code kept that row. Affects
   a one-cell ring at the boundary of each pollutant raster.
3. **Asset selection per year is stricter.** The old `get_geo_admin_metadata()` took every `.tif` and
   keyed it by the year parsed from the url, silently keeping the first match when a year had several
   variants. `resolve_assets()` prefers the asset naming EPSG 2056 and aborts on genuine ambiguity.
   Relevant where a collection changed resolution, as NO2 did from 2020.
4. **More years are available.** The old `get_years()` hard-coded PM2.5 from 2015 and the rest from
   2010. The new readers offer what the API actually holds: NO2 and O3 from 1990, PM10 from 1998,
   PM2.5 from 2015.
5. `no_data_value` moved from -999 to -9999 and is now checked against the data. No effect expected
   for non-negative concentrations.

What is **not** a difference, contrary to an earlier assumption: rasterising the hectare table. The
old `st_rasterize()` path was verified to produce 100 m cells on the correct origin with the sum
conserved. `table_to_stars()` is stricter (it verifies the grid and snaps to a fixed bbox, so all
years share one grid) but it does not move values where both produce data.

Also found by the regression, and fixed: `sf` and `stars` were only ever called as `stars::fun()`,
so their namespaces stayed unloaded and the S3 methods they register for each other's generics
(`st_normalize.stars`, `st_crop.stars`) were missing. Any `stars` object arriving from `readRDS()`,
or from a caller that had not itself touched `stars`, broke with "no applicable method". The package
now carries real `@importFrom` directives for both, with a test asserting they stay. No unit test
caught this because the suite always calls something `stars::` first.

## Conventions

* **Language:** code, comments, roxygen and user-facing `cli` messages in English. Non-ASCII
  characters in code (not comments) must be `\uXXXX` escapes — `R CMD check` requires it.
* **Style:** tidyverse style guide, native pipe `|>`, `.by =` instead of `group_by()/ungroup()`,
  `join_by()`, `purrr::map_*()` instead of `sapply()`, lambda shorthand `\(x)`,
  `dplyr::recode_values()` (not the deprecated `case_match()`).
* **Errors:** `cli::cli_abort()`/`cli_warn()` with a cause line and an actionable hint, validated at
  the function boundary.
* **Tests:** testthat 3e, tests first. The network is stubbed via `local_mocked_bindings()` on
  `fetch_to_file()`/`read_asset_stars()` or `httr2::with_mocked_responses()`; no test touches the
  network. The basemap's boundary is `utils::download.file()`: `local_fake_wms()` in
  `test-basemap-swisstopo.R` answers with a PNG of the requested size, records every request (so
  "served from the cache" is asserted as "no second request"), and moves the disk cache to a
  temporary directory via `R_USER_CACHE_DIR`. (The two tests taken over from `ufp25` called the
  live WMS when online; replaced 2026-10-01.) Coverage 93.4 % (2026-10-01).
* **Packages:** `pak` for installation, `renv` for the environment.

## Where to look next

* `vignette("geodata")` — the whole chain, collections to cube to evaluation.
* `R/raster-align.R` — the part that decides what the numbers mean: resampling method, temporal
  matching, the `sources` log.
* `R/geo-admin-statpop.R` — collector pixel correction and its accounting.
* `../airquality/scripts/_compile_exposition_data.R` — the consumer that defines what the geodata
  stack has to deliver.

## Open items in neighbouring repos

* **`airquality`:** done 2026-09-18 – obsolete prefixes removed, exposition switched to
  `read_geo_admin()`/`align_to_reference()`, no deprecated wrapper is called any more. It uses 0.4.0
  from a local renv install until 0.4.0 is pushed.
* **`ufp25`:** imports this package (pinned by commit in its `renv.lock`). Moved over on
  2026-09-30: `scale_capped` (its copy dropped), `fig_meta()` family and `band_key()` (0.6.0; replaced by `stat_distribution()` in 0.9.0), the
  polar plots and maps with basemap and scale bar (0.7.0 — one step, because the map furniture and
  the polar functions share their internal helpers). Deliberately staying in `ufp25`:
  `theme_report()`, `read_ostluft_parquet()`, the source location (`wd_extreme()`,
  `triangulate_rays()`, ...); the size distribution tools until its analysis steps 06–08 are
  settled. The record and the moving recipe: `ufp25/docs/decisions/13_airquality_methods.md`.
* A shell that inherited `ufp25`'s renv variables (`RENV_PROJECT`, `R_LIBS_USER`) runs R in
  *that* project's library even from this directory. Unset them (`env -u RENV_PROJECT
  R_LIBS_USER="$RENV_DEFAULT_R_LIBS_USER" Rscript ...`). This library has no `devtools`:
  `pkgload::load_all()`, `roxygen2::roxygenise()`, `testthat::test_dir()`.
