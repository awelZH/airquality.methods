# Aim and context of project

`airquality.methods` is the shared **function library** for systematic air quality analyses of the
Canton of Zurich (AWEL). It provides building blocks — readers for public data sources, spatial
raster handling, statistical helpers, plotting scales — that are consumed by analysis repositories,
above all [`awelZH/airquality`](https://github.com/awelZH/airquality) (data compilation, evaluation,
reporting), and potentially by `ndep.ostluft` and `ufp25`.

Design goal: **modular, flexible functions plus a small number of clear wrappers for recurring
tasks**, in a tidy file structure, properly documented.

## What this repo is

An R package (version 0.4.0, GPL >= 3, renv-managed, R >= 4.2). It is *not* an analysis repo: no
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
| `read-tabular.R`, `read-vector.R` | opendata.swiss, local CSV, geolion WFS |
| `recode.R` | pollutant and metric labels |
| `aggregate.R` | `aggregate_groups()` |
| `scale-capped*.R` | capped ggplot2 colour scales (moved from `ufp25`) |
| `scales.R`, `theme.R` | pollutant scales and figure themes |
| `utils.R` | `check_names()`, `round_off()`, `write_local_csv()` |
| `deprecated.R` | wrappers keeping `airquality` running during migration |

Vignettes: `geodata` (the whole chain), `statpop` (hectare grid and collector pixels), `scales`
(capped colour scales).

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

## Scope decisions (deliberate, not technical limits)

* **No analysis logic, no reports, no data.** Exposure distributions, health outcomes, emission
  cadastre preparation and all plotting of concrete figures live in `airquality`.
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
  network. Coverage is 93.6 %.
* **Packages:** `pak` for installation, `renv` for the environment.

## Where to look next

* `vignette("geodata")` — the whole chain, collections to cube to evaluation.
* `R/raster-align.R` — the part that decides what the numbers mean: resampling method, temporal
  matching, the `sources` log.
* `R/geo-admin-statpop.R` — collector pixel correction and its accounting.
* `../airquality/scripts/_compile_exposition_data.R` — the consumer that defines what the geodata
  stack has to deliver.

## Open items in neighbouring repos

* **`airquality`:** 11 calls in `scripts/` still carry the `airquality.methods::` prefix for
  functions that now live locally in `airquality/R/helpers.R` — `_compile_emission_data.R` (5×),
  `_plot_airquality.R` (3×), `_compile_exposition_data.R` (2×, incl. `get_years`),
  `_compile_outcomes.R`, `_compile_trend_data.R`, `_derive_o3_peak-season_rasterdata.R`,
  `_derive_pm25_rasterdata.R`. Only the prefix needs removing; those scripts are broken until then.
* **`ufp25`:** `scale_capped` now exists in both packages. `ufp25` should import it from
  `airquality.methods` (`R/polar_raster.R`, `R/polar_raster_plot.R` use it) and drop its own copy.
* `to-do.md` flags further `ufp25` functions worth integrating.
