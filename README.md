# Analysis code

Code for *Cross or bounce? GPS and video data reveal high road encounter rate and altered movement in a common ungulate* (Stemle et al., Movement Ecology).

We applied Barrier Behavior Analysis (BaBA; Xu et al. 2021) to GPS collar data from 125 white-tailed deer at three sites in southern Illinois, 2023–2025, to classify how deer behave when they encounter roads, and paired it with camera collar video from a subset of animals.

## Read this first: what this code does and does not regenerate

This is a **hybrid pipeline**, and knowing that up front will save you confusion.

The original analysis was carried out by the first author. The scripts here are a revised version written for the peer review revision. Some steps regenerate their results from the raw GPS data. Others read derived files produced during the original analysis, because those steps could not be reproduced exactly and rebuilding them would have changed results that the manuscript already reports.

**Regenerated from raw data by this code:**

- GPS cleaning (`01`)
- BaBA encounter classification (`02`)
- Everything downstream of the encounter classification: summaries, encounter rates, the impermeability index, the models, and the sensitivity analyses (`04`–`10`)

**Read in as derived inputs, not rebuilt here:**

- The 125-animal analysis set and per-encounter covariates — road category (OSM-based), road density in each deer's home range, land cover proportions within 100 m of each encounter, and their scaled/centered transforms — all in `7_Dryad_Upload/data/road_encounter_covariates.csv`. Script `01` restricts to this animal set rather than deriving it; scripts `06`, `07`, and `10` fit models directly on this file. The land cover extraction was done in ArcGIS Pro over the full dataset; `landcover_extraction_REFERENCE.R` documents the method on one site but is not part of the numbered pipeline and will not run without editing its paths.
- This file is a merge of two files from the original analysis (OSM road-category assignments and a scaled/centered NLCD extraction), joined on `burstID` at deposit time — previously `06`/`07`/`10` each did this join inline. See the Dryad deposit's README for the merge provenance and column-level verification against the original two-file join.
- Road density (`rdkm_sqkm`, the source of the `std_rdden`/`stdln_rdden` model covariates) — computed once from each deer's 95% kernel density home range and the road network, in a script that was never made self-contained (it depends on interactive session state and home-range shapefiles that were never saved) and is not included here. The value is not independently re-derivable from what's in this repository or the Dryad deposit.

So `01` and `02` will reproduce from raw data, while the modelling scripts depend on a file that ships with the data rather than being built by the chain above them.

## Setup

1. Clone this repository. Paths are resolved with the `here` package relative to the repository root (via its `.git` folder), so scripts work regardless of your working directory — no `.Rproj` file is required, though opening the folder as an RStudio project also works.
2. Download the data deposit from Dryad (https://doi.org/10.5061/dryad.51c59zwr4) and place its files in a `7_Dryad_Upload/data/` folder at the root of this repository, so you have `7_Dryad_Upload/data/USDA_deer_23_Mar25_clean_125animals.csv`, `.../deer_meta.csv`, and `.../road_encounter_covariates.csv` alongside the numbered scripts.
3. Create an empty `1_Data/revisions/` folder at the repository root — this is where every script writes its output.
4. Prepare the road network layer (see "Spatial data setup" below) and place it at `1_Data/leyna_data/spatial/` — needed by scripts `02`, `05`, and `08`.

### Spatial data setup

The road network used in `02`, `05`, and `08` is not included in this repository or the Dryad deposit — it's an extract of the public [USGS National Transportation Dataset](https://www.usgs.gov/programs/national-geospatial-program/national-transportation-dataset-ntd) (The National Map), which we merged and reprojected rather than modified, so we point to the source instead of redistributing a copy.

To reproduce the road layer these scripts expect:

1. Download road/transportation features from the USGS National Transportation Dataset (via [The National Map downloader](https://apps.nationalmap.gov/downloader/)) for the study region: southern Illinois, covering both the Shelbyville, IL area (~39.5°N) and the Touch of Nature / SIU-Carbondale area (~37.6–37.7°N).
2. Reproject to UTM Zone 16N (EPSG:32616).
3. Split (or download separately) into two layers matching the scripts' expected filenames: `Trans_Road_Nroadsutm.shp` (covers Shelbyville/LSV) and `Trans_Road_Sroadsutm.shp` (covers Touch of Nature/TON and SIU), and place both (with their `.dbf`/`.prj`/`.shx`/`.cpg` sidecar files) in `1_Data/leyna_data/spatial/`.

The attribute fields the scripts rely on are the standard NTD schema (`permanent_`, `mtfcc_code`, `tnmfrc`, etc.) — no custom attributes are required.

## Running the code

Run the numbered scripts in order. Each reads from `7_Dryad_Upload/data/` (or the previous script's output in `1_Data/revisions/`) and writes to `1_Data/revisions/`. Figure and table scripts can be run in any order once the numbered scripts have finished.

`01` takes roughly an hour on 1.6 million fixes.

## Pipeline

| Script | What it does | Reads | Writes |
|---|---|---|---|
| `01_clean_gps.R` | Removes duplicates, fixes within 24 h of capture, and implied speeds > 3 km/hr; confirms the 125-animal analysis set and drops fixes with DOP > 10 | `7_Dryad_Upload/data/USDA_deer_23_Mar25_clean_125animals.csv` | `all_23-25_GPS_cleandf.rds` (fix-level only), `all_23-25_GPS_cleandf_v2.rds` (analysis set), per-animal `{animal}_track2.csv` |
| `02_baba_classify_encounters.R` | Runs BaBA: finds road encounters and assigns each a behavioral category | `_v2.rds`, road polylines | `BaBA_all_AEN.Rdata`, per-site BaBA files |
| `04_deer_summaries.R` | Per-deer tracking summary, site totals, and road encounters per individual per day | `01`, `02`, deer metadata | `individual_summary_AEN.csv`, `site_summary_AEN.csv`, `encounter_rates_AEN.csv`, SI data table |
| `05_impermeability_index.R` | Impermeability index per 2 km road segment | `02` | `Rank_{site}_AEN.rds`, `allrank_AEN.csv` |
| `06_logistic_model.R` | Mixed-effects logistic regression of quick cross vs. bounce, with DHARMa and collinearity checks | `road_encounter_covariates.csv` | Model object, diagnostics |
| `07_model_selection.R` | Fits the candidate model set and ranks by AIC | Same as `06` | `model_selection_full_aic.csv` |
| `08_buffer_sensitivity.R` | Re-runs encounter detection over 20–80 m buffers to justify the 50 m choice | `_v2.rds`, roads | `buffer_sensitivity_results.csv` |
| `09_twilight_windows.R` | Seasonal range of twilight durations used to define diel periods | suncalc | Console summary, figure |
| `10_inter_encounter_intervals.R` | Time between successive encounters, used to check encounter independence | `02` | Console summary |

Encounter rates are calculated in `04`, alongside the tracking summaries they depend on.

## Figures (`figures/`)

Files beginning with `_` are shared helpers, not run on their own. `_colors.R` holds the palettes and `_site_panel_base.R` builds the common map frame so every site panel is comparable.

| Script | Produces |
|---|---|
| `fig1_study_site.R` | Fig. 1, land cover, roads, and example tracks |
| `fig2_site_comparison.R` | Fig. 2, road density, encounter rate, encounter types, impermeability |
| `fig3_model_results.R` | Fig. 3, odds ratios and predicted probabilities |
| `fig4_video_behavior.R` | Fig. 4, video behavior by encounter category |
| `figS1_track_overview.R` | Fig. S1, GPS tracks over the road network |
| `figS2_trajectory_examples.R` | Fig. S2, example trajectories per encounter type |
| `figS3_impermeability_map.R` | Fig. S3, impermeability mapped onto road segments |
| `figS4_buffer_sensitivity.R` | Fig. S4, encounter detection across buffer sizes |

The track figures read the per-animal `{animal}_track2.csv` files written by `01`. Those are written before the DOP filter, so plotted tracks include a small number of fixes the analysis excluded.

`fig1_study_site.R` sources `figS1_track_overview.R` to reuse its track selection.

## Tables (`tables/`)

`table_model_selection.R` and `table_model_coefficients.R` format model output as Word tables. Run after `06` and `07`. The individual data summary table comes from `04`.

## Encounter categories

BaBA assigns each encounter one of seven categories, grouped here as:

- **Unaltered (crossing):** quick cross, average movement
- **Altered (non-crossing):** bounce, trace, back-and-forth, trapped
- **Unclassified:** unknown

"Trapped" does not mean physical confinement. It means the deer stayed near the road for a prolonged period (≥ 36 h), whether or not the road was the reason.

The impermeability index in `05` is the fraction of a segment's encounters that fell in the altered group, from 0 (every encounter crossed) to 1 (none did). It is an unweighted proportion and is not rescaled within site, so values are comparable across sites.

## Key BaBA parameters

Buffer 50 m, short interaction 4 h, prolonged interaction 36 h, straightness window 7 days, `max_cross = 2`. Encounters with more than two crossings are reclassified as unknown to absorb GPS error (~10–12 m) and road polyline inaccuracy.

## Data

Data needed to run this pipeline (GPS, metadata, and encounter covariates) are deposited on Dryad rather than distributed with this repository, because of file size: https://doi.org/10.5061/dryad.51c59zwr4. See the Dryad deposit's own README for the file list and column dictionary, and for what was intentionally excluded (road network shapefile, NLCD raster — cite USGS sources instead; video behavioral data; reproducible pipeline outputs).
