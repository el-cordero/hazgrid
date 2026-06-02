# hazgrid

`hazgrid` is a programmable R implementation framework for transparent,
Hazus-style hazard and loss workflows. It is inspired by SPHERE's goal of
making Hazus methods scriptable, reproducible, and fast outside ArcGIS.

Tsunami support is the first target. The current earthquake module is an MVP
that applies user-supplied fragility curves to user-supplied hazard rasters. It
does **not** implement the full Hazus earthquake capacity-spectrum method.

`hazgrid` is not FEMA Hazus. It does not reproduce official Hazus results unless
official or independently validated lookup tables, hazard inputs, inventories,
units, and assumptions are supplied. The included CSV tables contain invented
demo values only. Users are responsible for validating inputs and outputs.

## Design

- `terra` is the required spatial engine for rasters and vectors.
- Hazard extraction, damage probability, economic loss, casualty estimation,
  and summaries are separate stages.
- Base R data frames keep the first implementation simple. Public interfaces
  leave room for optional DuckDB, Arrow, GeoParquet, and data.table backends.
- Unit metadata is checked when available. Missing metadata produces a warning;
  incompatible unit systems produce an error. Use `convert_hazard_units()` for
  explicit conversions.
- No ArcGIS or proprietary Hazus software is required.
- All examples run offline with small synthetic files.

## Install

```r
install.packages("terra")
install.packages(".", repos = NULL, type = "source")
```

## Tsunami Level 2

Level 2 takes inundation depth and velocity rasters. Structural damage uses
momentum flux `H * V^2`; nonstructural and contents damage use depth `H`.

```r
library(hazgrid)
library(terra)

ext <- function(filename) system.file("extdata", filename, package = "hazgrid")

tsunami_l2 <- run_tsunami_loss(
  inventory = ext("synthetic_buildings.gpkg"),
  depth = rast(ext("synthetic_depth.tif")),
  velocity = rast(ext("synthetic_velocity.tif")),
  fragility_table = ext("tsunami_fragility_example.csv"),
  loss_ratio_table = ext("tsunami_loss_ratios_example.csv"),
  group_fields = "municipality"
)

tsunami_l2$summary
```

## Tsunami Level 3

Level 3 accepts inundation depth and momentum flux directly.

```r
depth <- rast(ext("synthetic_depth.tif"))
velocity <- rast(ext("synthetic_velocity.tif"))
flux <- compute_tsunami_momentum_flux(depth, velocity)

tsunami_l3 <- run_tsunami_loss(
  inventory = ext("synthetic_buildings.gpkg"),
  depth = depth,
  momentum_flux = flux,
  fragility_table = ext("tsunami_fragility_example.csv"),
  loss_ratio_table = ext("tsunami_loss_ratios_example.csv")
)
```

## Earthquake MVP

```r
earthquake <- run_earthquake_mvp(
  inventory = ext("synthetic_buildings.gpkg"),
  pga = rast(ext("synthetic_pga.tif")),
  sa03 = rast(ext("synthetic_sa03.tif")),
  sa10 = rast(ext("synthetic_sa10.tif")),
  pgv = rast(ext("synthetic_pgv.tif")),
  fragility_table = ext("earthquake_fragility_example.csv"),
  group_fields = "municipality"
)

earthquake$summary
```

## Roadmap

Future modules can add official lookup-table ingestion, DuckDB/Arrow/GeoParquet
acceleration, a full tsunami casualty model, evacpath integration, the complete
earthquake capacity-spectrum method, ground failure, lifelines, debris, shelter,
and combined earthquake-tsunami damage.
