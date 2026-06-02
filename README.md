# hazgrid

`hazgrid` is an independent R framework for transparent, programmable
Hazus-style hazard and loss workflows outside ArcGIS. It is not FEMA Hazus and
is not affiliated with FEMA.

Real analyses require official FEMA Hazus or independently validated lookup
tables, hazard inputs, inventories, units, and assumptions. `hazgrid` validates
table schema and provenance metadata, but users remain responsible for
confirming that source values and analytical assumptions are appropriate.

The bundled files under `inst/extdata/demo/` are invented synthetic data for
offline tests and examples. They are not valid for real-world loss estimates or
decisions. Demo tables are rejected by default and require the explicit
`allow_demo_tables = TRUE` opt-in.

## Design

- `terra` is the required spatial engine for rasters and vectors.
- Hazard extraction, damage probability, economic loss, casualty estimation,
  assumptions, lookup validation, and summaries are separate stages.
- Base R data frames keep the first implementation simple. Public interfaces
  leave room for optional DuckDB, Arrow, GeoParquet, and data.table backends.
- Unit metadata is checked. Incompatible systems produce errors. Tsunami
  velocity in `cm/s` must be converted explicitly or requested with
  `auto_convert_units = TRUE`.
- No ArcGIS or proprietary Hazus software is required.
- All examples run offline with small synthetic files.

## Validated Lookup Tables

Typed schema templates are installed under `inst/extdata/schemas/`. Fill these
from official FEMA Hazus sources or independently validated tables before a
real analysis. Required metadata includes source name, source version, source
table, source page where applicable, validator, validation date, validation
status, units, and notes.

```r
schema_dir <- system.file("extdata", "schemas", package = "hazgrid")
list.files(schema_dir)

fragility <- read_hazgrid_lookup(
  "path/to/validated_tsunami_fragility.csv",
  table_type = "tsunami_fragility"
)
```

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

ext <- function(...) system.file("extdata", ..., package = "hazgrid")

tsunami_l2 <- run_tsunami_loss(
  inventory = ext("synthetic_buildings.gpkg"),
  depth = rast(ext("synthetic_depth.tif")),
  velocity = rast(ext("synthetic_velocity.tif")),
  fragility_table = ext("demo", "demo_tsunami_fragility.csv"),
  loss_ratio_table = ext("demo", "demo_tsunami_loss_ratio.csv"),
  allow_demo_tables = TRUE,
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
  fragility_table = ext("demo", "demo_tsunami_fragility.csv"),
  loss_ratio_table = ext("demo", "demo_tsunami_loss_ratio.csv"),
  allow_demo_tables = TRUE
)
```

## Earthquake Capacity Spectrum

The earthquake workflow is being upgraded toward the Hazus capacity-spectrum
method. The current implementation builds an elastic demand spectrum, retrieves
validated bilinear capacity curves, solves a transparent elastic-demand
intersection, and applies validated structural and nonstructural fragilities.
The full Hazus effective-damping iteration remains under development and is
reported explicitly by the code.

```r
earthquake <- run_earthquake_loss(
  inventory = ext("synthetic_buildings.gpkg"),
  pga = rast(ext("synthetic_pga.tif")),
  sa03 = rast(ext("synthetic_sa03.tif")),
  sa10 = rast(ext("synthetic_sa10.tif")),
  pgv = rast(ext("synthetic_pgv.tif")),
  capacity_table = ext("demo", "demo_earthquake_capacity_curve.csv"),
  structural_fragility_table = ext("demo", "demo_earthquake_structural_fragility.csv"),
  drift_fragility_table = ext("demo", "demo_earthquake_nonstructural_drift_fragility.csv"),
  accel_fragility_table = ext("demo", "demo_earthquake_nonstructural_acceleration_fragility.csv"),
  loss_ratio_table = ext("demo", "demo_earthquake_loss_ratio.csv"),
  allow_demo_tables = TRUE,
  group_fields = "municipality"
)

earthquake$summary
```

## Roadmap

Future modules can add curated official lookup-table ingestion,
DuckDB/Arrow/GeoParquet acceleration, expanded tsunami casualty methods,
evacpath integration, full earthquake effective-damping iteration, ground
failure, lifelines, debris, shelter, and combined earthquake-tsunami damage.
