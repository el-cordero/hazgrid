# Generate small offline example inputs for hazgrid.
# Run data-raw/make_lookup_data.R separately to generate isolated demo lookup
# tables and empty validated-table schemas.

library(terra)

output_dir <- file.path("inst", "extdata")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

template <- rast(
  nrows = 5, ncols = 5,
  xmin = 0, xmax = 500, ymin = 0, ymax = 500,
  crs = "EPSG:32620"
)

write_demo_raster <- function(values, filename, unit) {
  raster <- template
  values(raster) <- values
  units(raster) <- unit
  writeRaster(raster, file.path(output_dir, filename), overwrite = TRUE)
}

write_demo_raster(
  seq(0.2, 5, length.out = ncell(template)),
  "synthetic_depth.tif", "m"
)
write_demo_raster(
  seq(0.5, 4, length.out = ncell(template)),
  "synthetic_velocity.tif", "m/s"
)
write_demo_raster(
  seq(0.1, 0.7, length.out = ncell(template)),
  "synthetic_pga.tif", "g"
)
write_demo_raster(
  seq(0.15, 0.9, length.out = ncell(template)),
  "synthetic_sa03.tif", "g"
)
write_demo_raster(
  seq(0.08, 0.55, length.out = ncell(template)),
  "synthetic_sa10.tif", "g"
)
write_demo_raster(
  seq(5, 45, length.out = ncell(template)),
  "synthetic_pgv.tif", "cm/s"
)

buildings <- data.frame(
  building_id = sprintf("B%02d", 1:12),
  occupancy = rep(c("RES1", "COM1", "RES3"), 4),
  structure_type = rep(c("W1", "C1"), each = 6),
  design_level = rep(c("moderate", "high"), 6),
  replacement_value = seq(150000, 370000, length.out = 12),
  contents_value = seq(50000, 160000, length.out = 12),
  population_day = c(2, 4, 3, 2, 6, 5, 3, 5, 4, 2, 8, 6),
  population_night = c(5, 2, 8, 4, 1, 7, 6, 2, 9, 3, 2, 8),
  municipality = rep(c("North", "South"), each = 6),
  x = c(50, 150, 250, 350, 450, 75, 175, 275, 375, 475, 125, 425),
  y = c(450, 450, 350, 350, 250, 250, 150, 150, 50, 50, 325, 125)
)
inventory <- vect(buildings, geom = c("x", "y"), crs = "EPSG:32620")
writeVector(
  inventory,
  file.path(output_dir, "synthetic_buildings.gpkg"),
  overwrite = TRUE
)
