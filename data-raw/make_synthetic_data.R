# Generate small offline example inputs for hazgrid.
# All lookup values in this script are invented for demonstration and testing.
# They are not official or validated FEMA Hazus values.

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

states <- c("slight", "moderate", "extensive", "complete")
types <- expand.grid(
  structure_type = c("W1", "C1"),
  design_level = c("moderate", "high"),
  component = c("structure", "nonstructural", "contents"),
  ds = states,
  stringsAsFactors = FALSE
)
tsunami_base <- list(
  structure = c(slight = 2, moderate = 8, extensive = 20, complete = 40),
  nonstructural = c(slight = 0.5, moderate = 1.5, extensive = 3, complete = 5),
  contents = c(slight = 0.3, moderate = 1, extensive = 2.5, complete = 4.5)
)
types$median <- mapply(
  function(structure_type, design_level, component, ds) {
    type_factor <- if (structure_type == "C1") 1.4 else 1
    design_factor <- if (design_level == "high") 1.5 else 1
    tsunami_base[[component]][[ds]] * type_factor * design_factor
  },
  types$structure_type, types$design_level, types$component, types$ds
)
types$beta <- 0.65
types$driver <- ifelse(types$component == "structure", "HV2", "H")
types$note <- "Demo-only invented values; not official FEMA Hazus values."
write.csv(
  types,
  file.path(output_dir, "tsunami_fragility_example.csv"),
  row.names = FALSE
)

loss_ratios <- expand.grid(
  component = c("structure", "nonstructural", "contents"),
  damage_state = c("none", states),
  stringsAsFactors = FALSE
)
loss_ratio_values <- list(
  structure = c(none = 0, slight = 0.03, moderate = 0.12, extensive = 0.45, complete = 1),
  nonstructural = c(none = 0, slight = 0.05, moderate = 0.2, extensive = 0.6, complete = 1),
  contents = c(none = 0, slight = 0.08, moderate = 0.3, extensive = 0.7, complete = 1)
)
loss_ratios$loss_ratio <- mapply(
  function(component, damage_state) loss_ratio_values[[component]][[damage_state]],
  loss_ratios$component, loss_ratios$damage_state
)
loss_ratios$note <- "Demo-only invented values; not official FEMA Hazus values."
write.csv(
  loss_ratios,
  file.path(output_dir, "tsunami_loss_ratios_example.csv"),
  row.names = FALSE
)

earthquake_fragility <- expand.grid(
  structure_type = c("W1", "C1"),
  design_level = c("moderate", "high"),
  ds = states,
  stringsAsFactors = FALSE
)
eq_base <- c(slight = 0.12, moderate = 0.25, extensive = 0.45, complete = 0.8)
earthquake_fragility$median <- mapply(
  function(structure_type, design_level, ds) {
    type_factor <- if (structure_type == "C1") 1.25 else 1
    design_factor <- if (design_level == "high") 1.4 else 1
    eq_base[[ds]] * type_factor * design_factor
  },
  earthquake_fragility$structure_type,
  earthquake_fragility$design_level,
  earthquake_fragility$ds
)
earthquake_fragility$beta <- 0.65
earthquake_fragility$driver <- "PGA"
earthquake_fragility$note <- "Demo-only invented values; not official FEMA Hazus values."
write.csv(
  earthquake_fragility,
  file.path(output_dir, "earthquake_fragility_example.csv"),
  row.names = FALSE
)
