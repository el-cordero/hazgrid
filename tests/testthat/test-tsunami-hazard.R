test_that("tsunami Level 2 computes momentum flux", {
  depth <- terra::rast(extdata_file("synthetic_depth.tif"))
  velocity <- terra::rast(extdata_file("synthetic_velocity.tif"))
  hazard <- prepare_tsunami_hazard(depth, velocity = velocity, level = "level2")
  expect_equal(names(hazard), c("H", "V", "HV2"))
  expect_equal(
    as.numeric(terra::values(hazard$HV2)),
    as.numeric(terra::values(depth) * terra::values(velocity)^2)
  )
  expect_equal(terra::units(hazard$HV2), "m3/s2")
})

test_that("tsunami Level 3 accepts momentum flux directly", {
  depth <- terra::rast(extdata_file("synthetic_depth.tif"))
  velocity <- terra::rast(extdata_file("synthetic_velocity.tif"))
  flux <- compute_tsunami_momentum_flux(depth, velocity)
  hazard <- prepare_tsunami_hazard(depth, momentum_flux = flux, level = "level3")
  expect_equal(names(hazard), c("H", "HV2"))
  expect_equal(
    as.numeric(terra::values(hazard$HV2)),
    as.numeric(terra::values(flux))
  )
})

test_that("hazard unit conversions are explicit", {
  metric_depth <- structure(1, units = "m")
  metric_velocity <- structure(2, units = "m/s")
  metric_flux <- compute_tsunami_momentum_flux(metric_depth, metric_velocity)
  expect_equal(as.numeric(metric_flux), 4)
  expect_equal(attr(metric_flux, "units"), "m3/s2")

  depth <- structure(3, units = "ft")
  velocity <- structure(2, units = "ft/s")
  flux <- compute_tsunami_momentum_flux(depth, velocity)
  expect_equal(as.numeric(flux), 12)
  expect_equal(attr(flux, "units"), "ft3/s2")
  expect_equal(
    as.numeric(convert_hazard_units(depth, "ft", "m", "depth")),
    0.9144
  )
  expect_error(
    compute_tsunami_momentum_flux(depth, structure(2, units = "m/s")),
    "different measurement systems"
  )
})

test_that("unit systems recognize metric cm/s and require conversion for flux", {
  expect_equal(hazgrid:::.unit_system("m", "depth"), "metric")
  expect_equal(hazgrid:::.unit_system("cm/s", "velocity"), "metric")
  expect_equal(hazgrid:::.unit_system("ft/s", "velocity"), "imperial")
  expect_equal(hazgrid:::.unit_system("g", "acceleration"), "metric")
  expect_equal(hazgrid:::.unit_system("ft3/s2", "momentum_flux"), "imperial")
  depth <- structure(1, units = "m")
  velocity <- structure(100, units = "cm/s")
  expect_error(
    compute_tsunami_momentum_flux(depth, velocity),
    "must be explicitly converted"
  )
  flux <- compute_tsunami_momentum_flux(
    depth, velocity, auto_convert_units = TRUE
  )
  expect_equal(as.numeric(flux), 1)
  expect_equal(attr(flux, "units"), "m3/s2")
})

test_that("Level 3 momentum flux units must match depth system", {
  depth <- structure(1, units = "m")
  flux <- structure(1, units = "ft3/s2")
  expect_error(
    hazgrid:::.validate_tsunami_units(depth, momentum_flux = flux),
    "different measurement systems"
  )
})

test_that("inventories are read and hazards are extracted", {
  inventory <- read_inventory(extdata_file("synthetic_buildings.gpkg"))
  hazard <- prepare_tsunami_hazard(
    terra::rast(extdata_file("synthetic_depth.tif")),
    velocity = terra::rast(extdata_file("synthetic_velocity.tif"))
  )
  exposure <- extract_hazard(hazard, inventory)
  expect_s4_class(inventory, "SpatVector")
  expect_equal(nrow(exposure), 12)
  expect_true(all(c("building_id", "H", "V", "HV2") %in% names(exposure)))
})
