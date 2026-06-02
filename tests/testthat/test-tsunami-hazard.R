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
    "not directly compatible"
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
