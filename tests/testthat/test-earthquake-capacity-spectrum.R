test_that("earthquake hazard layers validate and combine", {
  hazard <- prepare_earthquake_hazard(
    terra::rast(extdata_file("synthetic_pga.tif")),
    terra::rast(extdata_file("synthetic_sa03.tif")),
    terra::rast(extdata_file("synthetic_sa10.tif")),
    terra::rast(extdata_file("synthetic_pgv.tif"))
  )
  expect_s4_class(hazard, "SpatRaster")
  expect_equal(names(hazard), c("PGA", "SA03", "SA10", "PGV"))
})

test_that("earthquake acceleration units must match", {
  pga <- terra::rast(extdata_file("synthetic_pga.tif"))
  sa03 <- terra::rast(extdata_file("synthetic_sa03.tif"))
  sa10 <- terra::rast(extdata_file("synthetic_sa10.tif"))
  terra::units(sa03) <- "m/s2"
  expect_error(prepare_earthquake_hazard(pga, sa03, sa10), "units differ")
})

test_that("demand spectrum and performance point are transparent", {
  spectrum <- build_demand_spectrum(0.2, 0.5, 0.25)
  curve <- capacity_curve(
    demo_file("demo_earthquake_capacity_curve.csv"),
    "W1", "moderate", allow_demo_tables = TRUE
  )
  point <- NULL
  expect_warning(
    point <- performance_point(spectrum, curve),
    "effective-damping iteration"
  )
  expect_true(point$performance_sd > 0)
  expect_true(point$performance_sa > 0)
})

test_that("earthquake capacity-spectrum does not silently fall back", {
  exposure <- data.frame(
    structure_type = "W1", design_level = "moderate",
    PGA = 0.2, SA03 = 0.5, SA10 = 0.25
  )
  expect_error(
    earthquake_damage(
      exposure,
      structural_fragility_table = demo_file("demo_earthquake_structural_fragility.csv"),
      allow_demo_tables = TRUE
    ),
    "requires a validated capacity_table"
  )
})

test_that("earthquake damage runs with explicit synthetic opt-in", {
  hazard <- prepare_earthquake_hazard(
    terra::rast(extdata_file("synthetic_pga.tif")),
    terra::rast(extdata_file("synthetic_sa03.tif")),
    terra::rast(extdata_file("synthetic_sa10.tif"))
  )
  exposure <- extract_hazard(
    hazard, read_inventory(extdata_file("synthetic_buildings.gpkg"))
  )
  args <- list(
    exposure = exposure,
    capacity_table = demo_file("demo_earthquake_capacity_curve.csv"),
    structural_fragility_table = demo_file("demo_earthquake_structural_fragility.csv")
  )
  expect_error(do.call(earthquake_damage, args), "Demo-only")
  damage <- suppressWarnings(do.call(
    earthquake_damage,
    c(args, list(allow_demo_tables = TRUE))
  ))
  probabilities <- damage[paste0("structure_p_", c("none", "slight", "moderate", "extensive", "complete"))]
  expect_equal(rowSums(probabilities), rep(1, nrow(damage)), tolerance = 1e-12)
})
