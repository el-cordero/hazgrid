test_that("high-level tsunami workflows reject demos unless explicitly allowed", {
  inventory <- extdata_file("synthetic_buildings.gpkg")
  depth <- terra::rast(extdata_file("synthetic_depth.tif"))
  velocity <- terra::rast(extdata_file("synthetic_velocity.tif"))
  fragility <- demo_file("demo_tsunami_fragility.csv")
  ratios <- demo_file("demo_tsunami_loss_ratio.csv")
  expect_error(
    run_tsunami_loss(
      inventory, depth, velocity = velocity,
      fragility_table = fragility, loss_ratio_table = ratios
    ),
    "Demo-only"
  )
  level2 <- run_tsunami_loss(
    inventory, depth, velocity = velocity,
    fragility_table = fragility, loss_ratio_table = ratios,
    casualty_table = demo_file("demo_tsunami_casualty.csv"),
    include_casualties = TRUE, population_field = "population_day",
    allow_demo_tables = TRUE, group_fields = "municipality"
  )
  level3 <- run_tsunami_loss(
    inventory, depth,
    momentum_flux = compute_tsunami_momentum_flux(depth, velocity),
    fragility_table = fragility, loss_ratio_table = ratios,
    allow_demo_tables = TRUE
  )
  expect_equal(nrow(level2$loss), 12)
  expect_equal(level2$loss$total_loss, level3$loss$total_loss)
  expect_true(inherits(level2$assumptions, "hazgrid_assumptions"))
  expect_true(all(c("expected_fatalities", "expected_injuries") %in% names(level2$loss)))
})

test_that("high-level earthquake capacity-spectrum workflow runs with demos only by opt-in", {
  args <- list(
    inventory = extdata_file("synthetic_buildings.gpkg"),
    pga = terra::rast(extdata_file("synthetic_pga.tif")),
    sa03 = terra::rast(extdata_file("synthetic_sa03.tif")),
    sa10 = terra::rast(extdata_file("synthetic_sa10.tif")),
    pgv = terra::rast(extdata_file("synthetic_pgv.tif")),
    capacity_table = demo_file("demo_earthquake_capacity_curve.csv"),
    structural_fragility_table = demo_file("demo_earthquake_structural_fragility.csv"),
    drift_fragility_table = demo_file("demo_earthquake_nonstructural_drift_fragility.csv"),
    accel_fragility_table = demo_file("demo_earthquake_nonstructural_acceleration_fragility.csv"),
    loss_ratio_table = demo_file("demo_earthquake_loss_ratio.csv"),
    group_fields = "municipality"
  )
  expect_error(do.call(run_earthquake_loss, args), "Demo-only")
  result <- suppressWarnings(do.call(
    run_earthquake_loss, c(args, list(allow_demo_tables = TRUE))
  ))
  expect_equal(nrow(result$damage), 12)
  expect_equal(sum(result$summary$asset_count), 12)
  expect_true(all(result$loss$total_loss >= 0))
  expect_true(inherits(result$assumptions, "hazgrid_assumptions"))
})

test_that("workflow output writing includes provenance files", {
  output_dir <- file.path(tempdir(), "hazgrid-output")
  unlink(output_dir, recursive = TRUE)
  result <- run_tsunami_loss(
    extdata_file("synthetic_buildings.gpkg"),
    terra::rast(extdata_file("synthetic_depth.tif")),
    velocity = terra::rast(extdata_file("synthetic_velocity.tif")),
    fragility_table = demo_file("demo_tsunami_fragility.csv"),
    loss_ratio_table = demo_file("demo_tsunami_loss_ratio.csv"),
    output_dir = output_dir, allow_demo_tables = TRUE
  )
  expect_true(all(file.exists(result$output_paths)))
  expect_true(all(c("assumptions", "lookup_validation") %in% names(result$output_paths)))
})
