test_that("high-level tsunami Level 2 and Level 3 workflows run", {
  inventory <- extdata_file("synthetic_buildings.gpkg")
  depth <- terra::rast(extdata_file("synthetic_depth.tif"))
  velocity <- terra::rast(extdata_file("synthetic_velocity.tif"))
  fragility <- extdata_file("tsunami_fragility_example.csv")
  ratios <- extdata_file("tsunami_loss_ratios_example.csv")
  level2 <- run_tsunami_loss(
    inventory, depth, velocity = velocity,
    fragility_table = fragility, loss_ratio_table = ratios,
    group_fields = "municipality"
  )
  level3 <- run_tsunami_loss(
    inventory, depth,
    momentum_flux = compute_tsunami_momentum_flux(depth, velocity),
    fragility_table = fragility, loss_ratio_table = ratios
  )
  expect_equal(nrow(level2$loss), 12)
  expect_equal(level2$loss$total_loss, level3$loss$total_loss)
  expect_equal(sum(level2$summary$asset_count), 12)
})

test_that("high-level earthquake MVP workflow runs", {
  result <- run_earthquake_mvp(
    extdata_file("synthetic_buildings.gpkg"),
    terra::rast(extdata_file("synthetic_pga.tif")),
    terra::rast(extdata_file("synthetic_sa03.tif")),
    terra::rast(extdata_file("synthetic_sa10.tif")),
    pgv = terra::rast(extdata_file("synthetic_pgv.tif")),
    fragility_table = extdata_file("earthquake_fragility_example.csv"),
    group_fields = "municipality"
  )
  expect_equal(nrow(result$damage), 12)
  expect_equal(sum(result$summary$asset_count), 12)
})

test_that("workflow output writing produces CSV and GeoPackage files", {
  output_dir <- file.path(tempdir(), "hazgrid-output")
  unlink(output_dir, recursive = TRUE)
  result <- run_tsunami_loss(
    extdata_file("synthetic_buildings.gpkg"),
    terra::rast(extdata_file("synthetic_depth.tif")),
    velocity = terra::rast(extdata_file("synthetic_velocity.tif")),
    fragility_table = extdata_file("tsunami_fragility_example.csv"),
    loss_ratio_table = extdata_file("tsunami_loss_ratios_example.csv"),
    output_dir = output_dir
  )
  expect_true(all(file.exists(result$output_paths)))
})
