test_that("tsunami losses are calculated and summarized", {
  inventory <- read_inventory(extdata_file("synthetic_buildings.gpkg"))
  hazard <- prepare_tsunami_hazard(
    terra::rast(extdata_file("synthetic_depth.tif")),
    velocity = terra::rast(extdata_file("synthetic_velocity.tif"))
  )
  damage <- tsunami_damage(
    extract_hazard(hazard, inventory),
    extdata_file("tsunami_fragility_example.csv")
  )
  loss <- tsunami_economic_loss(
    damage, extdata_file("tsunami_loss_ratios_example.csv")
  )
  expect_true(all(loss$total_loss >= 0))
  expect_equal(
    loss$total_loss,
    loss$structural_loss + loss$nonstructural_loss + loss$contents_loss
  )
  summary <- summarize_by(loss, "municipality")
  expect_equal(sum(summary$asset_count), nrow(loss))
  expect_equal(sum(summary$total_loss), sum(loss$total_loss))
})
