test_that("tsunami losses are calculated and summarized", {
  inventory <- read_inventory(extdata_file("synthetic_buildings.gpkg"))
  hazard <- prepare_tsunami_hazard(
    terra::rast(extdata_file("synthetic_depth.tif")),
    velocity = terra::rast(extdata_file("synthetic_velocity.tif"))
  )
  damage <- tsunami_damage(
    extract_hazard(hazard, inventory),
    demo_file("demo_tsunami_fragility.csv"),
    allow_demo_tables = TRUE
  )
  loss <- tsunami_economic_loss(
    damage, demo_file("demo_tsunami_loss_ratio.csv"),
    allow_demo_tables = TRUE
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

test_that("summaries retain missing group values", {
  results <- data.frame(
    municipality = c("North", NA, ""),
    total_loss = c(1, 2, 3),
    H = c(1, 2, 3)
  )
  summary <- summarize_by(results, "municipality")
  expect_equal(sum(summary$asset_count), 3)
  expect_true("(missing)" %in% summary$municipality)
  expect_equal(summary$total_loss[summary$municipality == "(missing)"], 5)
})
