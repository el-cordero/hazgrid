test_that("tsunami damage calculates each component", {
  inventory <- read_inventory(extdata_file("synthetic_buildings.gpkg"))
  hazard <- prepare_tsunami_hazard(
    terra::rast(extdata_file("synthetic_depth.tif")),
    velocity = terra::rast(extdata_file("synthetic_velocity.tif"))
  )
  exposure <- extract_hazard(hazard, inventory)
  damage <- tsunami_damage(
    exposure, extdata_file("tsunami_fragility_example.csv")
  )
  for (component in c("structure", "nonstructural", "contents")) {
    columns <- paste0(
      component, "_p_",
      c("none", "slight", "moderate", "extensive", "complete")
    )
    expect_equal(rowSums(damage[columns]), rep(1, nrow(damage)), tolerance = 1e-12)
  }
})

test_that("casualty MVP returns expected values", {
  exposure <- data.frame(H = c(0.25, 3), population_day = c(10, 10))
  params <- data.frame(
    min_depth = c(-Inf, 1),
    max_depth = c(1, Inf),
    fatality_rate = c(0, 0.1),
    injury_rate = c(0.01, 0.2)
  )
  casualty <- tsunami_casualty_mvp(exposure, params)
  expect_equal(casualty$expected_fatalities, c(0, 1))
  expect_equal(casualty$expected_injuries, c(0.1, 2))
})
