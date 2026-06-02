test_that("tsunami damage calculates each component", {
  inventory <- read_inventory(extdata_file("synthetic_buildings.gpkg"))
  hazard <- prepare_tsunami_hazard(
    terra::rast(extdata_file("synthetic_depth.tif")),
    velocity = terra::rast(extdata_file("synthetic_velocity.tif"))
  )
  exposure <- extract_hazard(hazard, inventory)
  damage <- tsunami_damage(
    exposure, demo_file("demo_tsunami_fragility.csv"),
    allow_demo_tables = TRUE
  )
  for (component in c("structure", "nonstructural", "contents")) {
    columns <- paste0(
      component, "_p_",
      c("none", "slight", "moderate", "extensive", "complete")
    )
    expect_equal(rowSums(damage[columns]), rep(1, nrow(damage)), tolerance = 1e-12)
  }
})

test_that("tsunami casualty requires table opt-in for synthetic data", {
  exposure <- data.frame(H = c(0.25, 3), population_day = c(10, 10))
  table <- demo_file("demo_tsunami_casualty.csv")
  expect_error(
    tsunami_casualty(exposure, table),
    "Demo-only"
  )
  casualty <- tsunami_casualty(exposure, table, allow_demo_tables = TRUE)
  expect_equal(casualty$expected_fatalities, c(0, 0.1))
  expect_equal(casualty$expected_injuries, c(0, 0.5))
})
