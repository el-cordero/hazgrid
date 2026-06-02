test_that("earthquake hazard and MVP damage run", {
  hazard <- prepare_earthquake_hazard(
    terra::rast(extdata_file("synthetic_pga.tif")),
    terra::rast(extdata_file("synthetic_sa03.tif")),
    terra::rast(extdata_file("synthetic_sa10.tif")),
    terra::rast(extdata_file("synthetic_pgv.tif"))
  )
  expect_s4_class(hazard, "SpatRaster")
  expect_equal(names(hazard), c("PGA", "SA03", "SA10", "PGV"))
  exposure <- extract_hazard(
    hazard, read_inventory(extdata_file("synthetic_buildings.gpkg"))
  )
  damage <- earthquake_damage_mvp(
    exposure, extdata_file("earthquake_fragility_example.csv")
  )
  probabilities <- damage[c("p_none", "p_slight", "p_moderate", "p_extensive", "p_complete")]
  expect_equal(rowSums(probabilities), rep(1, nrow(damage)), tolerance = 1e-12)
})

test_that("earthquake acceleration units must match", {
  pga <- terra::rast(extdata_file("synthetic_pga.tif"))
  sa03 <- terra::rast(extdata_file("synthetic_sa03.tif"))
  sa10 <- terra::rast(extdata_file("synthetic_sa10.tif"))
  terra::units(sa03) <- "m/s2"
  expect_error(
    prepare_earthquake_hazard(pga, sa03, sa10),
    "units differ"
  )
})
