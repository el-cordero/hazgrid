test_that("lookup tables are typed and carry provenance", {
  table <- read_hazgrid_lookup(
    demo_file("demo_tsunami_fragility.csv"), "tsunami_fragility"
  )
  expect_s3_class(table, "hazgrid_lookup")
  expect_equal(attr(table, "table_type"), "tsunami_fragility")
  expect_equal(unique(table$validation_status), "demo_only")
})

test_that("lookup schema validation rejects malformed tables", {
  table <- utils::read.csv(
    demo_file("demo_tsunami_loss_ratio.csv"),
    stringsAsFactors = FALSE
  )
  table$source_name <- NULL
  expect_error(
    validate_lookup_table(table, "tsunami_loss_ratio"),
    "source_name"
  )
  table <- utils::read.csv(
    demo_file("demo_tsunami_loss_ratio.csv"),
    stringsAsFactors = FALSE
  )
  table <- rbind(table, table[1L, ])
  expect_error(
    validate_lookup_table(table, "tsunami_loss_ratio"),
    "duplicate"
  )
})

test_that("assumptions can be written as JSON", {
  assumptions <- hazgrid_assumptions(
    hazard_model = "tsunami", units = c(H = "m")
  )
  path <- tempfile(fileext = ".json")
  write_assumptions(assumptions, path)
  expect_true(file.exists(path))
  expect_match(paste(readLines(path), collapse = "\n"), '"hazard_model": "tsunami"')
})

test_that("deprecated wrappers remain callable", {
  exposure <- data.frame(H = c(0.25, 3), population_day = c(10, 10))
  expect_warning(
    tsunami_casualty_mvp(
      exposure, demo_file("demo_tsunami_casualty.csv"),
      allow_demo_tables = TRUE
    ),
    "deprecated"
  )
  exposure <- data.frame(
    structure_type = "W1", design_level = "moderate", PGA = 0.2
  )
  expect_warning(
    earthquake_damage_mvp(
      exposure, demo_file("demo_earthquake_simple_fragility.csv"),
      allow_demo_tables = TRUE
    ),
    "deprecated"
  )
  expect_warning(
    run_earthquake_mvp(
      extdata_file("synthetic_buildings.gpkg"),
      terra::rast(extdata_file("synthetic_pga.tif")),
      terra::rast(extdata_file("synthetic_sa03.tif")),
      terra::rast(extdata_file("synthetic_sa10.tif")),
      fragility_table = demo_file("demo_earthquake_simple_fragility.csv"),
      allow_demo_tables = TRUE
    ),
    "deprecated"
  )
})
