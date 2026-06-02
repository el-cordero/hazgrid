# Generate synthetic demo lookup tables and empty validated-table schema files.
# Demo values are invented exclusively for offline examples and tests.
# They are not official FEMA Hazus values and are not valid for real analysis.

demo_dir <- file.path("inst", "extdata", "demo")
schema_dir <- file.path("inst", "extdata", "schemas")
dir.create(demo_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(schema_dir, recursive = TRUE, showWarnings = FALSE)

provenance <- function(x, units) {
  x$source_name <- "hazgrid synthetic demo"
  x$source_version <- "0.1.0"
  x$source_table <- "invented synthetic values"
  x$source_page <- "not_applicable"
  x$validated_by <- "not_validated_demo_only"
  x$validation_date <- "2026-06-02"
  x$validation_status <- "demo_only"
  x$units <- units
  x$notes <- "Invented synthetic demo values; not official FEMA Hazus values; do not use for real decisions."
  x
}

write_demo <- function(x, filename) {
  utils::write.csv(x, file.path(demo_dir, filename), row.names = FALSE)
}

write_schema <- function(filename, columns) {
  x <- stats::setNames(
    as.data.frame(matrix(nrow = 0L, ncol = length(columns))),
    columns
  )
  utils::write.csv(x, file.path(schema_dir, filename), row.names = FALSE)
}

states <- c("slight", "moderate", "extensive", "complete")
full_states <- c("none", states)
structures <- c("W1", "C1")
levels <- c("moderate", "high")

tsunami <- expand.grid(
  structure_type = structures, design_level = levels,
  component = c("structure", "nonstructural", "contents"),
  ds = states, stringsAsFactors = FALSE
)
tsunami_base <- list(
  structure = c(slight = 2, moderate = 8, extensive = 20, complete = 40),
  nonstructural = c(slight = 0.5, moderate = 1.5, extensive = 3, complete = 5),
  contents = c(slight = 0.3, moderate = 1, extensive = 2.5, complete = 4.5)
)
tsunami$median <- mapply(function(structure_type, design_level, component, ds) {
  tsunami_base[[component]][[ds]] *
    ifelse(structure_type == "C1", 1.4, 1) *
    ifelse(design_level == "high", 1.5, 1)
}, tsunami$structure_type, tsunami$design_level, tsunami$component, tsunami$ds)
tsunami$beta <- 0.65
tsunami$driver <- ifelse(tsunami$component == "structure", "HV2", "H")
tsunami <- provenance(tsunami, ifelse(tsunami$component == "structure", "m3/s2", "m"))
write_demo(tsunami, "demo_tsunami_fragility.csv")

tsunami_ratios <- expand.grid(
  component = c("structure", "nonstructural", "contents"),
  damage_state = full_states, stringsAsFactors = FALSE
)
ratio_values <- list(
  structure = c(none = 0, slight = 0.03, moderate = 0.12, extensive = 0.45, complete = 1),
  nonstructural = c(none = 0, slight = 0.05, moderate = 0.2, extensive = 0.6, complete = 1),
  contents = c(none = 0, slight = 0.08, moderate = 0.3, extensive = 0.7, complete = 1)
)
tsunami_ratios$loss_ratio <- mapply(
  function(component, damage_state) ratio_values[[component]][[damage_state]],
  tsunami_ratios$component, tsunami_ratios$damage_state
)
write_demo(provenance(tsunami_ratios, "ratio"), "demo_tsunami_loss_ratio.csv")

casualty <- data.frame(
  min_depth = c(-Inf, 0.5, 2, 5),
  max_depth = c(0.5, 2, 5, Inf),
  fatality_rate = c(0, 0.001, 0.01, 0.05),
  injury_rate = c(0, 0.01, 0.05, 0.15)
)
write_demo(provenance(casualty, "m"), "demo_tsunami_casualty.csv")

capacity <- expand.grid(
  structure_type = structures, design_level = levels, stringsAsFactors = FALSE
)
capacity$yield_sd <- c(0.3, 0.4, 0.45, 0.55)
capacity$yield_sa <- c(0.15, 0.2, 0.22, 0.28)
capacity$ultimate_sd <- c(3, 3.5, 4, 4.5)
capacity$ultimate_sa <- c(0.35, 0.42, 0.5, 0.58)
write_demo(provenance(capacity, "in|g"), "demo_earthquake_capacity_curve.csv")

make_eq_fragility <- function(driver, medians, units, filename) {
  x <- expand.grid(
    structure_type = structures, design_level = levels,
    ds = states, stringsAsFactors = FALSE
  )
  x$median <- mapply(function(structure_type, design_level, ds) {
    medians[[ds]] *
      ifelse(structure_type == "C1", 1.2, 1) *
      ifelse(design_level == "high", 1.35, 1)
  }, x$structure_type, x$design_level, x$ds)
  x$beta <- 0.65
  x$driver <- driver
  write_demo(provenance(x, units), filename)
}

make_eq_fragility(
  "performance_sd", c(slight = 0.5, moderate = 1.2, extensive = 2.3, complete = 4),
  "in", "demo_earthquake_structural_fragility.csv"
)
make_eq_fragility(
  "performance_sd", c(slight = 0.4, moderate = 1, extensive = 2, complete = 3.5),
  "in", "demo_earthquake_nonstructural_drift_fragility.csv"
)
make_eq_fragility(
  "performance_sa", c(slight = 0.12, moderate = 0.25, extensive = 0.45, complete = 0.8),
  "g", "demo_earthquake_nonstructural_acceleration_fragility.csv"
)
make_eq_fragility(
  "PGA", c(slight = 0.12, moderate = 0.25, extensive = 0.45, complete = 0.8),
  "g", "demo_earthquake_simple_fragility.csv"
)

eq_ratios <- expand.grid(
  component = c("structure", "nonstructural_drift", "nonstructural_acceleration"),
  damage_state = full_states, stringsAsFactors = FALSE
)
eq_values <- list(
  structure = c(none = 0, slight = 0.03, moderate = 0.12, extensive = 0.45, complete = 1),
  nonstructural_drift = c(none = 0, slight = 0.02, moderate = 0.08, extensive = 0.3, complete = 0.6),
  nonstructural_acceleration = c(none = 0, slight = 0.03, moderate = 0.1, extensive = 0.35, complete = 0.7)
)
eq_ratios$loss_ratio <- mapply(
  function(component, damage_state) eq_values[[component]][[damage_state]],
  eq_ratios$component, eq_ratios$damage_state
)
write_demo(provenance(eq_ratios, "ratio"), "demo_earthquake_loss_ratio.csv")

metadata <- c(
  "source_name", "source_version", "source_table", "source_page",
  "validated_by", "validation_date", "validation_status", "units", "notes"
)
write_schema("tsunami_fragility.csv", c("structure_type", "design_level", "component", "ds", "median", "beta", "driver", metadata))
write_schema("tsunami_loss_ratio.csv", c("component", "damage_state", "loss_ratio", metadata))
write_schema("tsunami_casualty.csv", c("min_depth", "max_depth", "fatality_rate", "injury_rate", metadata))
write_schema("earthquake_capacity_curve.csv", c("structure_type", "design_level", "yield_sd", "yield_sa", "ultimate_sd", "ultimate_sa", metadata))
write_schema("earthquake_structural_fragility.csv", c("structure_type", "design_level", "ds", "median", "beta", "driver", metadata))
write_schema("earthquake_nonstructural_drift_fragility.csv", c("structure_type", "design_level", "ds", "median", "beta", "driver", metadata))
write_schema("earthquake_nonstructural_acceleration_fragility.csv", c("structure_type", "design_level", "ds", "median", "beta", "driver", metadata))
write_schema("earthquake_loss_ratio.csv", c("component", "damage_state", "loss_ratio", metadata))
write_schema("earthquake_design_level_mapping.csv", c("input_design_level", "design_level", metadata))
write_schema("earthquake_occupancy_mapping.csv", c("occupancy", "occupancy_class", metadata))
write_schema("restoration_function.csv", c("component", "damage_state", "days", "recovery_fraction", metadata))
