#' Run a staged tsunami loss workflow
#'
#' Runs inventory reading, tsunami hazard preparation, hazard extraction,
#' component damage probabilities, expected economic losses, optional summaries,
#' and optional CSV/GeoPackage output writing.
#'
#' @param inventory A `terra::SpatVector` or inventory path.
#' @param depth Inundation depth raster or path.
#' @param velocity Optional flow velocity raster or path for Level 2 analysis.
#' @param momentum_flux Optional momentum-flux raster or path for Level 3 analysis.
#' @param fragility_table Data.frame or CSV path with tsunami fragility curves.
#' @param loss_ratio_table Data.frame or CSV path with expected loss ratios.
#' @param casualty_table Optional validated tsunami casualty table.
#' @param include_casualties Calculate casualty outputs.
#' @param population_field Population field used for casualty calculations.
#' @param assumptions Optional [hazgrid_assumptions()] object.
#' @param allow_demo_tables Allow synthetic demo tables. Never use this for
#'   real-world analysis.
#' @param auto_convert_units Convert metric velocity from `"cm/s"` to `"m/s"`.
#' @param output_dir Optional output directory.
#' @param group_fields Optional fields passed to [summarize_by()].
#'
#' @return A list containing workflow tables, assumptions, lookup validation,
#'   and output paths.
#' @export
run_tsunami_loss <- function(
  inventory, depth, velocity = NULL, momentum_flux = NULL,
  fragility_table, loss_ratio_table,
  casualty_table = NULL, include_casualties = FALSE, population_field = NULL,
  assumptions = NULL, allow_demo_tables = FALSE, auto_convert_units = FALSE,
  output_dir = NULL, group_fields = NULL
) {
  inventory_source <- if (is.character(inventory)) inventory else "terra::SpatVector"
  inventory <- .read_inventory_input(inventory)
  fragility_table <- .as_hazgrid_lookup(fragility_table, "tsunami_fragility")
  loss_ratio_table <- .as_hazgrid_lookup(loss_ratio_table, "tsunami_loss_ratio")
  .assert_lookup_allowed(fragility_table, allow_demo_tables)
  .assert_lookup_allowed(loss_ratio_table, allow_demo_tables)
  if (isTRUE(include_casualties) && is.null(casualty_table)) {
    stop("include_casualties = TRUE requires a validated casualty_table.", call. = FALSE)
  }
  if (!is.null(casualty_table)) {
    casualty_table <- .as_hazgrid_lookup(casualty_table, "tsunami_casualty")
    .assert_lookup_allowed(casualty_table, allow_demo_tables)
  }
  hazard <- prepare_tsunami_hazard(
    depth = depth, velocity = velocity, momentum_flux = momentum_flux,
    auto_convert_units = auto_convert_units
  )
  exposure <- extract_hazard(hazard, inventory)
  damage <- tsunami_damage(exposure, fragility_table, allow_demo_tables = allow_demo_tables)
  loss <- tsunami_economic_loss(damage, loss_ratio_table, allow_demo_tables = allow_demo_tables)
  casualty <- NULL
  if (isTRUE(include_casualties)) {
    if (is.null(population_field)) {
      stop("include_casualties = TRUE requires population_field.", call. = FALSE)
    }
    casualty <- tsunami_casualty(
      exposure, casualty_table, population_field = population_field,
      allow_demo_tables = allow_demo_tables
    )
    loss$expected_fatalities <- casualty$expected_fatalities
    loss$expected_injuries <- casualty$expected_injuries
  }
  summary <- if (is.null(group_fields)) NULL else summarize_by(loss, group_fields)
  lookup_validation <- .collect_lookup_validation(
    list(fragility_table, loss_ratio_table, casualty_table)
  )
  if (is.null(assumptions)) {
    assumptions <- hazgrid_assumptions(
      hazard_model = "tsunami",
      inventory_source = inventory_source,
      lookup_table_sources = .lookup_sources(lookup_validation),
      units = stats::setNames(terra::units(hazard), names(hazard)),
      analysis_level = if ("V" %in% names(hazard)) "level2" else "level3",
      tsunami_method = "Hazus-style structural HV2 and flood-depth component fragilities",
      casualty_method = if (isTRUE(include_casualties)) "validated depth-bin casualty table" else NULL
    )
  }
  output_paths <- .write_workflow_outputs(
    "tsunami",
    list(
      exposure = exposure, damage = damage, loss = loss,
      casualty = casualty, summary = summary
    ),
    inventory, loss, output_dir, assumptions, lookup_validation
  )
  list(
    exposure = exposure,
    damage = damage,
    loss = loss,
    casualty = casualty,
    summary = summary,
    assumptions = assumptions,
    lookup_validation = lookup_validation,
    output_paths = output_paths
  )
}
