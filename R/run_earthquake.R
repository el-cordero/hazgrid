#' Run an earthquake loss workflow
#'
#' Runs earthquake hazard preparation, extraction, capacity-spectrum damage,
#' optional economic loss, summaries, assumptions, and output writing. The
#' capacity-spectrum framework is in development: the current performance point
#' is the elastic-demand intersection and reports that limitation explicitly.
#'
#' @param inventory A `terra::SpatVector` or inventory path.
#' @param pga Peak ground acceleration raster or path.
#' @param sa03 0.3-second spectral acceleration raster or path.
#' @param sa10 1.0-second spectral acceleration raster or path.
#' @param pgv Optional peak ground velocity raster or path.
#' @param capacity_table Validated earthquake capacity-curve table or path.
#' @param structural_fragility_table Validated structural fragility table.
#' @param drift_fragility_table Optional drift-sensitive fragility table.
#' @param accel_fragility_table Optional acceleration-sensitive fragility table.
#' @param loss_ratio_table Optional earthquake loss-ratio table.
#' @param output_dir Optional output directory.
#' @param group_fields Optional fields passed to [summarize_by()].
#' @param assumptions Optional [hazgrid_assumptions()] object.
#' @param magnitude Optional earthquake magnitude.
#' @param allow_demo_tables Allow synthetic demo tables. Never use this for
#'   real-world analysis.
#'
#' @return A list containing workflow tables, assumptions, lookup validation,
#'   and output paths.
#' @export
run_earthquake_loss <- function(
  inventory, pga, sa03, sa10, pgv = NULL,
  capacity_table, structural_fragility_table,
  drift_fragility_table = NULL, accel_fragility_table = NULL,
  loss_ratio_table = NULL, output_dir = NULL, group_fields = NULL,
  assumptions = NULL, magnitude = NULL, allow_demo_tables = FALSE
) {
  inventory_source <- if (is.character(inventory)) inventory else "terra::SpatVector"
  inventory <- .read_inventory_input(inventory)
  capacity_table <- .as_hazgrid_lookup(capacity_table, "earthquake_capacity_curve")
  structural_fragility_table <- .as_hazgrid_lookup(
    structural_fragility_table, "earthquake_structural_fragility"
  )
  if (!is.null(drift_fragility_table)) {
    drift_fragility_table <- .as_hazgrid_lookup(
      drift_fragility_table, "earthquake_nonstructural_drift_fragility"
    )
  }
  if (!is.null(accel_fragility_table)) {
    accel_fragility_table <- .as_hazgrid_lookup(
      accel_fragility_table, "earthquake_nonstructural_acceleration_fragility"
    )
  }
  if (!is.null(loss_ratio_table)) {
    loss_ratio_table <- .as_hazgrid_lookup(loss_ratio_table, "earthquake_loss_ratio")
  }
  tables <- list(
    capacity_table, structural_fragility_table, drift_fragility_table,
    accel_fragility_table, loss_ratio_table
  )
  lapply(tables[!vapply(tables, is.null, logical(1))], .assert_lookup_allowed, allow_demo_tables)
  hazard <- prepare_earthquake_hazard(pga, sa03, sa10, pgv)
  acceleration_units <- terra::units(hazard[c("PGA", "SA03", "SA10")])
  if (any(is.na(acceleration_units)) || any(acceleration_units != "g")) {
    stop(
      "The capacity-spectrum workflow currently requires PGA, SA03, and SA10 ",
      "in g. Convert acceleration rasters explicitly with convert_hazard_units().",
      call. = FALSE
    )
  }
  exposure <- extract_hazard(hazard, inventory)
  damage <- earthquake_damage(
    exposure, capacity_table, structural_fragility_table,
    drift_fragility_table, accel_fragility_table,
    magnitude = magnitude, allow_demo_tables = allow_demo_tables
  )
  loss <- if (is.null(loss_ratio_table)) NULL else earthquake_economic_loss(
    damage, loss_ratio_table, allow_demo_tables
  )
  final <- if (is.null(loss)) damage else loss
  summary <- if (is.null(group_fields)) NULL else summarize_by(final, group_fields)
  lookup_validation <- .collect_lookup_validation(tables)
  if (is.null(assumptions)) {
    assumptions <- hazgrid_assumptions(
      hazard_model = "earthquake",
      inventory_source = inventory_source,
      lookup_table_sources = .lookup_sources(lookup_validation),
      units = stats::setNames(terra::units(hazard), names(hazard)),
      earthquake_method = paste(
        "capacity_spectrum development implementation:",
        "elastic-demand intersection; full Hazus effective-damping iteration pending"
      )
    )
  }
  output_paths <- .write_workflow_outputs(
    "earthquake",
    list(exposure = exposure, damage = damage, loss = loss, summary = summary),
    inventory, final, output_dir, assumptions, lookup_validation
  )
  list(
    exposure = exposure, damage = damage, loss = loss, summary = summary,
    assumptions = assumptions, lookup_validation = lookup_validation,
    output_paths = output_paths
  )
}
