#' Deprecated earthquake workflow wrapper
#'
#' @param inventory A `terra::SpatVector` or inventory path.
#' @param pga Peak ground acceleration raster or path.
#' @param sa03 0.3-second spectral acceleration raster or path.
#' @param sa10 1.0-second spectral acceleration raster or path.
#' @param pgv Optional peak ground velocity raster or path.
#' @param fragility_table Deprecated simple-fragility table.
#' @param output_dir Optional output directory.
#' @param group_fields Optional fields passed to [summarize_by()].
#'
#' @param allow_demo_tables Allow synthetic demo tables.
#'
#' @return A list containing exposure, damage, summary, and output paths.
#' @export
run_earthquake_mvp <- function(
  inventory, pga, sa03, sa10, pgv = NULL, fragility_table,
  output_dir = NULL, group_fields = NULL, allow_demo_tables = FALSE
) {
  .Deprecated("run_earthquake_loss")
  inventory_source <- if (is.character(inventory)) inventory else "terra::SpatVector"
  inventory <- .read_inventory_input(inventory)
  hazard <- prepare_earthquake_hazard(pga = pga, sa03 = sa03, sa10 = sa10, pgv = pgv)
  exposure <- extract_hazard(hazard, inventory)
  damage <- suppressWarnings(earthquake_damage(
    exposure, structural_fragility_table = fragility_table,
    method = "simple_fragility", allow_demo_tables = allow_demo_tables
  ))
  summary <- if (is.null(group_fields)) NULL else summarize_by(damage, group_fields)
  table <- .as_hazgrid_lookup(fragility_table, "earthquake_structural_fragility")
  lookup_validation <- .collect_lookup_validation(list(table))
  assumptions <- hazgrid_assumptions(
    hazard_model = "earthquake", inventory_source = inventory_source,
    lookup_table_sources = .lookup_sources(lookup_validation),
    units = stats::setNames(terra::units(hazard), names(hazard)),
    earthquake_method = "deprecated explicit simple_fragility fallback"
  )
  output_paths <- .write_workflow_outputs(
    "earthquake",
    list(exposure = exposure, damage = damage, summary = summary),
    inventory, damage, output_dir, assumptions, lookup_validation
  )
  list(
    exposure = exposure,
    damage = damage,
    summary = summary,
    assumptions = assumptions,
    lookup_validation = lookup_validation,
    output_paths = output_paths
  )
}
