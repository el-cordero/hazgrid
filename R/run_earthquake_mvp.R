#' Run the earthquake damage MVP workflow
#'
#' Runs inventory reading, earthquake hazard preparation, hazard extraction,
#' fragility-based damage probabilities, optional summaries, and optional
#' CSV/GeoPackage output writing. This is not the full Hazus capacity-spectrum
#' method.
#'
#' @param inventory A `terra::SpatVector` or inventory path.
#' @param pga Peak ground acceleration raster or path.
#' @param sa03 0.3-second spectral acceleration raster or path.
#' @param sa10 1.0-second spectral acceleration raster or path.
#' @param pgv Optional peak ground velocity raster or path.
#' @param fragility_table Data.frame or CSV path with earthquake fragility curves.
#' @param output_dir Optional output directory.
#' @param group_fields Optional fields passed to [summarize_by()].
#'
#' @return A list containing `exposure`, `damage`, `summary`, and `output_paths`.
#' @export
run_earthquake_mvp <- function(
  inventory, pga, sa03, sa10, pgv = NULL, fragility_table,
  output_dir = NULL, group_fields = NULL
) {
  inventory <- .read_inventory_input(inventory)
  hazard <- prepare_earthquake_hazard(pga = pga, sa03 = sa03, sa10 = sa10, pgv = pgv)
  exposure <- extract_hazard(hazard, inventory)
  damage <- earthquake_damage_mvp(exposure, fragility_table)
  summary <- if (is.null(group_fields)) NULL else summarize_by(damage, group_fields)
  output_paths <- .write_workflow_outputs(
    "earthquake",
    list(exposure = exposure, damage = damage, summary = summary),
    inventory, damage, output_dir
  )
  list(
    exposure = exposure,
    damage = damage,
    summary = summary,
    output_paths = output_paths
  )
}
