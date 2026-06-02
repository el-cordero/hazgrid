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
#' @param output_dir Optional output directory.
#' @param group_fields Optional fields passed to [summarize_by()].
#'
#' @return A list containing `exposure`, `damage`, `loss`, `summary`, and
#'   `output_paths`.
#' @export
run_tsunami_loss <- function(
  inventory, depth, velocity = NULL, momentum_flux = NULL,
  fragility_table, loss_ratio_table,
  output_dir = NULL, group_fields = NULL
) {
  inventory <- .read_inventory_input(inventory)
  hazard <- prepare_tsunami_hazard(
    depth = depth, velocity = velocity, momentum_flux = momentum_flux
  )
  exposure <- extract_hazard(hazard, inventory)
  damage <- tsunami_damage(exposure, fragility_table)
  loss <- tsunami_economic_loss(damage, loss_ratio_table)
  summary <- if (is.null(group_fields)) NULL else summarize_by(loss, group_fields)
  output_paths <- .write_workflow_outputs(
    "tsunami",
    list(exposure = exposure, damage = damage, loss = loss, summary = summary),
    inventory, loss, output_dir
  )
  list(
    exposure = exposure,
    damage = damage,
    loss = loss,
    summary = summary,
    output_paths = output_paths
  )
}
