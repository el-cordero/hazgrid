#' Deprecated earthquake damage wrapper
#'
#' @param exposure Asset exposure table.
#' @param fragility_table Data.frame or CSV path containing `structure_type`,
#'   `design_level`, `ds`, `median`, `beta`, and `driver`.
#' @param driver Deprecated. The fallback driver comes from `fragility_table`.
#' @param allow_demo_tables Allow synthetic demo tables.
#'
#' @return See [earthquake_damage()].
#' @export
earthquake_damage_mvp <- function(
  exposure, fragility_table, driver = "PGA", allow_demo_tables = FALSE
) {
  .Deprecated("earthquake_damage")
  suppressWarnings(earthquake_damage(
    exposure = exposure, structural_fragility_table = fragility_table,
    method = "simple_fragility", allow_demo_tables = allow_demo_tables
  ))
}
