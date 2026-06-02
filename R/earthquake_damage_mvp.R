#' Calculate earthquake damage probabilities with an MVP model
#'
#' Applies user-supplied lognormal fragility curves to an earthquake hazard
#' driver. This is an MVP and does not implement the full Hazus capacity-spectrum
#' method.
#'
#' @param exposure Asset exposure table returned by [extract_hazard()].
#' @param fragility_table Data.frame or CSV path containing `structure_type`,
#'   `design_level`, `ds`, `median`, `beta`, and `driver`.
#' @param driver Earthquake hazard field to apply, normally `"PGA"`.
#'
#' @return Asset-level table with mutually exclusive damage probabilities.
#' @export
earthquake_damage_mvp <- function(exposure, fragility_table, driver = "PGA") {
  if (!is.data.frame(exposure)) {
    stop("exposure must be a data.frame.", call. = FALSE)
  }
  .require_columns(exposure, c("structure_type", "design_level", driver), "exposure")
  fragility <- .as_lookup_table(fragility_table, "fragility_table")
  .require_columns(
    fragility,
    c("structure_type", "design_level", "ds", "median", "beta", "driver"),
    "fragility_table"
  )
  table_drivers <- unique(as.character(fragility$driver))
  if (length(table_drivers) != 1L || table_drivers != driver) {
    stop("fragility_table rows must use driver '", driver, "'.", call. = FALSE)
  }
  states <- c("slight", "moderate", "extensive", "complete")
  parameters <- .fragility_parameters(exposure, fragility, states)
  probabilities <- damage_state_probabilities(
    exposure[[driver]],
    medians = parameters$medians,
    betas = parameters$betas,
    states = states
  )
  cbind(exposure, probabilities)
}
