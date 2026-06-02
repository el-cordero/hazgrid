.calculate_tsunami_component <- function(
  exposure, fragility, component, driver, states
) {
  subset <- fragility[fragility$component == component, , drop = FALSE]
  if (nrow(subset) == 0L) {
    warning("No ", component, " rows found in fragility_table.", call. = FALSE)
    probabilities <- matrix(
      NA_real_, nrow = nrow(exposure), ncol = length(states) + 1L,
      dimnames = list(NULL, paste0("p_", c("none", states)))
    )
    return(data.frame(probabilities, check.names = FALSE))
  }
  if (!driver %in% names(exposure)) {
    stop("exposure is missing tsunami hazard driver: ", driver, call. = FALSE)
  }
  if ("driver" %in% names(subset)) {
    table_drivers <- unique(as.character(subset$driver))
    if (length(table_drivers) != 1L || table_drivers != driver) {
      stop(
        component, " fragility rows must use driver '", driver, "'.",
        call. = FALSE
      )
    }
  }
  parameters <- .fragility_parameters(exposure, subset, states, component)
  damage_state_probabilities(
    exposure[[driver]],
    medians = parameters$medians,
    betas = parameters$betas,
    states = states
  )
}

#' Calculate tsunami component damage probabilities
#'
#' Uses momentum flux for structural damage and depth for nonstructural and
#' contents damage. The lookup table must contain `structure_type`,
#' `design_level`, `component`, `ds`, `median`, `beta`, and `driver` columns.
#'
#' @param exposure Asset exposure table returned by [extract_hazard()].
#' @param fragility_table Data.frame or CSV path containing fragility curves.
#' @param structural_driver Structural hazard field, normally `"HV2"`.
#' @param flood_driver Flood-like hazard field, normally `"H"`.
#' @param allow_demo_tables Allow synthetic demo tables. Never use this for
#'   real-world analysis.
#'
#' @return Asset-level table with component damage probabilities.
#' @export
tsunami_damage <- function(
  exposure, fragility_table,
  structural_driver = "HV2", flood_driver = "H", allow_demo_tables = FALSE
) {
  if (!is.data.frame(exposure)) {
    stop("exposure must be a data.frame.", call. = FALSE)
  }
  .require_columns(
    exposure,
    c("structure_type", "design_level", structural_driver, flood_driver),
    "exposure"
  )
  fragility <- .as_hazgrid_lookup(fragility_table, "tsunami_fragility")
  .assert_lookup_allowed(fragility, allow_demo_tables)
  states <- c("slight", "moderate", "extensive", "complete")
  components <- c(
    structure = structural_driver,
    nonstructural = flood_driver,
    contents = flood_driver
  )
  output <- exposure
  for (component in names(components)) {
    probabilities <- .calculate_tsunami_component(
      exposure, fragility, component, components[[component]], states
    )
    names(probabilities) <- paste0(component, "_", names(probabilities))
    output <- cbind(output, probabilities)
  }
  output
}
