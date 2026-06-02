.earthquake_simple_fragility <- function(
  exposure, fragility_table, allow_demo_tables = FALSE
) {
  table <- .as_hazgrid_lookup(fragility_table, "earthquake_structural_fragility")
  .assert_lookup_allowed(table, allow_demo_tables)
  drivers <- unique(as.character(table$driver))
  if (length(drivers) != 1L || !drivers %in% names(exposure)) {
    stop("Simple fragility requires one table driver present in exposure.", call. = FALSE)
  }
  probabilities <- .earthquake_component_damage(
    exposure, table, "earthquake_structural_fragility",
    drivers, "structure", allow_demo_tables
  )
  cbind(exposure, probabilities)
}

#' Calculate earthquake damage probabilities
#'
#' Runs the earthquake capacity-spectrum damage framework. The default method
#' computes elastic demand spectra, bilinear-capacity performance points, and
#' structural/nonstructural fragility probabilities. Full Hazus effective
#' damping iteration remains under development and is reported explicitly.
#'
#' The `"simple_fragility"` method is retained only as an explicit fallback. It
#' is never selected silently.
#'
#' @param exposure Asset exposure table returned by [extract_hazard()].
#' @param capacity_table Validated earthquake capacity-curve table or path.
#' @param structural_fragility_table Validated structural fragility table.
#' @param drift_fragility_table Optional drift-sensitive fragility table.
#' @param accel_fragility_table Optional acceleration-sensitive fragility table.
#' @param method `"capacity_spectrum"` or explicit fallback `"simple_fragility"`.
#' @param magnitude Optional earthquake magnitude.
#' @param allow_demo_tables Allow synthetic demo tables. Never use this for
#'   real-world analysis.
#'
#' @return Asset-level earthquake damage table.
#' @export
earthquake_damage <- function(
  exposure, capacity_table = NULL, structural_fragility_table,
  drift_fragility_table = NULL, accel_fragility_table = NULL,
  method = c("capacity_spectrum", "simple_fragility"), magnitude = NULL,
  allow_demo_tables = FALSE
) {
  method <- match.arg(method)
  if (method == "simple_fragility") {
    warning(
      "Using explicitly requested simple_fragility earthquake fallback. ",
      "This is not the capacity-spectrum method.",
      call. = FALSE
    )
    return(.earthquake_simple_fragility(
      exposure, structural_fragility_table, allow_demo_tables
    ))
  }
  if (is.null(capacity_table)) {
    stop(
      "method = 'capacity_spectrum' requires a validated capacity_table. ",
      "No simplified PGA-only fallback is applied automatically.",
      call. = FALSE
    )
  }
  output <- earthquake_structural_damage(
    exposure, capacity_table, structural_fragility_table,
    magnitude = magnitude, allow_demo_tables = allow_demo_tables
  )
  earthquake_nonstructural_damage(
    output, drift_fragility_table, accel_fragility_table,
    allow_demo_tables = allow_demo_tables
  )
}

.earthquake_component_loss_ratio <- function(damage, ratios, component, prefix) {
  states <- c("none", "slight", "moderate", "extensive", "complete")
  probability_names <- paste0(prefix, "_p_", states)
  if (!all(probability_names %in% names(damage))) {
    return(rep(NA_real_, nrow(damage)))
  }
  subset <- ratios[ratios$component == component, , drop = FALSE]
  component_ratios <- subset$loss_ratio[match(states, subset$damage_state)]
  component_ratios[is.na(component_ratios)] <- 0
  rowSums(sweep(as.matrix(damage[probability_names]), 2L, component_ratios, `*`))
}

#' Calculate earthquake expected economic losses
#'
#' Converts structural and nonstructural component damage probabilities into
#' expected loss ratios and dollar losses using validated lookup tables.
#'
#' @param damage Asset damage table returned by [earthquake_damage()].
#' @param loss_ratio_table Validated earthquake loss-ratio table or path.
#' @param allow_demo_tables Allow synthetic demo tables. Never use this for
#'   real-world analysis.
#'
#' @return Asset-level earthquake expected loss table.
#' @export
earthquake_economic_loss <- function(
  damage, loss_ratio_table, allow_demo_tables = FALSE
) {
  ratios <- .as_hazgrid_lookup(loss_ratio_table, "earthquake_loss_ratio")
  .assert_lookup_allowed(ratios, allow_demo_tables)
  output <- damage
  output$structural_loss_ratio <- .earthquake_component_loss_ratio(
    damage, ratios, "structure", "structure"
  )
  output$nonstructural_drift_loss_ratio <- .earthquake_component_loss_ratio(
    damage, ratios, "nonstructural_drift", "nonstructural_drift"
  )
  output$nonstructural_acceleration_loss_ratio <- .earthquake_component_loss_ratio(
    damage, ratios, "nonstructural_acceleration", "nonstructural_acceleration"
  )
  output$structural_loss <- if ("replacement_value" %in% names(output)) {
    output$replacement_value * output$structural_loss_ratio
  } else NA_real_
  output$nonstructural_drift_loss <- if ("replacement_value" %in% names(output)) {
    output$replacement_value * output$nonstructural_drift_loss_ratio
  } else NA_real_
  output$nonstructural_acceleration_loss <- if ("replacement_value" %in% names(output)) {
    output$replacement_value * output$nonstructural_acceleration_loss_ratio
  } else NA_real_
  output$total_loss <- apply(
    output[c(
      "structural_loss", "nonstructural_drift_loss",
      "nonstructural_acceleration_loss"
    )],
    1L, .sum_available
  )
  output
}
