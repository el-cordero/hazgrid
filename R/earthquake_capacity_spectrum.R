#' Build an earthquake elastic demand spectrum
#'
#' Builds a transparent Hazus-style standard elastic response-spectrum shape
#' from PGA, 0.3-second spectral acceleration, and 1.0-second spectral
#' acceleration. Spectral displacement is returned in inches and spectral
#' acceleration in units of gravity. This function does not yet apply
#' magnitude-dependent effective damping or duration adjustments.
#'
#' @param pga Peak ground acceleration in units of gravity.
#' @param sa03 Spectral acceleration at 0.3 seconds in units of gravity.
#' @param sa10 Spectral acceleration at 1.0 second in units of gravity.
#' @param magnitude Optional earthquake magnitude, recorded for future damping
#'   and duration refinements.
#' @param periods Optional positive period vector in seconds.
#'
#' @return A data.frame containing period, spectral acceleration, and spectral
#'   displacement.
#' @export
build_demand_spectrum <- function(
  pga, sa03, sa10, magnitude = NULL, periods = NULL
) {
  values <- c(pga = pga, sa03 = sa03, sa10 = sa10)
  if (!is.numeric(values) || length(values) != 3L || any(!is.finite(values)) || any(values < 0)) {
    stop("pga, sa03, and sa10 must be finite non-negative scalar values.", call. = FALSE)
  }
  if (!is.null(magnitude) && (!is.numeric(magnitude) || length(magnitude) != 1L || magnitude <= 0)) {
    stop("magnitude must be a positive numeric scalar when supplied.", call. = FALSE)
  }
  if (is.null(periods)) {
    periods <- sort(unique(c(seq(0.01, 0.3, length.out = 30), seq(0.31, 1, length.out = 35), seq(1.05, 4, length.out = 60))))
  }
  if (!is.numeric(periods) || any(!is.finite(periods)) || any(periods <= 0)) {
    stop("periods must contain positive finite values.", call. = FALSE)
  }
  sa <- numeric(length(periods))
  short <- periods <= 0.3
  middle <- periods > 0.3 & periods <= 1
  long <- periods > 1
  sa[short] <- pga + (sa03 - pga) * periods[short] / 0.3
  sa[middle] <- sa03 + (sa10 - sa03) * (periods[middle] - 0.3) / 0.7
  sa[long] <- sa10 / periods[long]
  sd <- sa * 386.0886 * periods^2 / (4 * pi^2)
  output <- data.frame(
    period = periods,
    spectral_acceleration = sa,
    spectral_displacement = sd
  )
  attr(output, "magnitude") <- magnitude
  attr(output, "assumption") <- paste(
    "Elastic standard spectrum shape only.",
    "Effective damping and duration iteration remain under development."
  )
  output
}

#' Retrieve a building capacity curve
#'
#' Retrieves validated bilinear capacity parameters for one building type and
#' seismic design level. Capacity spectral displacement must use inches and
#' spectral acceleration must use units of gravity.
#'
#' @param capacity_table Validated earthquake capacity-curve table or path.
#' @param structure_type Building structure type.
#' @param design_level Seismic design level.
#' @param allow_demo_tables Allow synthetic demo tables. Never use this for
#'   real-world analysis.
#'
#' @return A named list of bilinear capacity parameters.
#' @export
capacity_curve <- function(
  capacity_table, structure_type, design_level, allow_demo_tables = FALSE
) {
  table <- .as_hazgrid_lookup(capacity_table, "earthquake_capacity_curve")
  .assert_lookup_allowed(table, allow_demo_tables)
  rows <- table[
    as.character(table$structure_type) == as.character(structure_type) &
      as.character(table$design_level) == as.character(design_level),
    , drop = FALSE
  ]
  if (nrow(rows) != 1L) {
    stop(
      "Capacity table must contain exactly one row for structure_type='",
      structure_type, "' and design_level='", design_level, "'.",
      call. = FALSE
    )
  }
  if (unique(rows$units) != "in|g") {
    stop("Capacity curve units must be 'in|g' for the current implementation.", call. = FALSE)
  }
  as.list(rows[1L, c("yield_sd", "yield_sa", "ultimate_sd", "ultimate_sa")])
}

.capacity_acceleration <- function(sd, curve) {
  elastic_slope <- curve$yield_sa / curve$yield_sd
  post_yield_slope <- (curve$ultimate_sa - curve$yield_sa) /
    (curve$ultimate_sd - curve$yield_sd)
  ifelse(
    sd <= curve$yield_sd,
    elastic_slope * sd,
    ifelse(
      sd <= curve$ultimate_sd,
      curve$yield_sa + post_yield_slope * (sd - curve$yield_sd),
      curve$ultimate_sa
    )
  )
}

#' Calculate an earthquake capacity-spectrum performance point
#'
#' Finds a numerical intersection between an elastic demand spectrum and a
#' bilinear building capacity curve. The current implementation is a transparent
#' first capacity-spectrum step. It warns because the complete Hazus effective
#' damping and iterative demand-reduction procedure is still under development.
#'
#' @param demand_spectrum Output from [build_demand_spectrum()].
#' @param capacity_curve Named capacity-curve parameter list.
#' @param method Performance-point method. Currently `"hazus"` only.
#'
#' @return A list containing performance spectral displacement and acceleration.
#' @export
performance_point <- function(
  demand_spectrum, capacity_curve, method = "hazus"
) {
  if (!identical(method, "hazus")) {
    stop("Only method = 'hazus' is currently supported.", call. = FALSE)
  }
  .require_columns(
    demand_spectrum,
    c("spectral_displacement", "spectral_acceleration"),
    "demand_spectrum"
  )
  required <- c("yield_sd", "yield_sa", "ultimate_sd", "ultimate_sa")
  if (!is.list(capacity_curve) || !all(required %in% names(capacity_curve))) {
    stop("capacity_curve must contain yield and ultimate Sd/Sa parameters.", call. = FALSE)
  }
  warning(
    "performance_point() currently uses the elastic-demand intersection. ",
    "The full Hazus effective-damping iteration is under development.",
    call. = FALSE
  )
  sd <- demand_spectrum$spectral_displacement
  demand_sa <- demand_spectrum$spectral_acceleration
  capacity_sa <- .capacity_acceleration(sd, capacity_curve)
  difference <- capacity_sa - demand_sa
  crossings <- which(difference[-length(difference)] * difference[-1L] <= 0)
  if (length(crossings) > 0L) {
    i <- crossings[[1L]]
    x <- sd[c(i, i + 1L)]
    y <- difference[c(i, i + 1L)]
    performance_sd <- if (diff(y) == 0) mean(x) else x[[1L]] - y[[1L]] * diff(x) / diff(y)
  } else {
    i <- which.min(abs(difference))
    performance_sd <- sd[[i]]
  }
  performance_sa <- .capacity_acceleration(performance_sd, capacity_curve)
  list(
    performance_sd = as.numeric(performance_sd),
    performance_sa = as.numeric(performance_sa),
    method = "hazus_elastic_intersection_development",
    intersection_found = length(crossings) > 0L
  )
}

.earthquake_component_damage <- function(
  exposure, fragility_table, table_type, driver, prefix, allow_demo_tables
) {
  table <- .as_hazgrid_lookup(fragility_table, table_type)
  .assert_lookup_allowed(table, allow_demo_tables)
  .require_columns(exposure, c("structure_type", "design_level", driver), "exposure")
  states <- c("slight", "moderate", "extensive", "complete")
  parameters <- .fragility_parameters(exposure, table, states)
  probabilities <- damage_state_probabilities(
    exposure[[driver]], parameters$medians, parameters$betas, states
  )
  names(probabilities) <- paste0(prefix, "_", names(probabilities))
  probabilities
}

#' Calculate earthquake structural damage probabilities
#'
#' Builds an elastic demand spectrum for each asset, retrieves its validated
#' bilinear capacity curve, calculates a performance point, and applies
#' spectral-displacement structural fragility curves.
#'
#' @param exposure Asset exposure table with `PGA`, `SA03`, and `SA10`.
#' @param capacity_table Validated earthquake capacity-curve table or path.
#' @param fragility_table Validated structural fragility table or path.
#' @param hazard_fields Names of PGA, SA03, and SA10 fields.
#' @param magnitude Optional earthquake magnitude.
#' @param allow_demo_tables Allow synthetic demo tables. Never use this for
#'   real-world analysis.
#'
#' @return Asset table with performance points and structural probabilities.
#' @export
earthquake_structural_damage <- function(
  exposure, capacity_table, fragility_table,
  hazard_fields = c("PGA", "SA03", "SA10"), magnitude = NULL,
  allow_demo_tables = FALSE
) {
  .require_columns(exposure, c("structure_type", "design_level", hazard_fields), "exposure")
  capacity_table <- .as_hazgrid_lookup(capacity_table, "earthquake_capacity_curve")
  fragility_table <- .as_hazgrid_lookup(fragility_table, "earthquake_structural_fragility")
  .assert_lookup_allowed(capacity_table, allow_demo_tables)
  .assert_lookup_allowed(fragility_table, allow_demo_tables)
  # TODO: implement the FEMA Hazus effective-damping and demand-reduction
  # iteration after its validated coefficient tables and equations are ingested.
  warning(
    "earthquake_structural_damage() currently uses elastic-demand ",
    "performance points. Full Hazus effective-damping iteration is under development.",
    call. = FALSE
  )
  performance <- lapply(seq_len(nrow(exposure)), function(i) {
    spectrum <- build_demand_spectrum(
      exposure[[hazard_fields[[1L]]]][[i]],
      exposure[[hazard_fields[[2L]]]][[i]],
      exposure[[hazard_fields[[3L]]]][[i]],
      magnitude = magnitude
    )
    curve <- capacity_curve(
      capacity_table, exposure$structure_type[[i]], exposure$design_level[[i]],
      allow_demo_tables = allow_demo_tables
    )
    suppressWarnings(performance_point(spectrum, curve))
  })
  output <- exposure
  output$performance_sd <- vapply(performance, `[[`, numeric(1), "performance_sd")
  output$performance_sa <- vapply(performance, `[[`, numeric(1), "performance_sa")
  output$performance_intersection_found <- vapply(performance, `[[`, logical(1), "intersection_found")
  probabilities <- .earthquake_component_damage(
    output, fragility_table, "earthquake_structural_fragility",
    "performance_sd", "structure", allow_demo_tables
  )
  cbind(output, probabilities)
}

#' Calculate earthquake nonstructural damage probabilities
#'
#' Calculates drift-sensitive damage from performance spectral displacement and
#' acceleration-sensitive damage from performance spectral acceleration.
#'
#' @param exposure Asset table with performance-point fields.
#' @param drift_fragility_table Optional validated drift-sensitive table.
#' @param accel_fragility_table Optional validated acceleration-sensitive table.
#' @param allow_demo_tables Allow synthetic demo tables. Never use this for
#'   real-world analysis.
#'
#' @return Asset table with separate nonstructural component probabilities.
#' @export
earthquake_nonstructural_damage <- function(
  exposure, drift_fragility_table = NULL, accel_fragility_table = NULL,
  allow_demo_tables = FALSE
) {
  output <- exposure
  if (!is.null(drift_fragility_table)) {
    probabilities <- .earthquake_component_damage(
      exposure, drift_fragility_table,
      "earthquake_nonstructural_drift_fragility",
      "performance_sd", "nonstructural_drift", allow_demo_tables
    )
    output <- cbind(output, probabilities)
  }
  if (!is.null(accel_fragility_table)) {
    probabilities <- .earthquake_component_damage(
      exposure, accel_fragility_table,
      "earthquake_nonstructural_acceleration_fragility",
      "performance_sa", "nonstructural_acceleration", allow_demo_tables
    )
    output <- cbind(output, probabilities)
  }
  output
}
