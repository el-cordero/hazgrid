#' Calculate tsunami casualty estimates
#'
#' Applies a validated user-supplied depth-bin casualty table to estimate
#' expected fatalities and injuries. The method is intentionally isolated so
#' validated casualty methods can evolve without changing hazard extraction.
#' Depth thresholds must use the same units as the exposure depth field.
#'
#' @param exposure Asset exposure table.
#' @param casualty_table Data.frame or CSV path with `min_depth`, `max_depth`,
#'   `fatality_rate`, and `injury_rate`.
#' @param population_field Population field in `exposure`.
#' @param depth_field Depth field in `exposure`.
#' @param allow_demo_tables Allow synthetic demo tables. Never use this for
#'   real-world analysis.
#'
#' @return Asset-level table with expected fatalities and injuries.
#' @export
tsunami_casualty <- function(
  exposure, casualty_table,
  population_field = "population_day", depth_field = "H",
  allow_demo_tables = FALSE
) {
  if (!is.data.frame(exposure)) {
    stop("exposure must be a data.frame.", call. = FALSE)
  }
  .require_columns(exposure, c(population_field, depth_field), "exposure")
  casualty_params <- .as_hazgrid_lookup(casualty_table, "tsunami_casualty")
  .assert_lookup_allowed(casualty_params, allow_demo_tables)
  casualty_params <- casualty_params[order(casualty_params$min_depth), , drop = FALSE]
  depth <- exposure[[depth_field]]
  index <- findInterval(depth, casualty_params$min_depth)
  index[index < 1L] <- NA_integer_
  outside <- !is.na(index) & depth >= casualty_params$max_depth[index]
  index[outside] <- NA_integer_
  output <- exposure
  output$expected_fatalities <- exposure[[population_field]] *
    casualty_params$fatality_rate[index]
  output$expected_injuries <- exposure[[population_field]] *
    casualty_params$injury_rate[index]
  output
}

#' Deprecated tsunami casualty wrapper
#'
#' @inheritParams tsunami_casualty
#' @param casualty_params Deprecated alias for `casualty_table`.
#'
#' @return See [tsunami_casualty()].
#' @export
tsunami_casualty_mvp <- function(
  exposure, casualty_params,
  population_field = "population_day", depth_field = "H",
  allow_demo_tables = FALSE
) {
  .Deprecated("tsunami_casualty")
  tsunami_casualty(
    exposure = exposure, casualty_table = casualty_params,
    population_field = population_field, depth_field = depth_field,
    allow_demo_tables = allow_demo_tables
  )
}
