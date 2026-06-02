#' Calculate tsunami casualty estimates with an MVP model
#'
#' Applies a simple user-supplied depth-bin model to estimate expected
#' fatalities and injuries. This is a placeholder interface for future work,
#' not the complete Hazus tsunami casualty model. Depth thresholds must use the
#' same units as the exposure depth field.
#'
#' @param exposure Asset exposure table.
#' @param casualty_params Optional data.frame or CSV path with `min_depth`,
#'   `max_depth`, `fatality_rate`, and `injury_rate`. When `NULL`, conservative
#'   demo parameters are used with a warning.
#' @param population_field Population field in `exposure`.
#' @param depth_field Depth field in `exposure`.
#'
#' @return Asset-level table with expected fatalities and injuries.
#' @export
tsunami_casualty_mvp <- function(
  exposure, casualty_params = NULL,
  population_field = "population_day", depth_field = "H"
) {
  if (!is.data.frame(exposure)) {
    stop("exposure must be a data.frame.", call. = FALSE)
  }
  .require_columns(exposure, c(population_field, depth_field), "exposure")
  if (is.null(casualty_params)) {
    warning(
      "Using demo-only casualty parameters. Supply validated parameters for analysis.",
      call. = FALSE
    )
    casualty_params <- data.frame(
      min_depth = c(-Inf, 0.5, 2, 5),
      max_depth = c(0.5, 2, 5, Inf),
      fatality_rate = c(0, 0.001, 0.01, 0.05),
      injury_rate = c(0, 0.01, 0.05, 0.15)
    )
  } else {
    casualty_params <- .as_lookup_table(casualty_params, "casualty_params")
  }
  .require_columns(
    casualty_params,
    c("min_depth", "max_depth", "fatality_rate", "injury_rate"),
    "casualty_params"
  )
  casualty_params <- casualty_params[order(casualty_params$min_depth), , drop = FALSE]
  if (any(casualty_params$max_depth <= casualty_params$min_depth)) {
    stop("Each casualty depth bin must have max_depth > min_depth.", call. = FALSE)
  }
  if (
    nrow(casualty_params) > 1L &&
    any(casualty_params$min_depth[-1L] < casualty_params$max_depth[-nrow(casualty_params)])
  ) {
    stop("Casualty depth bins must not overlap.", call. = FALSE)
  }
  rates <- casualty_params[c("fatality_rate", "injury_rate")]
  if (any(as.matrix(rates) < 0 | as.matrix(rates) > 1, na.rm = TRUE)) {
    stop("Casualty rates must be between zero and one.", call. = FALSE)
  }
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
