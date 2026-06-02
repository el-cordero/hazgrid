.unit_aliases <- list(
  depth = list(
    m = c("m", "meter", "meters", "metre", "metres"),
    ft = c("ft", "foot", "feet")
  ),
  velocity = list(
    "m/s" = c("m/s", "mps", "meter/s", "meters/s", "metre/s", "metres/s"),
    "cm/s" = c("cm/s", "cmps", "centimeter/s", "centimeters/s"),
    "ft/s" = c("ft/s", "fps", "foot/s", "feet/s")
  ),
  acceleration = list(
    g = c("g"),
    "m/s2" = c("m/s2", "m/s^2"),
    "cm/s2" = c("cm/s2", "cm/s^2"),
    "ft/s2" = c("ft/s2", "ft/s^2")
  ),
  momentum_flux = list(
    "m3/s2" = c("m3/s2", "m^3/s^2"),
    "ft3/s2" = c("ft3/s2", "ft^3/s^2")
  )
)

.normalize_unit <- function(unit, quantity) {
  if (length(unit) != 1L || is.na(unit) || !nzchar(unit)) {
    return(NA_character_)
  }
  aliases <- .unit_aliases[[quantity]]
  if (is.null(aliases)) {
    stop("Unsupported unit quantity: ", quantity, call. = FALSE)
  }
  unit <- tolower(trimws(unit))
  for (canonical in names(aliases)) {
    if (unit %in% tolower(aliases[[canonical]])) {
      return(canonical)
    }
  }
  stop("Unsupported ", quantity, " unit: ", unit, call. = FALSE)
}

.get_units <- function(x) {
  if (inherits(x, "SpatRaster")) {
    value <- terra::units(x)
    if (length(value) == 0L || is.na(value[[1L]]) || !nzchar(value[[1L]])) {
      return(NA_character_)
    }
    return(value[[1L]])
  }
  value <- attr(x, "units", exact = TRUE)
  if (is.null(value) || length(value) == 0L) {
    return(NA_character_)
  }
  as.character(value[[1L]])
}

.set_units <- function(x, unit) {
  if (inherits(x, "SpatRaster")) {
    terra::units(x) <- unit
    return(x)
  }
  attr(x, "units") <- unit
  x
}

.unit_factor <- function(from, to, quantity) {
  factors <- switch(
    quantity,
    depth = c(m = 1, ft = 0.3048),
    velocity = c("m/s" = 1, "cm/s" = 0.01, "ft/s" = 0.3048),
    acceleration = c(g = 9.80665, "m/s2" = 1, "cm/s2" = 0.01, "ft/s2" = 0.3048),
    momentum_flux = c("m3/s2" = 1, "ft3/s2" = 0.3048^3),
    stop("Unsupported unit quantity: ", quantity, call. = FALSE)
  )
  unname(factors[[from]] / factors[[to]])
}

#' Convert hazard units explicitly
#'
#' Converts numeric vectors or `terra::SpatRaster` objects between supported
#' units. Use this helper before combining hazard layers whose units differ.
#'
#' Supported quantities and canonical units are: depth (`"m"`, `"ft"`),
#' velocity (`"m/s"`, `"cm/s"`, `"ft/s"`), acceleration (`"g"`, `"m/s2"`, `"cm/s2"`,
#' `"ft/s2"`), and momentum flux (`"m3/s2"`, `"ft3/s2"`).
#'
#' @param x Numeric vector or `terra::SpatRaster`.
#' @param from Source unit.
#' @param to Target unit.
#' @param quantity One of `"depth"`, `"velocity"`, `"acceleration"`, or
#'   `"momentum_flux"`.
#'
#' @return Converted object with unit metadata.
#' @export
convert_hazard_units <- function(
  x, from, to,
  quantity = c("depth", "velocity", "acceleration", "momentum_flux")
) {
  quantity <- match.arg(quantity)
  if (!is.numeric(x) && !inherits(x, "SpatRaster")) {
    stop("x must be numeric or a terra SpatRaster.", call. = FALSE)
  }
  from <- .normalize_unit(from, quantity)
  to <- .normalize_unit(to, quantity)
  .set_units(x * .unit_factor(from, to, quantity), to)
}

.validate_tsunami_units <- function(depth, velocity = NULL, momentum_flux = NULL) {
  depth_unit <- .normalize_unit(.get_units(depth), "depth")
  if (is.na(depth_unit)) {
    warning(
      "Depth unit metadata is missing. Use terra::units() or ",
      "convert_hazard_units() to record units.",
      call. = FALSE
    )
  }
  if (!is.null(velocity)) {
    velocity_unit <- .normalize_unit(.get_units(velocity), "velocity")
    if (is.na(velocity_unit)) {
      warning(
        "Velocity unit metadata is missing. Use terra::units() or ",
        "convert_hazard_units() to record units.",
        call. = FALSE
      )
    }
    if (
      !is.na(depth_unit) && !is.na(velocity_unit) &&
      ((depth_unit == "m") != (velocity_unit == "m/s"))
    ) {
      stop(
        "Depth and velocity units are not directly compatible. ",
        "Convert them explicitly with convert_hazard_units().",
        call. = FALSE
      )
    }
  }
  if (!is.null(momentum_flux)) {
    flux_unit <- .normalize_unit(.get_units(momentum_flux), "momentum_flux")
    if (is.na(flux_unit)) {
      warning(
        "Momentum-flux unit metadata is missing. Use terra::units() or ",
        "convert_hazard_units() to record units.",
        call. = FALSE
      )
    }
    if (
      !is.na(depth_unit) && !is.na(flux_unit) &&
      ((depth_unit == "m") != (flux_unit == "m3/s2"))
    ) {
      stop(
        "Depth and momentum-flux units use different measurement systems. ",
        "Convert them explicitly with convert_hazard_units().",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.momentum_flux_unit <- function(depth, velocity) {
  depth_unit <- .normalize_unit(.get_units(depth), "depth")
  velocity_unit <- .normalize_unit(.get_units(velocity), "velocity")
  if (is.na(depth_unit) || is.na(velocity_unit)) {
    return(NA_character_)
  }
  if (depth_unit == "m") "m3/s2" else "ft3/s2"
}
