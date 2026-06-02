#' Compute tsunami momentum flux
#'
#' Computes the Hazus-style tsunami structural damage driver `H * V^2`.
#' Inputs may be aligned single-layer rasters or numeric vectors. When unit
#' metadata is available, depth and velocity must use the same measurement
#' system and the result receives momentum-flux unit metadata.
#'
#' @param depth Inundation depth raster or numeric vector.
#' @param velocity Flow velocity raster or numeric vector.
#' @param name Output raster layer name.
#' @param auto_convert_units Convert metric velocity from `"cm/s"` to `"m/s"`.
#'
#' @return A raster or numeric vector containing `H * V^2`.
#' @export
compute_tsunami_momentum_flux <- function(
  depth, velocity, name = "HV2", auto_convert_units = FALSE
) {
  raster_mode <- inherits(depth, "SpatRaster") || inherits(velocity, "SpatRaster")
  if (raster_mode) {
    if (!inherits(depth, "SpatRaster") || !inherits(velocity, "SpatRaster")) {
      stop("depth and velocity must both be rasters or both be numeric.", call. = FALSE)
    }
    .assert_single_layer(depth, "depth")
    .assert_single_layer(velocity, "velocity")
    .assert_aligned_rasters(list(depth = depth, velocity = velocity))
  } else if (!is.numeric(depth) || !is.numeric(velocity)) {
    stop("depth and velocity must both be rasters or both be numeric.", call. = FALSE)
  }
  .assert_nonnegative(depth, "depth")
  .assert_nonnegative(velocity, "velocity")
  .validate_tsunami_units(
    depth, velocity = velocity, auto_convert_units = auto_convert_units
  )
  velocity_unit <- .normalize_unit(.get_units(velocity), "velocity")
  if (!is.na(velocity_unit) && velocity_unit == "cm/s") {
    velocity <- convert_hazard_units(velocity, "cm/s", "m/s", "velocity")
  }
  output <- depth * velocity^2
  if (raster_mode) {
    names(output) <- name
  }
  unit <- .momentum_flux_unit(depth, velocity)
  if (!is.na(unit)) {
    output <- .set_units(output, unit)
  }
  output
}

#' Prepare tsunami hazard layers
#'
#' Prepares Level 2 (`depth + velocity`) or Level 3 (`depth + momentum flux`)
#' tsunami hazard rasters. Structural damage uses momentum flux (`HV2`);
#' flood-like nonstructural and contents damage use depth (`H`).
#'
#' @param depth Single-layer inundation depth raster.
#' @param velocity Optional single-layer flow velocity raster.
#' @param momentum_flux Optional single-layer momentum-flux raster.
#' @param level One of `"auto"`, `"level2"`, or `"level3"`.
#' @param auto_convert_units Convert metric velocity from `"cm/s"` to `"m/s"`
#'   before computing momentum flux.
#'
#' @return A `terra::SpatRaster` with layers `H`, optional `V`, and `HV2`.
#' @export
prepare_tsunami_hazard <- function(
  depth, velocity = NULL, momentum_flux = NULL,
  level = c("auto", "level2", "level3"), auto_convert_units = FALSE
) {
  level <- match.arg(level)
  depth <- .as_raster_input(depth, "depth")
  .assert_single_layer(depth, "depth")
  if (!is.null(velocity)) {
    velocity <- .as_raster_input(velocity, "velocity")
    .assert_single_layer(velocity, "velocity")
  }
  if (!is.null(momentum_flux)) {
    momentum_flux <- .as_raster_input(momentum_flux, "momentum_flux")
    .assert_single_layer(momentum_flux, "momentum_flux")
  }
  if (!is.null(velocity) && !is.null(momentum_flux)) {
    stop("Supply velocity for Level 2 or momentum_flux for Level 3, not both.", call. = FALSE)
  }
  if (level == "auto") {
    if (!is.null(velocity)) {
      level <- "level2"
    } else if (!is.null(momentum_flux)) {
      level <- "level3"
    } else {
      stop("Supply velocity for Level 2 or momentum_flux for Level 3.", call. = FALSE)
    }
  }
  if (level == "level2" && is.null(velocity)) {
    stop("Level 2 tsunami hazard requires depth and velocity.", call. = FALSE)
  }
  if (level == "level3" && is.null(momentum_flux)) {
    stop("Level 3 tsunami hazard requires depth and momentum_flux.", call. = FALSE)
  }
  if (level == "level2") {
    .assert_aligned_rasters(list(depth = depth, velocity = velocity))
    momentum_flux <- compute_tsunami_momentum_flux(
      depth, velocity, auto_convert_units = auto_convert_units
    )
    velocity_unit <- .normalize_unit(.get_units(velocity), "velocity")
    if (!is.na(velocity_unit) && velocity_unit == "cm/s") {
      velocity <- convert_hazard_units(velocity, "cm/s", "m/s", "velocity")
    }
    names(depth) <- "H"
    names(velocity) <- "V"
    return(c(depth, velocity, momentum_flux))
  }
  .assert_aligned_rasters(list(depth = depth, momentum_flux = momentum_flux))
  .assert_nonnegative(depth, "depth")
  .assert_nonnegative(momentum_flux, "momentum_flux")
  .validate_tsunami_units(depth, momentum_flux = momentum_flux)
  names(depth) <- "H"
  names(momentum_flux) <- "HV2"
  c(depth, momentum_flux)
}
