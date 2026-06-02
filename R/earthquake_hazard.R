.validate_earthquake_units <- function(raster, label, quantity = "acceleration") {
  unit <- .get_units(raster)
  if (is.na(unit)) {
    warning(
      label, " unit metadata is missing. Use terra::units() or ",
      "convert_hazard_units() to record units.",
      call. = FALSE
    )
    return(NA_character_)
  }
  .normalize_unit(unit, quantity)
}

#' Prepare earthquake hazard layers
#'
#' Combines aligned user-supplied ShakeMap-style or other earthquake hazard
#' rasters. This MVP does not implement legacy Hazus attenuation relationships.
#'
#' @param pga Single-layer peak ground acceleration raster.
#' @param sa03 Single-layer 0.3-second spectral acceleration raster.
#' @param sa10 Single-layer 1.0-second spectral acceleration raster.
#' @param pgv Optional single-layer peak ground velocity raster.
#'
#' @return A `terra::SpatRaster` with `PGA`, `SA03`, `SA10`, and optional `PGV`.
#' @export
prepare_earthquake_hazard <- function(pga, sa03, sa10, pgv = NULL) {
  pga <- .as_raster_input(pga, "pga")
  sa03 <- .as_raster_input(sa03, "sa03")
  sa10 <- .as_raster_input(sa10, "sa10")
  rasters <- list(PGA = pga, SA03 = sa03, SA10 = sa10)
  if (!is.null(pgv)) {
    rasters$PGV <- .as_raster_input(pgv, "pgv")
  }
  for (label in names(rasters)) {
    .assert_single_layer(rasters[[label]], tolower(label))
    .assert_nonnegative(rasters[[label]], tolower(label))
  }
  .assert_aligned_rasters(rasters)
  acceleration_units <- c(
    .validate_earthquake_units(rasters$PGA, "PGA"),
    .validate_earthquake_units(rasters$SA03, "SA03"),
    .validate_earthquake_units(rasters$SA10, "SA10")
  )
  if (length(unique(stats::na.omit(acceleration_units))) > 1L) {
    stop(
      "PGA, SA03, and SA10 units differ. Convert them explicitly with ",
      "convert_hazard_units().",
      call. = FALSE
    )
  }
  if (!is.null(rasters$PGV)) {
    .validate_earthquake_units(rasters$PGV, "PGV", quantity = "velocity")
  }
  names(rasters$PGA) <- "PGA"
  names(rasters$SA03) <- "SA03"
  names(rasters$SA10) <- "SA10"
  if (!is.null(rasters$PGV)) {
    names(rasters$PGV) <- "PGV"
  }
  terra::rast(unname(rasters))
}
