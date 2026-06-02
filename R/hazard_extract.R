#' Read a building inventory
#'
#' Reads a spatial inventory from a GeoPackage, shapefile, GeoJSON, or CSV.
#' CSV files must include `x`/`y` columns or `lon`/`lat` columns. A CRS is
#' required for CSV `x`/`y` coordinates; CSV `lon`/`lat` coordinates default
#' to `"EPSG:4326"`.
#'
#' @param path Inventory file path.
#' @param crs Optional CRS used when the source does not provide one.
#'
#' @return A `terra::SpatVector`.
#' @export
read_inventory <- function(path, crs = NULL) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("path must identify an existing inventory file.", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(path))
  if (extension == "csv") {
    inventory <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    lower_names <- tolower(names(inventory))
    if (all(c("x", "y") %in% lower_names)) {
      geometry <- names(inventory)[match(c("x", "y"), lower_names)]
      if (is.null(crs)) {
        stop("crs is required for CSV x/y coordinates.", call. = FALSE)
      }
    } else if (all(c("lon", "lat") %in% lower_names)) {
      geometry <- names(inventory)[match(c("lon", "lat"), lower_names)]
      if (is.null(crs)) {
        crs <- "EPSG:4326"
      }
    } else {
      stop("CSV inventory must include x/y or lon/lat columns.", call. = FALSE)
    }
    return(terra::vect(inventory, geom = geometry, crs = crs))
  }
  if (!extension %in% c("gpkg", "shp", "geojson", "json")) {
    stop("Unsupported inventory format: .", extension, call. = FALSE)
  }
  inventory <- terra::vect(path)
  if (.crs_missing(inventory)) {
    if (is.null(crs)) {
      stop("Inventory CRS is missing; supply crs explicitly.", call. = FALSE)
    }
    terra::crs(inventory) <- crs
  }
  inventory
}

#' Extract raster hazard values to inventory assets
#'
#' Projects inventory geometry to the hazard CRS when needed, then extracts
#' raster values. Point assets use the requested extraction method. Polygon
#' assets use the mean of intersecting non-missing cells.
#'
#' @param hazard A `terra::SpatRaster`.
#' @param inventory A `terra::SpatVector`.
#' @param method Point extraction method passed to [terra::extract()].
#'
#' @return A data.frame containing inventory attributes and hazard values.
#' @export
extract_hazard <- function(hazard, inventory, method = "simple") {
  if (!inherits(hazard, "SpatRaster")) {
    stop("hazard must be a terra SpatRaster.", call. = FALSE)
  }
  if (!inherits(inventory, "SpatVector")) {
    stop("inventory must be a terra SpatVector.", call. = FALSE)
  }
  if (.crs_missing(hazard) || .crs_missing(inventory)) {
    stop("hazard and inventory must both have a CRS.", call. = FALSE)
  }
  projected_inventory <- inventory
  if (!isTRUE(terra::same.crs(hazard, inventory))) {
    projected_inventory <- terra::project(inventory, terra::crs(hazard))
  }
  attributes <- terra::values(projected_inventory)
  geometry_type <- tolower(terra::geomtype(projected_inventory))
  if (all(geometry_type == "points")) {
    values <- terra::extract(hazard, projected_inventory, method = method)
  } else if (all(geometry_type == "polygons")) {
    values <- terra::extract(
      hazard, projected_inventory,
      fun = mean, na.rm = TRUE
    )
  } else {
    stop("inventory must contain only points or only polygons.", call. = FALSE)
  }
  if ("ID" %in% names(values)) {
    values$ID <- NULL
  }
  data.frame(attributes, values, check.names = FALSE)
}
