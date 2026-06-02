.require_columns <- function(x, columns, what = "input") {
  missing <- setdiff(columns, names(x))
  if (length(missing) > 0L) {
    stop(
      what, " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.as_lookup_table <- function(x, what) {
  if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) {
      stop(what, " file does not exist: ", x, call. = FALSE)
    }
    return(utils::read.csv(x, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (!is.data.frame(x)) {
    stop(what, " must be a data.frame or CSV path.", call. = FALSE)
  }
  x
}

.as_raster_input <- function(x, what) {
  if (inherits(x, "SpatRaster")) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) {
      stop(what, " raster does not exist: ", x, call. = FALSE)
    }
    return(terra::rast(x))
  }
  stop(what, " must be a terra SpatRaster or raster path.", call. = FALSE)
}

.assert_single_layer <- function(x, what) {
  if (!inherits(x, "SpatRaster") || terra::nlyr(x) != 1L) {
    stop(what, " must be a single-layer terra SpatRaster.", call. = FALSE)
  }
  invisible(TRUE)
}

.assert_aligned_rasters <- function(rasters, labels = names(rasters)) {
  if (length(rasters) < 2L) {
    return(invisible(TRUE))
  }
  reference <- rasters[[1L]]
  for (i in seq.int(2L, length(rasters))) {
    if (!isTRUE(terra::compareGeom(
      reference, rasters[[i]],
      crs = TRUE, ext = TRUE, rowcol = TRUE, res = TRUE,
      stopOnError = FALSE
    ))) {
      stop(
        "Raster '", labels[[i]], "' is not aligned with raster '",
        labels[[1L]], "'. CRS, extent, resolution, and dimensions must match.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.assert_nonnegative <- function(x, what) {
  if (inherits(x, "SpatRaster")) {
    minimum <- terra::global(x, "min", na.rm = TRUE)[[1L]]
    if (!is.na(minimum) && minimum < 0) {
      stop(what, " must not contain negative values.", call. = FALSE)
    }
  } else if (any(x < 0, na.rm = TRUE)) {
    stop(what, " must not contain negative values.", call. = FALSE)
  }
  invisible(TRUE)
}

.crs_missing <- function(x) {
  value <- terra::crs(x, proj = TRUE)
  is.na(value) || !nzchar(value)
}

.read_inventory_input <- function(x) {
  if (inherits(x, "SpatVector")) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L) {
    return(read_inventory(x))
  }
  stop("inventory must be a terra SpatVector or a file path.", call. = FALSE)
}

.sum_available <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}

.mean_available <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

.write_workflow_outputs <- function(
  prefix, tables, inventory, asset_table, output_dir,
  assumptions = NULL, lookup_validation = NULL
) {
  if (is.null(output_dir)) {
    return(character())
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  paths <- character()
  for (table_name in names(tables)) {
    table <- tables[[table_name]]
    if (is.null(table)) {
      next
    }
    path <- file.path(output_dir, paste0(prefix, "_", table_name, ".csv"))
    utils::write.csv(table, path, row.names = FALSE)
    paths[[table_name]] <- path
  }
  vector_output <- inventory
  values <- terra::values(vector_output)
  append_names <- setdiff(names(asset_table), names(values))
  terra::values(vector_output) <- cbind(values, asset_table[append_names])
  vector_path <- file.path(output_dir, paste0(prefix, "_assets.gpkg"))
  terra::writeVector(vector_output, vector_path, overwrite = TRUE)
  paths[["assets_gpkg"]] <- vector_path
  if (!is.null(assumptions)) {
    assumptions_path <- file.path(output_dir, paste0(prefix, "_assumptions.json"))
    write_assumptions(assumptions, assumptions_path)
    paths[["assumptions"]] <- assumptions_path
  }
  if (!is.null(lookup_validation)) {
    lookup_path <- file.path(output_dir, paste0(prefix, "_lookup_validation.csv"))
    utils::write.csv(lookup_validation, lookup_path, row.names = FALSE)
    paths[["lookup_validation"]] <- lookup_path
  }
  paths
}
