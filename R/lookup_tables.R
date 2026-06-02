.lookup_provenance_columns <- c(
  "source_name", "source_version", "source_table", "source_page",
  "validated_by", "validation_date", "validation_status", "units", "notes"
)

.lookup_statuses <- c(
  "official_fema_hazus", "independently_validated", "demo_only"
)

.lookup_specs <- list(
  tsunami_fragility = list(
    required = c("structure_type", "design_level", "component", "ds", "median", "beta", "driver"),
    keys = c("structure_type", "design_level", "component", "ds"),
    states = "ds",
    numeric_positive = c("median", "beta"),
    allowed_units = c("m", "ft", "m3/s2", "ft3/s2"),
    required_states = c("slight", "moderate", "extensive", "complete"),
    allowed = list(
      component = c("structure", "nonstructural", "contents"),
      driver = c("H", "HV2")
    )
  ),
  tsunami_loss_ratio = list(
    required = c("component", "damage_state", "loss_ratio"),
    keys = c("component", "damage_state"),
    states = "damage_state",
    numeric_ratio = "loss_ratio",
    allowed_units = "ratio",
    required_states = c("none", "slight", "moderate", "extensive", "complete"),
    allowed = list(component = c("structure", "nonstructural", "contents"))
  ),
  tsunami_casualty = list(
    required = c("min_depth", "max_depth", "fatality_rate", "injury_rate"),
    keys = c("min_depth", "max_depth"),
    numeric_ratio = c("fatality_rate", "injury_rate"),
    allowed_units = c("m", "ft")
  ),
  earthquake_capacity_curve = list(
    required = c(
      "structure_type", "design_level", "yield_sd", "yield_sa",
      "ultimate_sd", "ultimate_sa"
    ),
    keys = c("structure_type", "design_level"),
    numeric_positive = c("yield_sd", "yield_sa", "ultimate_sd", "ultimate_sa"),
    allowed_units = "in|g"
  ),
  earthquake_structural_fragility = list(
    required = c("structure_type", "design_level", "ds", "median", "beta", "driver"),
    keys = c("structure_type", "design_level", "ds"),
    states = "ds",
    numeric_positive = c("median", "beta"),
    allowed_units = c("in", "g"),
    required_states = c("slight", "moderate", "extensive", "complete"),
    allowed = list(driver = c("performance_sd", "PGA"))
  ),
  earthquake_nonstructural_drift_fragility = list(
    required = c("structure_type", "design_level", "ds", "median", "beta", "driver"),
    keys = c("structure_type", "design_level", "ds"),
    states = "ds",
    numeric_positive = c("median", "beta"),
    allowed_units = "in",
    required_states = c("slight", "moderate", "extensive", "complete"),
    allowed = list(driver = "performance_sd")
  ),
  earthquake_nonstructural_acceleration_fragility = list(
    required = c("structure_type", "design_level", "ds", "median", "beta", "driver"),
    keys = c("structure_type", "design_level", "ds"),
    states = "ds",
    numeric_positive = c("median", "beta"),
    allowed_units = "g",
    required_states = c("slight", "moderate", "extensive", "complete"),
    allowed = list(driver = "performance_sa")
  ),
  earthquake_loss_ratio = list(
    required = c("component", "damage_state", "loss_ratio"),
    keys = c("component", "damage_state"),
    states = "damage_state",
    numeric_ratio = "loss_ratio",
    allowed_units = "ratio",
    required_states = c("none", "slight", "moderate", "extensive", "complete"),
    allowed = list(
      component = c(
        "structure", "nonstructural_drift", "nonstructural_acceleration"
      )
    )
  ),
  earthquake_design_level_mapping = list(
    required = c("input_design_level", "design_level"),
    keys = "input_design_level",
    allowed_units = "not_applicable"
  ),
  earthquake_occupancy_mapping = list(
    required = c("occupancy", "occupancy_class"),
    keys = "occupancy",
    allowed_units = "not_applicable"
  ),
  restoration_function = list(
    required = c("component", "damage_state", "days", "recovery_fraction"),
    keys = c("component", "damage_state", "days"),
    states = "damage_state",
    numeric_nonnegative = "days",
    numeric_ratio = "recovery_fraction",
    allowed_units = "days|ratio"
  )
)

.lookup_spec <- function(table_type) {
  spec <- .lookup_specs[[table_type]]
  if (is.null(spec)) {
    stop(
      "Unsupported table_type. Choose one of: ",
      paste(names(.lookup_specs), collapse = ", "),
      call. = FALSE
    )
  }
  spec
}

.validate_lookup_allowed_values <- function(x, allowed, table_type) {
  if (is.null(allowed)) {
    return(invisible(TRUE))
  }
  for (column in names(allowed)) {
    invalid <- setdiff(unique(as.character(x[[column]])), allowed[[column]])
    if (length(invalid) > 0L) {
      stop(
        table_type, " has unsupported ", column, " value(s): ",
        paste(invalid, collapse = ", "),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.lookup_validation_report <- function(x, table_type) {
  data.frame(
    table_type = table_type,
    row_count = nrow(x),
    validation_status = paste(unique(x$validation_status), collapse = ";"),
    source_name = paste(unique(x$source_name), collapse = ";"),
    source_version = paste(unique(x$source_version), collapse = ";"),
    source_table = paste(unique(x$source_table), collapse = ";"),
    validated_by = paste(unique(x$validated_by), collapse = ";"),
    validation_date = paste(unique(x$validation_date), collapse = ";"),
    units = paste(unique(x$units), collapse = ";"),
    stringsAsFactors = FALSE
  )
}

#' Validate a hazgrid lookup table
#'
#' Validates the schema, keys, numeric ranges, units, and provenance metadata
#' for a typed lookup table. Validation confirms that the supplied table is
#' internally consistent; it does not independently certify the source values.
#'
#' @param x A data.frame lookup table.
#' @param table_type Supported hazgrid lookup-table type.
#' @param require_validated Require provenance metadata columns.
#'
#' @return `x` with class `hazgrid_lookup` and validation attributes.
#' @export
validate_lookup_table <- function(x, table_type, require_validated = TRUE) {
  if (!is.data.frame(x)) {
    stop("x must be a data.frame.", call. = FALSE)
  }
  spec <- .lookup_spec(table_type)
  required <- spec$required
  if (isTRUE(require_validated)) {
    required <- c(required, .lookup_provenance_columns)
  }
  .require_columns(x, required, table_type)
  if (nrow(x) == 0L) {
    stop(table_type, " must contain at least one data row.", call. = FALSE)
  }
  if (isTRUE(require_validated)) {
    missing_metadata <- vapply(
      x[.lookup_provenance_columns],
      function(column) any(is.na(column) | !nzchar(trimws(as.character(column)))),
      logical(1)
    )
    if (any(missing_metadata)) {
      stop(
        table_type, " has blank provenance metadata in: ",
        paste(names(missing_metadata)[missing_metadata], collapse = ", "),
        call. = FALSE
      )
    }
    invalid_status <- setdiff(unique(x$validation_status), .lookup_statuses)
    if (length(invalid_status) > 0L) {
      stop(
        "validation_status must be one of: ",
        paste(.lookup_statuses, collapse = ", "),
        call. = FALSE
      )
    }
    bad_dates <- is.na(as.Date(x$validation_date))
    if (any(bad_dates)) {
      stop("validation_date must use an ISO date such as 2026-06-02.", call. = FALSE)
    }
  }
  if (!is.null(spec$states)) {
    allowed_states <- c("none", "slight", "moderate", "extensive", "complete")
    invalid_states <- setdiff(unique(as.character(x[[spec$states]])), allowed_states)
    if (length(invalid_states) > 0L) {
      stop(
        table_type, " has unsupported damage state(s): ",
        paste(invalid_states, collapse = ", "),
        call. = FALSE
      )
    }
  }
  if (isTRUE(require_validated) && !is.null(spec$allowed_units)) {
    invalid_units <- setdiff(unique(as.character(x$units)), spec$allowed_units)
    if (length(invalid_units) > 0L) {
      stop(
        table_type, " has unsupported units value(s): ",
        paste(invalid_units, collapse = ", "),
        ". Allowed values: ", paste(spec$allowed_units, collapse = ", "),
        call. = FALSE
      )
    }
  }
  .validate_lookup_allowed_values(x, spec$allowed, table_type)
  if (!is.null(spec$keys)) {
    key <- do.call(paste, c(x[spec$keys], sep = "\r"))
    if (anyDuplicated(key)) {
      stop(table_type, " contains duplicate lookup key rows.", call. = FALSE)
    }
  }
  for (column in spec$numeric_positive) {
    if (!is.numeric(x[[column]]) || any(x[[column]] <= 0, na.rm = TRUE)) {
      stop(table_type, " column ", column, " must be positive numeric values.", call. = FALSE)
    }
  }
  for (column in spec$numeric_nonnegative) {
    if (!is.numeric(x[[column]]) || any(x[[column]] < 0, na.rm = TRUE)) {
      stop(table_type, " column ", column, " must be non-negative numeric values.", call. = FALSE)
    }
  }
  for (column in spec$numeric_ratio) {
    if (!is.numeric(x[[column]]) || any(x[[column]] < 0 | x[[column]] > 1, na.rm = TRUE)) {
      stop(table_type, " column ", column, " must be between zero and one.", call. = FALSE)
    }
  }
  if (!is.null(spec$states) && !is.null(spec$required_states)) {
    parent_keys <- setdiff(spec$keys, spec$states)
    parent <- if (length(parent_keys) == 0L) rep("all", nrow(x)) else
      do.call(paste, c(x[parent_keys], sep = "\r"))
    present <- split(as.character(x[[spec$states]]), parent)
    incomplete <- vapply(
      present,
      function(states) !setequal(states, spec$required_states),
      logical(1)
    )
    if (any(incomplete)) {
      stop(
        table_type, " must provide exactly these states for each lookup key: ",
        paste(spec$required_states, collapse = ", "),
        call. = FALSE
      )
    }
  }
  if (table_type == "tsunami_casualty") {
    if (any(x$max_depth <= x$min_depth)) {
      stop("Each tsunami casualty bin must have max_depth > min_depth.", call. = FALSE)
    }
    ordered <- x[order(x$min_depth), , drop = FALSE]
    if (nrow(ordered) > 1L && any(ordered$min_depth[-1L] < ordered$max_depth[-nrow(ordered)])) {
      stop("Tsunami casualty depth bins must not overlap.", call. = FALSE)
    }
  }
  if (table_type == "tsunami_fragility") {
    if (any(x$driver == "H" & !x$units %in% c("m", "ft")) ||
        any(x$driver == "HV2" & !x$units %in% c("m3/s2", "ft3/s2"))) {
      stop("Tsunami fragility units must match driver H or HV2.", call. = FALSE)
    }
  }
  if (table_type == "earthquake_structural_fragility") {
    if (any(x$driver == "performance_sd" & x$units != "in") ||
        any(x$driver == "PGA" & x$units != "g")) {
      stop("Earthquake structural fragility units must match its driver.", call. = FALSE)
    }
  }
  if (table_type == "tsunami_casualty" && length(unique(x$units)) != 1L) {
    stop("Tsunami casualty bins must use one consistent depth unit.", call. = FALSE)
  }
  if (table_type == "earthquake_capacity_curve") {
    if (any(x$ultimate_sd <= x$yield_sd) || any(x$ultimate_sa < x$yield_sa)) {
      stop(
        "Capacity curves require ultimate_sd > yield_sd and ultimate_sa >= yield_sa.",
        call. = FALSE
      )
    }
  }
  class(x) <- unique(c("hazgrid_lookup", class(x)))
  attr(x, "table_type") <- table_type
  attr(x, "lookup_validation") <- .lookup_validation_report(x, table_type)
  x
}

#' Read a typed hazgrid lookup table
#'
#' Reads and validates a CSV or RDS lookup table. Real analyses should use
#' tables with official FEMA Hazus provenance or independent validation.
#'
#' @param path CSV or RDS lookup-table path.
#' @param table_type Supported hazgrid lookup-table type.
#' @param require_validated Require provenance metadata columns.
#'
#' @return A validated `hazgrid_lookup` data.frame.
#' @export
read_hazgrid_lookup <- function(path, table_type, require_validated = TRUE) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("path must identify an existing CSV or RDS lookup table.", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(path))
  x <- switch(
    extension,
    csv = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    rds = readRDS(path),
    stop("Lookup tables must use CSV or RDS format.", call. = FALSE)
  )
  x <- validate_lookup_table(x, table_type, require_validated = require_validated)
  attr(x, "lookup_path") <- normalizePath(path, winslash = "/", mustWork = TRUE)
  x
}

.as_hazgrid_lookup <- function(x, table_type, require_validated = TRUE) {
  if (inherits(x, "hazgrid_lookup") && identical(attr(x, "table_type"), table_type)) {
    return(validate_lookup_table(x, table_type, require_validated))
  }
  if (is.character(x) && length(x) == 1L) {
    return(read_hazgrid_lookup(x, table_type, require_validated))
  }
  validate_lookup_table(x, table_type, require_validated)
}

.assert_lookup_allowed <- function(x, allow_demo_tables = FALSE) {
  statuses <- unique(as.character(x$validation_status))
  if ("demo_only" %in% statuses && !isTRUE(allow_demo_tables)) {
    stop(
      "Demo-only lookup tables are rejected by default. Supply official FEMA ",
      "Hazus or independently validated tables. For synthetic tests only, set ",
      "allow_demo_tables = TRUE.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.collect_lookup_validation <- function(tables) {
  tables <- tables[!vapply(tables, is.null, logical(1))]
  if (length(tables) == 0L) {
    return(data.frame())
  }
  do.call(rbind, lapply(tables, function(x) attr(x, "lookup_validation")))
}

.lookup_sources <- function(validation) {
  if (is.null(validation) || nrow(validation) == 0L) {
    return(character())
  }
  stats::setNames(
    paste(validation$source_name, validation$source_version, validation$source_table, sep = " | "),
    validation$table_type
  )
}
