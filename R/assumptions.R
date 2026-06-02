#' Create a hazgrid assumptions record
#'
#' Creates a structured record of analysis assumptions and input sources.
#'
#' @param hazard_model Hazard model name.
#' @param hazard_source Hazard input source.
#' @param inventory_source Inventory source.
#' @param lookup_table_sources Named lookup-table source character vector.
#' @param units Named unit character vector.
#' @param analysis_level Analysis level, where applicable.
#' @param earthquake_method Earthquake method, where applicable.
#' @param tsunami_method Tsunami method, where applicable.
#' @param casualty_method Casualty method, where applicable.
#' @param date_created Record creation date and time.
#' @param notes Additional notes.
#'
#' @return A `hazgrid_assumptions` list.
#' @export
hazgrid_assumptions <- function(
  hazard_model = NULL, hazard_source = NULL, inventory_source = NULL,
  lookup_table_sources = character(), units = character(),
  analysis_level = NULL, earthquake_method = NULL, tsunami_method = NULL,
  casualty_method = NULL, date_created = Sys.time(), notes = NULL
) {
  structure(
    list(
      hazard_model = hazard_model,
      hazard_source = hazard_source,
      inventory_source = inventory_source,
      lookup_table_sources = lookup_table_sources,
      units = units,
      analysis_level = analysis_level,
      earthquake_method = earthquake_method,
      tsunami_method = tsunami_method,
      casualty_method = casualty_method,
      date_created = format(date_created, tz = "UTC", usetz = TRUE),
      notes = notes
    ),
    class = "hazgrid_assumptions"
  )
}

.json_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\n", "\\\\n", x)
  x
}

.json_value <- function(x, indent = 0L) {
  if (is.null(x)) {
    return("null")
  }
  if (length(x) == 0L) {
    return("[]")
  }
  if (is.list(x) || (!is.null(names(x)) && any(nzchar(names(x))))) {
    if (is.null(names(x))) {
      return(paste0("[", paste(vapply(x, .json_value, character(1), indent = indent + 2L), collapse = ", "), "]"))
    }
    entries <- paste0(
      "\"", .json_escape(names(x)), "\": ",
      vapply(unname(x), .json_value, character(1), indent = indent + 2L)
    )
    return(paste0("{\n", strrep(" ", indent + 2L), paste(entries, collapse = paste0(",\n", strrep(" ", indent + 2L))), "\n", strrep(" ", indent), "}"))
  }
  if (length(x) > 1L) {
    return(paste0("[", paste(vapply(as.list(x), .json_value, character(1), indent = indent), collapse = ", "), "]"))
  }
  if (is.na(x)) {
    return("null")
  }
  if (is.logical(x)) {
    return(tolower(as.character(x)))
  }
  if (is.numeric(x)) {
    return(as.character(x))
  }
  paste0("\"", .json_escape(as.character(x)), "\"")
}

#' Write an assumptions record
#'
#' Writes a [hazgrid_assumptions()] object as JSON or a readable text file.
#'
#' @param assumptions A `hazgrid_assumptions` list.
#' @param path Output `.json` or text file path.
#'
#' @return `path`, invisibly.
#' @export
write_assumptions <- function(assumptions, path) {
  if (!inherits(assumptions, "hazgrid_assumptions")) {
    stop("assumptions must be created by hazgrid_assumptions().", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(path))
  if (extension == "json") {
    writeLines(.json_value(unclass(assumptions)), path, useBytes = TRUE)
  } else {
    lines <- unlist(lapply(names(assumptions), function(name) {
      value <- assumptions[[name]]
      if (length(value) == 0L || is.null(value)) {
        value <- ""
      } else if (!is.null(names(value))) {
        value <- paste(paste(names(value), value, sep = "="), collapse = "; ")
      } else {
        value <- paste(value, collapse = "; ")
      }
      paste0(name, ": ", value)
    }))
    writeLines(lines, path, useBytes = TRUE)
  }
  invisible(path)
}
