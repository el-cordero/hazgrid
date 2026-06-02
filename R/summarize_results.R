#' Summarize asset-level workflow results
#'
#' Groups asset-level output by any fields present, such as municipality,
#' county, tract, block, or occupancy. Losses and expected casualties are
#' summed; common hazard fields are averaged.
#'
#' @param results Asset-level result data.frame.
#' @param group_fields Character vector of grouping fields. Use `character()`
#'   for a single overall summary row.
#'
#' @return A grouped summary data.frame.
#' @export
summarize_by <- function(results, group_fields) {
  if (!is.data.frame(results)) {
    stop("results must be a data.frame.", call. = FALSE)
  }
  if (!is.character(group_fields)) {
    stop("group_fields must be a character vector.", call. = FALSE)
  }
  .require_columns(results, group_fields, "results")
  sum_fields <- intersect(
    c(
      "structural_loss", "nonstructural_loss", "contents_loss", "total_loss",
      "nonstructural_drift_loss", "nonstructural_acceleration_loss",
      "expected_fatalities", "expected_injuries"
    ),
    names(results)
  )
  mean_fields <- intersect(c("H", "V", "HV2", "PGA", "SA03", "SA10", "PGV"), names(results))
  if (length(group_fields) == 0L) {
    output <- data.frame(asset_count = nrow(results))
    for (field in sum_fields) {
      output[[field]] <- .sum_available(results[[field]])
    }
    for (field in mean_fields) {
      output[[paste0("mean_", field)]] <- .mean_available(results[[field]])
    }
    return(output)
  }
  groups <- results[group_fields]
  for (field in group_fields) {
    groups[[field]] <- as.character(groups[[field]])
    groups[[field]][is.na(groups[[field]]) | !nzchar(groups[[field]])] <- "(missing)"
  }
  key <- interaction(groups, drop = TRUE, lex.order = TRUE)
  split_rows <- split(seq_len(nrow(results)), key)
  output <- do.call(rbind, lapply(split_rows, function(rows) {
    values <- groups[rows[[1L]], , drop = FALSE]
    values$asset_count <- length(rows)
    for (field in sum_fields) {
      values[[field]] <- .sum_available(results[[field]][rows])
    }
    for (field in mean_fields) {
      values[[paste0("mean_", field)]] <- .mean_available(results[[field]][rows])
    }
    values
  }))
  rownames(output) <- NULL
  output
}
