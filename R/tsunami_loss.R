.component_loss_ratio <- function(damage, loss_ratios, component, states) {
  subset <- loss_ratios[loss_ratios$component == component, , drop = FALSE]
  if (nrow(subset) == 0L) {
    return(rep(NA_real_, nrow(damage)))
  }
  if (anyDuplicated(subset$damage_state)) {
    stop("loss_ratio_table contains duplicate component/damage_state rows.", call. = FALSE)
  }
  ratios <- subset$loss_ratio[match(states, subset$damage_state)]
  ratios[is.na(ratios)] <- 0
  probabilities <- damage[paste0(component, "_p_", states)]
  rowSums(sweep(as.matrix(probabilities), 2L, ratios, `*`))
}

#' Calculate tsunami expected economic losses
#'
#' Converts component damage-state probabilities into expected loss ratios and
#' dollar losses. Structural and nonstructural losses use `replacement_value`;
#' contents losses use `contents_value`.
#'
#' @param damage Asset damage table returned by [tsunami_damage()].
#' @param loss_ratio_table Data.frame or CSV path containing `component`,
#'   `damage_state`, and `loss_ratio`.
#' @param allow_demo_tables Allow synthetic demo tables. Never use this for
#'   real-world analysis.
#'
#' @return Asset-level expected loss table.
#' @export
tsunami_economic_loss <- function(
  damage, loss_ratio_table, allow_demo_tables = FALSE
) {
  if (!is.data.frame(damage)) {
    stop("damage must be a data.frame.", call. = FALSE)
  }
  loss_ratios <- .as_hazgrid_lookup(loss_ratio_table, "tsunami_loss_ratio")
  .assert_lookup_allowed(loss_ratios, allow_demo_tables)
  states <- c("none", "slight", "moderate", "extensive", "complete")
  output <- damage
  output$structural_loss_ratio <- .component_loss_ratio(
    damage, loss_ratios, "structure", states
  )
  output$nonstructural_loss_ratio <- .component_loss_ratio(
    damage, loss_ratios, "nonstructural", states
  )
  output$contents_loss_ratio <- .component_loss_ratio(
    damage, loss_ratios, "contents", states
  )
  output$structural_loss <- if ("replacement_value" %in% names(output)) {
    output$replacement_value * output$structural_loss_ratio
  } else {
    NA_real_
  }
  output$nonstructural_loss <- if ("replacement_value" %in% names(output)) {
    output$replacement_value * output$nonstructural_loss_ratio
  } else {
    NA_real_
  }
  output$contents_loss <- if ("contents_value" %in% names(output)) {
    output$contents_value * output$contents_loss_ratio
  } else {
    NA_real_
  }
  output$total_loss <- apply(
    output[c("structural_loss", "nonstructural_loss", "contents_loss")],
    1L, .sum_available
  )
  output
}
