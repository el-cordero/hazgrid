#' Lognormal fragility exceedance probability
#'
#' Calculates the probability that a hazard intensity exceeds a lognormal
#' fragility threshold.
#'
#' @param x Numeric hazard intensity.
#' @param median Positive median fragility threshold.
#' @param beta Positive lognormal standard deviation.
#'
#' @return Numeric probability of exceedance. Values of `x <= 0` return zero.
#' @export
lognormal_exceedance <- function(x, median, beta) {
  if (!is.numeric(x) || !is.numeric(median) || !is.numeric(beta)) {
    stop("x, median, and beta must be numeric.", call. = FALSE)
  }
  if (any(median <= 0, na.rm = TRUE)) {
    stop("median must contain only positive values.", call. = FALSE)
  }
  if (any(beta <= 0, na.rm = TRUE)) {
    stop("beta must contain only positive values.", call. = FALSE)
  }
  probability <- stats::pnorm(log(x / median) / beta)
  probability[!is.na(x) & x <= 0] <- 0
  probability
}

.parameter_matrix <- function(x, n, states, what) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (is.matrix(x)) {
    if (ncol(x) != length(states)) {
      stop(what, " must have one column per damage state.", call. = FALSE)
    }
    if (!is.null(colnames(x)) && all(states %in% colnames(x))) {
      x <- x[, states, drop = FALSE]
    }
    if (nrow(x) == 1L && n > 1L) {
      x <- x[rep(1L, n), , drop = FALSE]
    }
    if (nrow(x) != n) {
      stop(what, " must have one row or one row per asset.", call. = FALSE)
    }
    storage.mode(x) <- "double"
    return(x)
  }
  if (!is.numeric(x) || length(x) != length(states)) {
    stop(what, " must provide one numeric value per damage state.", call. = FALSE)
  }
  matrix(rep(x, each = n), nrow = n, dimnames = list(NULL, states))
}

#' Mutually exclusive damage-state probabilities
#'
#' Converts lognormal damage-state exceedance curves into mutually exclusive
#' damage-state probabilities.
#'
#' @param x Numeric hazard intensity vector.
#' @param medians Numeric vector with one value per state, or a matrix/data.frame
#'   with one row per asset and one column per state.
#' @param betas Numeric vector with one value per state, or a matrix/data.frame
#'   with one row per asset and one column per state.
#' @param states Ordered damage-state names, from least to most severe.
#'
#' @return A data.frame with `p_none` and one probability column per state.
#' @export
damage_state_probabilities <- function(
  x, medians, betas,
  states = c("slight", "moderate", "extensive", "complete")
) {
  if (!is.numeric(x)) {
    stop("x must be numeric.", call. = FALSE)
  }
  if (length(states) < 1L || anyDuplicated(states)) {
    stop("states must contain unique ordered damage-state names.", call. = FALSE)
  }
  n <- length(x)
  medians <- .parameter_matrix(medians, n, states, "medians")
  betas <- .parameter_matrix(betas, n, states, "betas")
  if (any(medians <= 0, na.rm = TRUE)) {
    stop("medians must contain only positive values.", call. = FALSE)
  }
  if (any(betas <= 0, na.rm = TRUE)) {
    stop("betas must contain only positive values.", call. = FALSE)
  }

  exceedance <- vapply(
    seq_along(states),
    function(i) lognormal_exceedance(x, medians[, i], betas[, i]),
    numeric(n)
  )
  exceedance <- matrix(exceedance, nrow = n, ncol = length(states))
  colnames(exceedance) <- states
  if (
    nrow(exceedance) > 0L && ncol(exceedance) > 1L &&
    any(exceedance[, -ncol(exceedance), drop = FALSE] + 1e-12 <
      exceedance[, -1L, drop = FALSE], na.rm = TRUE)
  ) {
    stop(
      "Fragility exceedance probabilities are not ordered from least to ",
      "most severe state. Check medians and betas.",
      call. = FALSE
    )
  }

  output <- matrix(NA_real_, nrow = n, ncol = length(states) + 1L)
  colnames(output) <- paste0("p_", c("none", states))
  if (n > 0L) {
    output[, 1L] <- 1 - exceedance[, 1L]
    if (length(states) > 1L) {
      for (i in seq_len(length(states) - 1L)) {
        output[, i + 1L] <- exceedance[, i] - exceedance[, i + 1L]
      }
    }
    output[, ncol(output)] <- exceedance[, ncol(exceedance)]
  }
  output[] <- pmin(1, pmax(0, output))
  data.frame(output, check.names = FALSE)
}

.fragility_parameters <- function(exposure, fragility, states, component = NULL) {
  subset <- fragility
  if (!is.null(component)) {
    subset <- subset[subset$component == component, , drop = FALSE]
  }
  if (nrow(subset) == 0L) {
    return(NULL)
  }
  subset$structure_type <- as.character(subset$structure_type)
  subset$design_level <- as.character(subset$design_level)
  subset$ds <- as.character(subset$ds)
  exposure_key <- paste(
    as.character(exposure$structure_type),
    as.character(exposure$design_level),
    sep = "\r"
  )
  medians <- matrix(NA_real_, nrow = nrow(exposure), ncol = length(states))
  betas <- medians
  colnames(medians) <- colnames(betas) <- states
  for (i in seq_along(states)) {
    state_rows <- subset[subset$ds == states[[i]], , drop = FALSE]
    state_key <- paste(state_rows$structure_type, state_rows$design_level, sep = "\r")
    if (anyDuplicated(state_key)) {
      stop("Fragility table contains duplicate lookup rows.", call. = FALSE)
    }
    index <- match(exposure_key, state_key)
    if (anyNA(index)) {
      stop(
        "Fragility table has no ", states[[i]], " row for one or more ",
        "structure_type/design_level combinations.",
        call. = FALSE
      )
    }
    medians[, i] <- state_rows$median[index]
    betas[, i] <- state_rows$beta[index]
  }
  list(medians = medians, betas = betas)
}
