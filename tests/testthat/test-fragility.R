test_that("lognormal exceedance is vectorized", {
  expect_equal(lognormal_exceedance(1, 1, 0.5), 0.5)
  expect_equal(lognormal_exceedance(c(0, 1, 2), 1, 0.5)[1:2], c(0, 0.5))
  expect_gt(lognormal_exceedance(2, 1, 0.5), 0.5)
  expect_error(lognormal_exceedance(1, 0, 0.5), "positive")
  expect_error(lognormal_exceedance(1, 1, 0), "positive")
})

test_that("damage-state probabilities are exclusive and sum to one", {
  probabilities <- damage_state_probabilities(
    c(0, 0.5, 1, 2),
    medians = c(0.25, 0.75, 1.5, 3),
    betas = rep(0.6, 4)
  )
  expect_named(
    probabilities,
    c("p_none", "p_slight", "p_moderate", "p_extensive", "p_complete")
  )
  expect_equal(rowSums(probabilities), rep(1, 4), tolerance = 1e-12)
  expect_true(all(as.matrix(probabilities) >= 0))
  expect_true(all(as.matrix(probabilities) <= 1))
})

test_that("damage-state probabilities accept asset-specific parameters", {
  medians <- rbind(c(0.25, 0.75, 1.5, 3), c(0.5, 1, 2, 4))
  probabilities <- damage_state_probabilities(
    c(1, 2), medians = medians, betas = matrix(0.6, nrow = 2, ncol = 4)
  )
  expect_equal(nrow(probabilities), 2)
  expect_equal(rowSums(probabilities), c(1, 1), tolerance = 1e-12)
})
