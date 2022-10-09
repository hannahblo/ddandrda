testthat::test_that("calculate_nominal_scaling_vec works", {
  attr_nominal_4 <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1"))
  nominal_context <- array(0, c(4, 2))
  nominal_context[, 1] <- c(1, 0, 0, 1)
  nominal_context[, 2] <- c(0, 1, 1, 0)
  colnames(nominal_context) <- c("nominal: factor_1", "nominal: factor_2")
  testthat::expect_equal(
    compute_nominal_scaling_vec(
      attr_nominal_4,
      "nominal"
    ),
    nominal_context
  )
})

testthat::test_that("calculate_ordinal_scaling_vec works", {
  attr_numeric_4 <- as.factor(c(1.2, 1, 1.6, 2))
  ordinal_context <- array(0, c(4, 4))
  ordinal_context[, 1] <- c(0, 1, 0, 0)
  ordinal_context[, 2] <- c(1, 1, 0, 0)
  ordinal_context[, 3] <- c(1, 1, 1, 0)
  ordinal_context[, 4] <- c(1, 1, 1, 1)
  colnames(ordinal_context) <- c(
    "numeric: x<=1", "numeric: x<=1.2",
    "numeric: x<=1.6", "numeric: x<=2"
  )
  testthat::expect_equal(
    compute_ordinal_scaling_vec(
      attr_numeric_4,
      "numeric"
    ),
    ordinal_context
  )
})

testthat::test_that("calculate_dual_ordinal_scaling_vec works", {
  attr_numeric_4 <- as.factor(c(1.2, 1, 1.6, 2))
  dual_ordinal_context <- array(0, c(4, 4))
  dual_ordinal_context[, 1] <- c(1, 1, 1, 1)
  dual_ordinal_context[, 2] <- c(1, 0, 1, 1)
  dual_ordinal_context[, 3] <- c(0, 0, 1, 1)
  dual_ordinal_context[, 4] <- c(0, 0, 0, 1)
  colnames(dual_ordinal_context) <- c(
    "numeric: x>=1", "numeric: x>=1.2",
    "numeric: x>=1.6", "numeric: x>=2"
  )
  testthat::expect_equal(
    compute_dual_ordinal_scaling_vec(
      attr_numeric_4,
      "numeric"
    ),
    dual_ordinal_context
  )
})



testthat::test_that("calculate_conceptual_scaling works", {
  attr_numeric_4 <- as.factor(c(1.2, 1, 1.6, 2))
  attr_nominal_4 <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1"))
  nominal_numeric_context <- array(0, c(4, 10))
  nominal_numeric_context[, 1] <- c(1, 0, 0, 1)
  nominal_numeric_context[, 2] <- c(0, 1, 1, 0)
  nominal_numeric_context[, 3] <- c(0, 1, 0, 0)
  nominal_numeric_context[, 4] <- c(1, 1, 0, 0)
  nominal_numeric_context[, 5] <- c(1, 1, 1, 0)
  nominal_numeric_context[, 6] <- c(1, 1, 1, 1)
  nominal_numeric_context[, 7] <- c(1, 1, 1, 1)
  nominal_numeric_context[, 8] <- c(1, 0, 1, 1)
  nominal_numeric_context[, 9] <- c(0, 0, 1, 1)
  nominal_numeric_context[, 10] <- c(0, 0, 0, 1)
  colnames(nominal_numeric_context) <- c(
    "nominal: factor_1", "nominal: factor_2",
    "numeric: x<=1", "numeric: x<=1.2",
    "numeric: x<=1.6", "numeric: x<=2",
    "numeric: x>=1", "numeric: x>=1.2",
    "numeric: x>=1.6", "numeric: x>=2"
  )
  testthat::expect_equal(
    compute_conceptual_scaling(
      data.frame(
        nominal = attr_nominal_4,
        numeric = as.numeric(as.character(attr_numeric_4))
      )
    ),
    nominal_numeric_context
  )
})
