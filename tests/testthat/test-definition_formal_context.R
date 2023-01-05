# Test of nominal and ordinal scaling
attr_nominal_4 <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1"))
attr_nominal_5 <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1", "factor_2"))
attr_numeric_4 <- as.numeric(c(1.2, 1, 1.6, 2))
attr_numeric_5 <- as.numeric(c(1.2, 1, 1.6, 2, 1.5))


nominal_context <- array(0, c(4, 2))
nominal_context[, 1] <- c(1, 0, 0, 1)
nominal_context[, 2] <- c(0, 1, 1, 0)
colnames(nominal_context) <- c("nominal: factor_1", "nominal: factor_2")
nominal_context_2 <- array(0, c(5, 2))
nominal_context_2[, 1] <- c(1,0,0,1,0)
nominal_context_2[, 2] <- c(0,1,1,0,1)
colnames(nominal_context_2) <- c("nominal: factor_1", "nominal: factor_2")

ordinal_context <- array(0, c(4, 4))
ordinal_context[, 1] <- c(0, 1, 0, 0)
ordinal_context[, 2] <- c(1, 1, 0, 0)
ordinal_context[, 3] <- c(1, 1, 1, 0)
ordinal_context[, 4] <- c(1, 1, 1, 1)
colnames(ordinal_context) <- c(
  "numeric: x<=1", "numeric: x<=1.2",
  "numeric: x<=1.6", "numeric: x<=2")
ordinal_context_2 <- array(0, c(5, 5))
ordinal_context_2[, 1] <- c(0, 1, 0, 0, 0)
ordinal_context_2[, 2] <- c(1, 1, 0, 0, 0)
ordinal_context_2[, 3] <- c(1, 1, 0, 0, 1)
ordinal_context_2[, 4] <- c(1, 1, 1, 0, 1)
ordinal_context_2[, 5] <- c(1, 1, 1, 1, 1)
colnames(ordinal_context_2) <- c(
  "numeric: x<=1", "numeric: x<=1.2", "numeric: x<=1.5",
  "numeric: x<=1.6", "numeric: x<=2")

dual_ordinal_context <- array(0, c(4, 4))
dual_ordinal_context[, 1] <- c(1, 1, 1, 1)
dual_ordinal_context[, 2] <- c(1, 0, 1, 1)
dual_ordinal_context[, 3] <- c(0, 0, 1, 1)
dual_ordinal_context[, 4] <- c(0, 0, 0, 1)
colnames(dual_ordinal_context) <- c(
  "numeric: x>=1", "numeric: x>=1.2",
  "numeric: x>=1.6", "numeric: x>=2")


# Objects for testing partial order scaling
relation_1 <- matrix(0, ncol = 4, nrow = 4)
diag(relation_1) <- 1

relation_2 <- relation_1
relation_2[1, c(2,3,4)] <- 1
relation_2[2, c(3,4)] <- 1
relation_2[3, 4] <- 1

list_porder_1 <- list(relation_1, relation_2)

porder_context <- matrix(NA, ncol = 32, nrow = 2)
porder_context[1, ] <- c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1,
                         1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0)
porder_context[2, ] <- c(1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1,
                         1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0)







testthat::test_that("calculate_nominal_scaling_vec works", {
  testthat::expect_equal(
    compute_nominal_scaling_vec(
      attr_nominal_4,
      "nominal"
    ),
    nominal_context
  )
  testthat::expect_equal(
    compute_nominal_scaling_vec(
      attr_nominal_5,
      "nominal"
    ),
    nominal_context_2
  )
})

testthat::test_that("calculate_ordinal_scaling_vec works", {
  testthat::expect_equal(
    compute_ordinal_scaling_vec(
      attr_numeric_4,
      "numeric"
    ),
    ordinal_context
  )
  testthat::expect_equal(
    compute_ordinal_scaling_vec(
      attr_numeric_5,
      "numeric"
    ),
    ordinal_context_2
  )
})

testthat::test_that("calculate_dualordinal_scal_vec works", {
  testthat::expect_equal(
    compute_dualordinal_scal_vec(
      attr_numeric_4,
      "numeric"
    ),
    dual_ordinal_context
  )
})



testthat::test_that("compute_conceptual_scaling works", {
  expect_error(compute_conceptual_scaling())
  expect_error(compute_conceptual_scaling(input_factor = attr_nominal_4,
                                          input_ordinal_numeric = attr_nominal_5))
  expect_error(compute_conceptual_scaling(input_spatial = list(c(1,2), c(2,3))))
  expect_error(compute_conceptual_scaling(input_porder = list(1,b,c)))
  expect_error(compute_conceptual_scaling(input_factor = c(ab,d,e, 1,2)))
  expect_error(compute_conceptual_scaling(input_ordinal_numeric = list(c,1,3,5)))
  expect_equal(compute_conceptual_scaling(input_factor = attr_nominal_4,
                                          input_ordinal_numeric = attr_numeric_4),
               cbind(nominal_context, ordinal_context, dual_ordinal_context))
  expect_equal(compute_conceptual_scaling(input_porder = list_porder_1),
               porder_context)
  expect_equal(compute_conceptual_scaling(input_porder = list_porder_1,
                                          scaling_methods = c("porder_edge")),
               porder_context[, seq(1,16)])
})
