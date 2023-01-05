testthat::test_that("compute_nominal_conclusion works", {
  attr_nominal_4 <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1"))
  testthat::expect_equal(
    compute_nominal_conclusion(
      c(1, 0, 0, 0),
      list(data_values = attr_nominal_4)
    )$conclusion, c(1, 0, 0, 1)
  )
  testthat::expect_equal(
    compute_nominal_conclusion(
      c(1, 0, 0, 1),
      list(data_values = attr_nominal_4)
    )$conclusion, c(1, 0, 0, 1)
  )
  testthat::expect_equal(
    compute_nominal_conclusion(
      c(0, 1, 0, 0),
      list(data_values = attr_nominal_4)
    )$conclusion, c(0, 1, 1, 0)
  )
  testthat::expect_equal(
    compute_nominal_conclusion(
      c(1, 1, 0, 0),
      list(data_values = attr_nominal_4)
    )$conclusion, c(1, 1, 1, 1)
  )
})

testthat::test_that("compute_interordinal_concl works", {
  attr_numeric_4 <- as.factor(c(1.2, 1, 1.6, 2))
  testthat::expect_equal(
    compute_interordinal_concl(
      c(1, 0, 0, 0),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(1, 0, 0, 0)
  )
  testthat::expect_equal(
    compute_interordinal_concl(
      c(1, 0, 0, 1),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(1, 0, 1, 1)
  )
  testthat::expect_equal(
    compute_interordinal_concl(
      c(0, 1, 0, 1),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(1, 1, 1, 1)
  )
  testthat::expect_equal(
    compute_interordinal_concl(
      c(1, 0, 1, 0),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(1, 0, 1, 0)
  )
  testthat::expect_equal(
    compute_interordinal_concl(
      c(0, 0, 1, 0),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(0, 0, 1, 0)
  )
  testthat::expect_equal(
    compute_interordinal_concl(
      c(0, 1, 1, 0),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(1, 1, 1, 0)
  )
})

testthat::test_that("compute_generator_nominal", {
  attr_nominal_4 <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1"))
  testthat::expect_equal(
    compute_generator_nominal(
      c(1, 0, 0, 1),
      list(data_values = attr_nominal_4)
    ), list(c(1, 0, 0, 0), c(0, 0, 0, 1))
  )
  testthat::expect_equal(
    compute_generator_nominal(
      c(1, 0, 0, 0),
      list(data_values = attr_nominal_4)
    ), list(c(1, 0, 0, 0), c(0, 0, 0, 1))
  )
  testthat::expect_equal(
    compute_generator_nominal(
      c(1, 1, 0, 0),
      list(data_values = attr_nominal_4)
    ), list(c(1, 1, 0, 0), c(1, 0, 1, 0), c(0, 1, 0, 1), c(0, 0, 1, 1))
  )
  testthat::expect_equal(
    compute_generator_nominal(
      c(1, 1, 1, 1),
      list(data_values = attr_nominal_4)
    ), list(c(1, 1, 0, 0), c(1, 0, 1, 0), c(0, 1, 0, 1), c(0, 0, 1, 1))
  )
})


testthat::test_that("compute_generator_interordinal", {
  attr_numeric_4 <- as.factor(c(1.2, 1, 1.6, 2))
  testthat::expect_equal(
    compute_generator_interordinal(
      c(1, 1, 1, 1),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    ), list(c(0, 1, 0, 1))
  )
  testthat::expect_equal(
    compute_generator_interordinal(
      c(0, 1, 1, 1),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    ), list(c(0, 1, 0, 1))
  )
  testthat::expect_equal(
    compute_generator_interordinal(
      c(1, 1, 1, 0),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    ), list(c(0, 1, 1, 0))
  )
  testthat::expect_equal(
    compute_generator_interordinal(
      c(1, 1, 0, 0),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    ), list(c(1, 1, 0, 0))
  )
  testthat::expect_equal(
    compute_generator_interordinal(
      c(1, 0, 0, 0),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    ), list(c(1, 0, 0, 0))
  )
})


testthat::test_that("test_interordinal_in_concl", {
  attr_numeric_4 <- as.factor(c(1.2, 1, 1.6, 2))
  testthat::expect_equal(test_interordinal_in_concl(
    subset = c(0, 0, 1, 1), 1.8,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), TRUE)
  testthat::expect_equal(test_interordinal_in_concl(
    subset = c(1, 0, 1, 1), 1.8,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), TRUE)
  testthat::expect_equal(test_interordinal_in_concl(
    subset = c(1, 0, 1, 0), 1.8,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), FALSE)
  testthat::expect_equal(test_interordinal_in_concl(
    subset = c(1, 1, 1, 0), 1.8,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), FALSE)
  testthat::expect_equal(test_interordinal_in_concl(
    subset = c(0, 0, 0, 0), 1.8,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), FALSE)
  testthat::expect_equal(test_interordinal_in_concl(
    subset = c(1, 0, 0.0), 1.2,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), TRUE)
})


testthat::test_that("test_ordinal_in_concl", {
  attr_nominal_4 <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1"))
  testthat::expect_equal(test_ordinal_in_concl(
    subset = c(0, 0, 1, 1), "factor_2",
    info_list = list(data_values = attr_nominal_4)
  ), TRUE)
  testthat::expect_equal(test_ordinal_in_concl(
    subset = c(0, 0, 1, 1), "factor_1",
    info_list = list(data_values = attr_nominal_4)
  ), TRUE)
  testthat::expect_equal(test_ordinal_in_concl(
    subset = c(1, 0, 0, 1), "factor_2",
    info_list = list(data_values = attr_nominal_4)
  ), FALSE)
  testthat::expect_equal(test_ordinal_in_concl(
    subset = c(0, 0, 0, 0), "factor_2",
    info_list = list(data_values = attr_nominal_4)
  ), FALSE)
})



relation_1 <- matrix(0, ncol = 4, nrow = 4)
diag(relation_1) <- 1

relation_2 <- relation_1
relation_2[1, c(2,3,4)] <- 1
relation_2[2, c(3,4)] <- 1
relation_2[3, 4] <- 1

relation_3 <- relation_1
relation_3[1, c(2,3,4)] <- 1
relation_3[2, c(3,4)] <- 1
relation_3[4, 3] <- 1

relation_4 <- relation_1
relation_4[1, c(2,3,4)] <- 1
relation_4[2, 4] <- 1
relation_4[3, 4] <- 1

relation_5 <- relation_1
relation_5[1, 4] <- 1
relation_5[2, c(2,3,4)] <- 1
relation_5[3, 4] <- 1

list_porder_1 <- list(relation_1, relation_2, relation_4) # keine ufg
list_porder_2 <- list(relation_1, relation_2) # ist ufg
list_porder_3 <- list(relation_1) # keine ufgg

testthat::test_that("test_porder_in_concl works", {
  expect_equal(test_porder_in_concl(list_porder_1, list_porder_1),
               c(TRUE, TRUE, TRUE))
  expect_equal(test_porder_in_concl(list_porder_2, list_porder_1),
               c(TRUE, TRUE, TRUE))
  expect_equal(test_porder_in_concl(list_porder_3, list_porder_1),
               c(TRUE, FALSE, FALSE))

})
