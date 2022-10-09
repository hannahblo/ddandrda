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

testthat::test_that("compute_numeric_conclusion works", {
  attr_numeric_4 <- as.factor(c(1.2, 1, 1.6, 2))
  testthat::expect_equal(
    compute_interordinal_conclusion(
      c(1, 0, 0, 0),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(1, 0, 0, 0)
  )
  testthat::expect_equal(
    compute_interordinal_conclusion(
      c(1, 0, 0, 1),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(1, 0, 1, 1)
  )
  testthat::expect_equal(
    compute_interordinal_conclusion(
      c(0, 1, 0, 1),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(1, 1, 1, 1)
  )
  testthat::expect_equal(
    compute_interordinal_conclusion(
      c(1, 0, 1, 0),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(1, 0, 1, 0)
  )
  testthat::expect_equal(
    compute_interordinal_conclusion(
      c(0, 0, 1, 0),
      list(data_values = as.numeric(as.character(attr_numeric_4)))
    )$conclusion, c(0, 0, 1, 0)
  )
  testthat::expect_equal(
    compute_interordinal_conclusion(
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


testthat::test_that("test_interordinal_in_conclusion_newobj", {
  attr_numeric_4 <- as.factor(c(1.2, 1, 1.6, 2))
  testthat::expect_equal(test_interordinal_in_conclusion_newobj(
    subset = c(0, 0, 1, 1), 1.8,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), TRUE)
  testthat::expect_equal(test_interordinal_in_conclusion_newobj(
    subset = c(1, 0, 1, 1), 1.8,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), TRUE)
  testthat::expect_equal(test_interordinal_in_conclusion_newobj(
    subset = c(1, 0, 1, 0), 1.8,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), FALSE)
  testthat::expect_equal(test_interordinal_in_conclusion_newobj(
    subset = c(1, 1, 1, 0), 1.8,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), FALSE)
  testthat::expect_equal(test_interordinal_in_conclusion_newobj(
    subset = c(0, 0, 0, 0), 1.8,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), FALSE)
  testthat::expect_equal(test_interordinal_in_conclusion_newobj(
    subset = c(1, 0, 0.0), 1.2,
    info_list = list(data_values = as.numeric(as.character(attr_numeric_4)))
  ), TRUE)
})


testthat::test_that("test_ordinal_in_conclusion_newobj", {
  attr_nominal_4 <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1"))
  testthat::expect_equal(test_ordinal_in_conclusion_newobj(
    subset = c(0, 0, 1, 1), "factor_2",
    info_list = list(data_values = attr_nominal_4)
  ), TRUE)
  testthat::expect_equal(test_ordinal_in_conclusion_newobj(
    subset = c(0, 0, 1, 1), "factor_1",
    info_list = list(data_values = attr_nominal_4)
  ), TRUE)
  testthat::expect_equal(test_ordinal_in_conclusion_newobj(
    subset = c(1, 0, 0, 1), "factor_2",
    info_list = list(data_values = attr_nominal_4)
  ), FALSE)
  testthat::expect_equal(test_ordinal_in_conclusion_newobj(
    subset = c(0, 0, 0, 0), "factor_2",
    info_list = list(data_values = attr_nominal_4)
  ), FALSE)
})
