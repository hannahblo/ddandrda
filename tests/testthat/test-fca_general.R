
context_pp <- matrix(c(1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1),
                     nrow = 4)





test_that("get_weighted_representation  works", {
  expect_equal(2 * 2, 4)
})


test_that("calculate_phi", {
  expect_equal(calculate_phi(c(0, 1, 1, 0), context = context_pp), c(0, 1, 0, 0))
  expect_equal(calculate_phi(c(1, 0, 0, 0), context = context_pp), c(1, 1, 1, 1))
  expect_equal(calculate_phi(c(0, 1, 1, 0), context = context_pp), c(0, 1, 0, 0))
  expect_equal(calculate_phi(c(0, 0, 1, 0), context = context_pp), c(1, 1, 0, 0))
})


test_that("calculate_psi", {
  expect_equal(calculate_psi(c(1,0,0,0), context = context_pp), c(1, 0, 1, 0))
  expect_equal(calculate_psi(c(0,0,1,0), context = context_pp), c(1, 0, 0, 0))
  expect_equal(calculate_psi(c(0,1,0,1), context = context_pp), c(1, 0, 0, 0))

})


test_that("operator_closure_attr_input works", {
  expect_equal(operator_closure_attr_input(c(1, 0, 0, 0), context_pp),
               c(1, 0, 0, 0))
  expect_equal(operator_closure_attr_input(c(0, 1, 0, 0), context_pp),
               c(1, 1, 1, 0))
})

test_that("operator_closure_obj_input works", {
  expect_equal(operator_closure_obj_input(c(1, 0, 0, 0), context_pp),
               c(1, 1, 0, 0))
  expect_equal(operator_closure_obj_input(c(0, 1, 0, 0), context_pp),
               c(0, 1, 0, 0))
})

test_that("get_weighted_representation works", {
  context <- diag((1:10))
  expect_equal(get_weighted_representation(context[c(1,2,3,4,4,4),])$counts, c(3,1,1,1))
})
