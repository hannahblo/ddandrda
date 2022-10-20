test_that("compute_tukeys_outlyingness works", {
  context <- diag(rep(1, 10))
  outlyingness <- compute_tukeys_outlyingness(context[1, ], context)
  expect_equal(outlyingness, 0.1)
})


test_that("compute_tukeys_outlyingness works", {
  context <- diag(rep(1, 10))
  outlyingness <- compute_tukeys_outlyingness(context, context)
  names(outlyingness) <- NULL
  expect_equal(outlyingness, rep(0.1, 10))
})

test_that("compute_tukeys_depth works", {
  context <- diag(rep(1, 10))
  depth <- compute_tukeys_depth(context[1, ], context)
  expect_equal(depth, 0.9)
})

test_that("compute_tukeys_depth works", {
  context <- diag(rep(1, 10))
  depth <- compute_tukeys_depth(context, context)
  names(depth) <- NULL
  expect_equal(depth, rep(0.9, 10))
})
