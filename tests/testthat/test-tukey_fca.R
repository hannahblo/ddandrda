test_that("compute_tukeys_outlyingness works", {
  context <- diag(rep(1, 10))
  outlyingness <- compute_tukeys_outlyingness(context[1, ], context)
  expect_equal(outlyingness, 0.1)
})

test_that("compute_tukeys_depth works", {
  context <- fcaR::planets
  depth_values <- compute_tukeys_depth(context, context)
  argmax <- which.max(depth_values)
  names(argmax) <- NULL
  expect_equal(argmax, 9)

  depth_values <- compute_tukeys_depth(context, context,row_weights=c(1,50,rep(1,7))
  argmax <- which.max(depth_values)
  names(argmax) <- NULL
  expect_equal(argmax, 2)
})
