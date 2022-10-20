test_that("compute_tukeys_outlyingness works", {
  context <- diag(rep(1,10))
  outlyingness <- compute_tukeys_outlyingness(context,context)
  expect_equal(outlyingness, rep(0.1,10))
})
