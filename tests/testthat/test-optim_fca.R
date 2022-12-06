test_that("sample_concept works", {
  n_items <- 4
  context <- compute_context_all_p_orders(n_items = n_items)
  c_orders <- compute_all_partial_orders(
    n_items = n_items,
    complemented = TRUE, list = TRUE
  )
  context <- compute_all_partial_orders(
    n_items = n_items,
    complemented = TRUE, list = FALSE
  )
  i <- sample(seq_len(nrow(context)), size = 50)
  reference_context <- context[i, ]
  g <- function(intent, context) {
    compute_tukeys_depth(
      c(intent, 1 - intent),
      reference_context
    )
  }

  ans1 <- sample_concept(context, steps = 10000, g)
  ans2 <- compute_tukeys_median_order(c_orders[i])



  expect_equal(as.vector(ans2$median)[(1:n_items^2)], ans1)
})
