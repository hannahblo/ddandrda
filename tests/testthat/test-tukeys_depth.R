test_that("convert_list_to_context works", {
  list <- list()
  for (k in (1:20)) {
    temp <- array(stats::rnorm(100), c(10, 10))
    list[[k]] <- temp
  }

  list2 <- convert_context_to_list(convert_list_to_context(
    list,
    complemented = FALSE
  ),
  complemented = FALSE
  )
  expect_equal(list, list2)
})

test_that("compute_betweenness_depth works", {
  context <- compute_random_context(1000, 5)
  bd <- compute_betweenness_depth(context, context, context[1, ])
  expect_equal(which.max(bd), 1)
})

test_that("compute_one_simplicial_depth works", {
  context <- compute_random_context(10, 5)
  bd <- compute_one_simplicial_depth(context, context, context[1, ])
  expect_equal(which.max(bd), 1)
})





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


test_that("compute_tukeys_median_order works", {
  q <- sample((3:5), size = 1)
  a <- compute_all_partial_orders(q, complemented = FALSE, list = FALSE)
  orders <- list()
  m <- nrow(a)
  for (k in (1:m)) {
    temp <- a[k, ]
    dim(temp) <- c(q, q)
    orders[[k]] <- cbind(temp, 1 - temp)
  }
  i <- sample((1:m), size = ceiling(nrow(a) / 3))
  a <- cbind(a, 1 - a)
  ans1 <- compute_tukeys_median_order(orders[i])$median
  td <- compute_tukeys_depth(intent = a, context = a[i, ])
  j <- which(td == max(td))
  output <- FALSE
  for (jj in j) {
    ans2 <- a[jj, ]
    dim(ans2) <- c(q, 2 * q)
    if (all(ans1 == ans2)) {
      output <- TRUE
    }
  }


  expect_equal(output, TRUE)
})


test_that("test_if_quasiconcave works", {
  context <- compute_random_context(40, 6)
  context <- cbind(context, 0)
  depths <- compute_tukeys_depth(context, context)
  ans <- test_if_quasiconcave(depths, context)

  expect_equal(ans, TRUE)
})


test_that("test_if_strictly_quasiconcave works", {
  context <- compute_random_context(40, 6)
  context <- cbind(context, 0)
  depths <- (-1) * compute_tukeys_depth(context, context)
  ans <- test_if_strictly_quasiconcave(depths, context)
  expect_equal(ans, FALSE)
})

test_that("compute_all_partial_orders works", {
  all_4_orders <- compute_all_partial_orders(4,
    complemented = FALSE,
    list = FALSE
  )
  expect_equal(nrow(all_4_orders), 219)
})


test_that("compute_loc_sep_test works", {
  all_4_orders <- compute_all_partial_orders(4,
    complemented = TRUE,
    list = TRUE
  )
  i <- sample((1:219), size = 110)
  orders1 <- all_4_orders[i]
  orders2 <- all_4_orders[-i]
  test <- compute_loc_sep_test(orders1, orders2, 0.5, n_rep = 3)
  test_value <- compute_loc_sep_statistic(orders1, orders2, 0.5)
  expect_equal(test$observed_statistic, test_value)
})


test_that("plot_relation works", {
  incidence <- diag(rep(1, 10))
  ans <- plot_relation(incidence)
  expect_equal(incidence[1, 1], 1)
})



test_that("compute_geodetic_median works", {
  all_4_c_orders <- compute_all_partial_orders(4,
    complemented = TRUE,
    list = TRUE
  )
  i <- sample((1:219), size = 55)
  c_orders <- all_4_c_orders[i]
  ans <- compute_geodetic_median(c_orders, proportion = 1)
  ans2 <- compute_tukeys_median_order(c_orders)
  ans3 <- compute_geodetic_median(c_orders, auto = TRUE, fraction = 0.8)
  expect_equal(ans, ans2)
})


test_that("calculate_concept_lattice works", {
  context <- compute_random_context(20, 9)
  ans <- calculate_concept_lattice(context)
  ans2 <- calculate_concept_lattice(context, compute_extents = TRUE)
  expect_equal(ans$intents, ans2$intents)
})


test_that("get_strict_quasiconcave_phull works", {
  context <- compute_random_context(300, 6)
  context <- rbind(context, context)
  depths <- runif(600)
  ans <- all(get_strict_quasiconcave_phull(depths, context) >= 0)
  expect_equal(ans, TRUE)
})


test_that("compute_quasiconcave_hull works", {
  context <- compute_random_context(100, 7)
  depth_values <- compute_tukeys_depth(context, context)
  names(depth_values) <- NULL
  quasiconcavized_depth_values <- compute_quasiconcave_hull(
    depth_values,
    context
  )

  expect_equal(depth_values, quasiconcavized_depth_values)
})


test_that("compute_local_tukeys_depth works", {
  context <- compute_random_context(1000, 6)
  context <- cbind(context, 1, 1)
  index <- sample((1:1000), size = 1)
  location <- context[index, ]
  indexs <- which(context[index, ] == 1)
  depths1 <- compute_tukeys_depth(context[, indexs], context[, indexs])
  depths2 <- compute_local_tukeys_depth(context, context, location)
  expect_equal(depths1, depths2)
})

test_that("compute_weighted_tukeys_depth works", {
  all_partial_5_orders <- compute_all_partial_orders(
    n_items = 5,
    complemented = TRUE,
    list = FALSE
  )
  result <- compute_weighted_tukeys_depth(all_partial_5_orders[20, ],
    all_partial_5_orders,
    all_partial_5_orders[20, ],
    complemented = TRUE,
    parameters = list(
      alphaweight = 0.001,
      beta_weight = 0.001
    )
  )
  expect_equal(result, 1)
})



test_that("compute_delta_mu_p_q works", {
  p_orders <- compute_all_partial_orders(5, list = TRUE, complemented = FALSE)
  result <- compute_delta_mu_p_q(3, 4, p_orders[[10]])
  expect_equal(result, 4)
})

test_that("compute_delta_mu works", {
  p_orders <- compute_all_partial_orders(5, list = TRUE, complemented = FALSE)
  result <- compute_delta_mu(p_orders[[10]])
  expect_equal(result[4, 1], 2)
})
test_that("compute_weighted_tukeys_depth works", {
  p_orders <- compute_all_partial_orders(5, list = FALSE, complemented = TRUE)
  modus <- p_orders[100, ]
  result <- compute_weighted_tukeys_depth(
    p_orders[c(20, 30, 40), ], p_orders,
    modus, TRUE, list(
      alpha_weight = .01,
      beta_weight = .01
    )
  )
  expect_equal(result[1], 0.96)
})
