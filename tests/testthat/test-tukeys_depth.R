test_that("compute_tukeys_outlyingness works", {
  context <- diag(rep(1, 10))
  outlyingness <- compute_tukeys_outlyingness(context[1, ], context)
  expect_equal(outlyingness, 0.1)
})

test_that("compute_tukeys_depth works", {
  context <- fcaR::planets
  depth_values <- compute_tukeys_depth(context,context)
  argmax <- which.max(depth_values)
  names(argmax) <- NULL
  expect_equal(argmax,9)
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
  a <- compute_all_partial_orders(q, complemented = FALSE,list=FALSE)
  orders <- list()
  m <- nrow(a)
  for (k in (1:m)) {
    temp <- a[k, ]
    dim(temp) <- c(q, q)
    orders[[k]] <- cbind(temp, 1 - temp)
  }
  i <- sample((1:m), size = ceiling(nrow(a) / 3))
  a <- cbind(a, 1 - a)
  ans1 <- compute_tukeys_median_order(orders[i])
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


test_that("is_quasiconcave works", {
  context <- random_context(40, 6)
  depths <- compute_tukeys_depth(context, context)
  ans <- is_quasiconcave(depths, context)

  expect_equal(ans, TRUE)
})
