# this is not a partial order (reflexivity, transitivity doesn't hold)
mat_1 <- matrix(0, nrow = 5, ncol = 5)
mat_1[1, 3] <- 1
mat_1[2, 1] <- 1
mat_1[4, 3] <- 1
# this is not a partial order (only if reflexiv ommitted)
mat_2 <- mat_1
mat_2[2, 3] <- 1
# this is a partial order
mat_3 <- matrix(0, nrow = 5, ncol = 5)
diag(mat_3) <- 1
# this is not a partial order (only if reflexiv ommitted)
mat_4 <- matrix(0, nrow = 4, ncol = 2)
# this is not a partial order (only if reflexiv ommitted)
mat_5 <- mat_3
mat_5[1, 2] <- -1
# this is not a partial order
mat_6 <- matrix(rep(1, 9), nrow = 3)
# this is not a partial order
mat_7 <- matrix(0, nrow = 4, ncol = 4)
diag(mat_7) <- 1
mat_7[1, 3] <- 1
mat_7[3, 1] <- 1
# this is a partial order
mat_8 <- mat_1
diag(mat_8) <- 1
# this is a partial order
mat_9 <- mat_2
diag(mat_9) <- 1
# this is not a partial order (only if reflexiv ommitted)
mat_10 <- matrix(0, nrow = 6, ncol = 6)

mat_11 <- diag(5)
mat_11[1, ] <- 1
mat_11[2, seq(2,5)] <- 1
mat_11[3, seq(3,5)] <- 1
mat_11[4, seq(4,5)] <- 1
mat_11[5, seq(5,5)] <- 1

mat_12 <- diag(5)
mat_12[1, c(1,2)] <- 1
mat_12[2, c(2,3)] <- 1
mat_12[3, c(3,4)] <- 1
mat_12[4, c(4,5)] <- 1

mat_13 <- diag(4)
mat_13[1, c(2,3,4)] <- 1
mat_13[2, c(3,4)] <- 1
mat_13[4, 3] <- 1

mat_14 <- diag(4)
diag(mat_14) <- 1
mat_14[1,2] <- mat_14[2,4] <- mat_14[4,3] <- 1



test_that("compute_all_poset works", {
  all_4_orders <- compute_all_poset(4, complemented = FALSE, list = FALSE)
  expect_equal(nrow(all_4_orders), 219)
})


test_that("test_if_poset works", {
  expect_error(test_if_poset(c(1, 0, 0)))
  expect_error(test_if_poset(mat_4))
  expect_error(test_if_poset(mat_6, omit_reflexivity = 5))
  expect_error(test_if_poset(mat_5))
  expect_error(test_if_poset(matrix(c(1,2,a,v), ncol = 2)))
  expect_equal(test_if_poset(mat_1), FALSE)
  expect_equal(test_if_poset(mat_2), FALSE)
  expect_equal(test_if_poset(mat_6), FALSE)
  expect_equal(test_if_poset(mat_7), FALSE)
  expect_equal(test_if_poset(mat_3), TRUE)
  expect_equal(test_if_poset(mat_8), FALSE)
  expect_equal(test_if_poset(mat_9), TRUE)
  expect_equal(test_if_poset(mat_1, omit_reflexivity = TRUE), FALSE)
  expect_equal(test_if_poset(mat_2, omit_reflexivity = TRUE), TRUE)
  expect_equal(test_if_poset(mat_3, omit_reflexivity = TRUE), TRUE)
  expect_equal(test_if_poset(mat_6, omit_reflexivity = TRUE), FALSE)
  expect_equal(test_if_poset(mat_7, omit_reflexivity = TRUE), FALSE)
  expect_equal(test_if_poset(mat_8, omit_reflexivity = TRUE), FALSE)
  expect_equal(test_if_poset(mat_9, omit_reflexivity = TRUE), TRUE)
  expect_equal(test_if_poset(matrix(0, nrow = 6, ncol = 6),
                              omit_reflexivity = TRUE
  ), TRUE)
})


test_that("compute_transitive_hull_poset works", {
  expect_error(compute_transitive_hull_poset(c(1, 0, 0)))
  expect_error(compute_transitive_hull_poset(mat_4))
  expect_error(compute_transitive_hull_poset(mat_5))
  expect_error(compute_transitive_hull_poset(matrix(c(1,2,a,b), nrow = 2)))
  expect_equal(compute_transitive_hull_poset(mat_1), mat_2)
  expect_equal(compute_transitive_hull_poset(mat_2), mat_2)
  expect_equal(compute_transitive_hull_poset(mat_3), mat_3)
})


test_that("compute_relation_product_poset works", {
  expect_error(compute_relation_product_poset(c(2,3,4), mat_1))
  expect_error(compute_relation_product_poset(mat_1, mat_7))
  expect_equal(compute_relation_product_poset(mat_1, mat_3), mat_1)
  expect_equal(compute_relation_product_poset(mat_3, mat_3), mat_3)
})

