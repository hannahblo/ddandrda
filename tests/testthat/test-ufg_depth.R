# a,b,c,d
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
list_porder_3 <- list(relation_1) # keine ufg
list_porder_4 <- list(relation_1, relation_4, relation_5) # ist ufg
list_porder_5 <- list(relation_2, relation_3) # ist ufg
list_porder_6 <- list(relation_3, relation_5) # ist ufg
list_porder_7 <- list(relation_2, relation_5, relation_4) # ist keine ufg



test_that("test_ufg_porder works", {
  expect_error(test_ufg_porder(c(1,2,3)))
  expect_error(test_ufg_porder(list(1,"b",4)))
  expect_equal(test_ufg_porder(list_porder_1), FALSE)
  expect_equal(test_ufg_porder(list_porder_2), TRUE)
  expect_equal(test_ufg_porder(list_porder_3), FALSE)
  expect_equal(test_ufg_porder(list_porder_4), TRUE)
  expect_equal(test_ufg_porder(list_porder_5), TRUE)
  expect_equal(test_ufg_porder(list_porder_6), TRUE)
  expect_equal(test_ufg_porder(list_porder_7), FALSE)
})

test_that("compute_ufg_porder_index works", {
  expect_error(compute_ufg_porder_index(c(1,2,3)))
  expect_error(compute_ufg_porder_index(list(1,"b",4)))
  expect_equal(compute_ufg_porder_index(list_porder_1), list(c(1, 2), c(1, 3),
                                                       c(2, 3)))
  expect_equal(compute_ufg_porder_index(list_porder_2), list(c(1, 2)))
  expect_equal(compute_ufg_porder_index(list_porder_3), list())
  expect_equal(compute_ufg_porder_index(list_porder_4), list(c(1, 2), c(1, 3),
                                                       c(2, 3), c(1, 2, 3)))
  expect_equal(compute_ufg_porder_index(list_porder_5), list(c(1, 2)))
  expect_equal(compute_ufg_porder_index(list_porder_6),  list(c(1, 2)))
  expect_equal(compute_ufg_porder_index(list_porder_7), list(c(1, 2), c(1, 3),
                                                       c(2, 3)))

})


test_that("compute_ufg_depth works", {

  expect_error(compute_ufg_depth(c(1,2,3)))
  expect_error(compute_ufg_depth(list(1,"b",4)))

  expect_equal(compute_ufg_depth(list_porder_1, list_porder_1),
               c(0.6666667, 0.6666667, 1.0000000),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth(list_porder_2, list_porder_1),
               c(0.6666667, 0.6666667),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth(list_porder_1, list_porder_2),
               c(1, 1, 1),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth(list_porder_3, list_porder_1),
               c(0.6666667),
               tolerance = 1e-7)

})
