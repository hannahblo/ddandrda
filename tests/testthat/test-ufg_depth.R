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

relation_6 <- relation_1
relation_6[4,2] <- 1

relation_7 <- relation_1
relation_7[3,2] <- 1

relation_8 <- relation_1
relation_8[3,1] <- 1

relation_9 <- relation_1
relation_9[2,1] <- 1
relation_9[1,3] <- 1
relation_9[2,3] <- 1


relation_10 <- relation_1
relation_10[4,] <- 1
relation_10[1,3] <- 1
relation_10[2,3] <- 1

relation_11 <- relation_1
relation_11[2,3] <- 1
relation_11[3,1] <- 1
relation_11[2,1] <- 1

relation_12 <- relation_1
relation_12[1,4] <- 1
relation_12[3,2] <- 1

relation_13 <- relation_1
relation_13[2,1] <- 1
relation_13[2,4] <- 1
relation_13[2,3] <- 1
relation_13[4,3] <- 1


list_porder_1 <- list(relation_1, relation_2, relation_4) # keine ufg
list_porder_2 <- list(relation_1, relation_2) # ist ufg
list_porder_3 <- list(relation_1) # keine ufg
list_porder_4 <- list(relation_1, relation_4, relation_5) # ist keine ufg nicht union-free
list_porder_5 <- list(relation_2, relation_3) # ist ufg
list_porder_6 <- list(relation_3, relation_5) # ist ufg
list_porder_7 <- list(relation_2, relation_5, relation_4) # ist keine ufg
list_porder_8 <- list(relation_6, relation_7, relation_8, relation_9) # ist keine ufg
list_porder_9 <- list(relation_6,  relation_7, relation_8)
# now with duplicates
list_porder_10 <- list(relation_1, relation_2, relation_2, relation_4)
list_porder_11 <- list(relation_1, relation_1, relation_1, relation_2)
list_porder_12 <- list(relation_1, relation_4, relation_4, relation_4,
                       relation_5, relation_5)
list_porder_13 <- list(relation_2, relation_4) # keine ufg
list_porder_14 <- list(relation_10, relation_11, relation_12, relation_13)


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
  expect_equal(test_ufg_porder(list_porder_8), FALSE)
  expect_equal(test_ufg_porder(list_porder_9), TRUE)
  expect_equal(test_ufg_porder(list_porder_13), FALSE)
  expect_equal(test_ufg_porder(list(relation_4, relation_5)), TRUE)
  expect_equal(test_ufg_porder(list_porder_14), TRUE)
})


test_that("compute_ufg_depth_porder works", {

  expect_error(compute_ufg_depth_porder(c(1,2,3)))
  expect_error(compute_ufg_depth_porder(list(1,"b",4)))
  expect_error(compute_ufg_depth_porder(list(matrix(c(1,2,3,4,5,6), ncol = 2)),
                                        list(matrix(c(1,2,3,4,5,6), ncol = 2))))

  expect_equal(compute_ufg_depth_porder(list_porder_1,
                                        print_progress_text = FALSE),
               list(ufg_depth = c(1.0000000, 0.5, 1.0000000),
                    total_number_premises = 2),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_porder(list_porder_1, list_porder_2,
                                        print_progress_text = FALSE),
               list(ufg_depth = c(1.0000000, 0.5),
                    total_number_premises = 2),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_porder(list_porder_2, list_porder_1,
                                        print_progress_text = FALSE),
               list(ufg_depth = c(1, 1, 1),
                    total_number_premises = 1),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_porder(list_porder_1, list_porder_3,
                                        print_progress_text = FALSE),
               list(ufg_depth = c(1),
                    total_number_premises = 2),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_porder(list_porder_4, list_porder_3,
                                        print_progress_text = FALSE),
               list(ufg_depth = c(0.75),
                    total_number_premises = 4),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_porder(list_porder_4, list_porder_5,
                                        print_progress_text = FALSE),
               list(ufg_depth = c(0.5, 0),
                    total_number_premises = 4),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_porder(list_porder_8,
                                        print_progress_text = FALSE),
               list(ufg_depth = c(0.5, 0.625, 0.625, 0.5),
                    total_number_premises = 8),
               tolerance = 1e-7)
  # Duplications
  expect_equal(compute_ufg_depth_porder(list_porder_10, list_porder_1,
                                        print_progress_text = FALSE),
               list(ufg_depth = c(1, 0.666666667, 1),
                    total_number_premises = 3),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_porder(list_porder_11, list_porder_2,
                                        print_progress_text = FALSE),
               list(ufg_depth = c(1,1),
                    total_number_premises = 3),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_porder(list_porder_12, list_porder_4,
                                        print_progress_text = FALSE),
               list(ufg_depth = c(11/17, 15/17, 14/17),
                    total_number_premises = 17),
               tolerance = 1e-7)

})



test_that("approx_ufg_depth_porder works", {
  set.seed(34)
  expect_equal(approx_ufg_depth_porder(stop_criteria = list(
    number_iterations = as.integer(1000),
    number_premises = Inf,
    max_time = Inf),
    porder_observed = list_porder_1)$ufg_depth,
               c(1.0000000, 0.5, 1.0000000),
               tolerance = 1e-1)
  expect_equal(approx_ufg_depth_porder(stop_criteria = list(
    number_iterations = as.integer(5000),
    number_premises = Inf,
    max_time = Inf),
    porder_observed = list_porder_4, list_porder_5)$ufg_depth,
               c(0.5, 0),
               tolerance = 1e-1)
  expect_equal(approx_ufg_depth_porder(stop_criteria = list(
    number_iterations = as.integer(10000),
    number_premises = Inf,
    max_time = Inf),
    porder_observed = list_porder_12, list_porder_4)$ufg_depth,
               c(11/17, 15/17, 14/17),
               tolerance = 1e-1)
  expect_equal(approx_ufg_depth_porder(stop_criteria = list(
    number_iterations = as.integer(1000),
    number_premises = Inf,
    max_time = Inf),
    porder_observed = list_porder_8)$ufg_depth,
               c(0.5, 0.625, 0.625, 0.5),
               tolerance = 1e-1)
  expect_equal(approx_ufg_depth_porder(stop_criteria = list(
    number_iterations = Inf,
    number_premises = 1000,
    max_time = Inf),
    porder_observed = list_porder_1,
    porder_depth = list_porder_2)$ufg_depth,
    c(1.0000000, 0.5),
    tolerance = 1e-1)
  expect_equal(approx_ufg_depth_porder(stop_criteria = list(
    number_iterations = 5000,
    number_premises = 100,
    max_time = Inf),
    porder_observed = list_porder_4,
    porder_depth = list_porder_3)$ufg_depth,
    c(0.75),
    tolerance = 1e-1)
  expect_equal(approx_ufg_depth_porder(stop_criteria = list(
    number_iterations = Inf,
    number_premises = Inf,
    max_time = 1),
    porder_observed = list_porder_1,
    porder_depth = list_porder_3)$ufg_depth,
    c(1),
    tolerance = 1e-1)
  expect_error(approx_ufg_depth_porder(stop_criteria = list(
    number_iterations = Inf,
    number_premises = Inf,
    max_time = Inf),
    porder_observed = list_porder_1,
    porder_depth = list_porder_3))
})




