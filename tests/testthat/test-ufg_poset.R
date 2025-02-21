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


test_that("prepare_ufgpremises_poset works",  {
  expect_equal(prepare_ufgpremises_poset(list_mat_poset_ml =  list_porder_10,
                                         number_items = 4)$count_dup,
               c(1, 1, 2))
  expect_equal(prepare_ufgpremises_poset(list_mat_poset_ml =  list_porder_10,
                                         number_items = 4)$number_obs,
               4)
  expect_equal(dim(prepare_ufgpremises_poset(
    list_mat_poset_ml = list_porder_10,
    number_items = 4)$whole_context),
               c(219, 32))
  expect_equal(prepare_ufgpremises_poset(list_mat_poset_ml =  list_porder_10,
                                         number_items = 4)$n_row_context,
               3)
})



test_that("compute_ufg_existprem_poset works", {
  prep_ufgpremises <- prepare_ufgpremises_poset(list_mat_poset_ml =  list_porder_10,
                                                number_items = 4)
  expect_equal(compute_ufg_existprem_poset(list(relation_2), list(c(1,2)),
                                           prep_ufgpremises),
               0)
  expect_equal(compute_ufg_existprem_poset(list_porder_10, list(c(1,2)),
                                           prep_ufgpremises),
               c(1, 0, 0, 1))
})



test_that("compute_ufg_depth_poset works", {

  expect_error(compute_ufg_depth_poset(c(1,2,3)))
  expect_error(compute_ufg_depth_poset(list(1,"b",4)))
  expect_error(compute_ufg_depth_poset(list(matrix(c(1,2,3,4,5,6), ncol = 2)),
                                        list(matrix(c(1,2,3,4,5,6), ncol = 2))))

  expect_equal(compute_ufg_depth_poset(poset_observed = list_porder_1,
                                        print_progress_text = FALSE,
                                        save_ufg_premises = TRUE),
               list(depth_ufg = c(1.0000000, 0.5, 1.0000000),
                    constant_cn = 0.222222222,
                    total_number_premises = 2,
                    ufg_premises = list(list(relation_1, relation_2),
                                        list(relation_1, relation_4))),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_poset(list_porder_1, list_porder_2,
                                        print_progress_text = FALSE),
               list(depth_ufg = c(1.0000000, 0.5),
                    constant_cn = 0.2222222222,
                    total_number_premises = 2,
                    ufg_premises = list()),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_poset(list_porder_2, list_porder_1,
                                        print_progress_text = FALSE),
               list(depth_ufg = c(1, 1, 1),
                    constant_cn = 0.25,
                    total_number_premises = 1,
                    ufg_premises = list()),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_poset(list_porder_1, list_porder_3,
                                        print_progress_text = FALSE),
               list(depth_ufg = c(1),
                    constant_cn = 0.2222222222,
                    total_number_premises = 2,
                    ufg_premises = list()),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_poset(list_porder_4, list_porder_3,
                                        print_progress_text = FALSE),
               list(depth_ufg = c(0.7),
                    constant_cn = 0.3703704,
                    total_number_premises = 4,
                    ufg_premises = list()),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_poset(list_porder_4, list_porder_5,
                                        print_progress_text = FALSE),
               list(depth_ufg = c(0.4, 0),
                    constant_cn = 0.3703704,
                    total_number_premises = 4,
                    ufg_premises = list()),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_poset(list_porder_8,
                                        print_progress_text = FALSE),
               list(depth_ufg = c(0.5, 0.5384615, 0.5384615, 0.5),
                    constant_cn = 0.40625,
                    total_number_premises = 8,
                    ufg_premises = list()),
               tolerance = 1e-7)
  # Duplications
  expect_equal(compute_ufg_depth_poset(list_porder_10, list_porder_1,
                                        print_progress_text = FALSE),
               list(depth_ufg = c(1, 0.666666667, 1),
                    constant_cn = 0.1875,
                    total_number_premises = 3,
                    ufg_premises = list()),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_poset(list_porder_11, list_porder_2,
                                        print_progress_text = FALSE),
               list(depth_ufg = c(1,1),
                    constant_cn = 0.1875,
                    total_number_premises = 3,
                    ufg_premises = list()),
               tolerance = 1e-7)
  expect_equal(compute_ufg_depth_poset(list_porder_12, list_porder_4,
                                        print_progress_text = FALSE),
               list(depth_ufg = c(0.5, 0.83333333333333, 0.75),
                    constant_cn  = 0.3333333333333333333,
                    total_number_premises = 17,
                    ufg_premises = list()),
               tolerance = 1e-7)

})



test_that("test_ufg_poset works", {
  expect_error(test_ufg_poset(c(1,2,3)))
  expect_error(test_ufg_poset(list(1,"b",4)))
  expect_equal(test_ufg_poset(list_porder_1), FALSE)
  expect_equal(test_ufg_poset(list_porder_2), TRUE)
  expect_equal(test_ufg_poset(list_porder_3), FALSE)
  expect_equal(test_ufg_poset(list_porder_4), TRUE)
  expect_equal(test_ufg_poset(list_porder_5), TRUE)
  expect_equal(test_ufg_poset(list_porder_6), TRUE)
  expect_equal(test_ufg_poset(list_porder_7), FALSE)
  expect_equal(test_ufg_poset(list_porder_8), FALSE)
  expect_equal(test_ufg_poset(list_porder_9), TRUE)
  expect_equal(test_ufg_poset(list_porder_13), FALSE)
  expect_equal(test_ufg_poset(list(relation_4, relation_5)), TRUE)
  expect_equal(test_ufg_poset(list_porder_14), TRUE)
})

test_that("enumerate_ufg_premises works", {
  expect_equal(enumerate_ufg_premises_poset(compute_context_all_poset(3),
                                            n_row_context = 4),
               list(c(1, 3), c(2, 3), c(3, 4)))
})
