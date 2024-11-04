
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
list_porder_3 <- list(relation_1) # keine ufgg



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




testthat::test_that("test_porder_in_concl works", {
  expect_equal(test_porder_in_concl(list_porder_1, list_porder_1),
               c(TRUE, TRUE, TRUE))
  expect_equal(test_porder_in_concl(list_porder_2, list_porder_1),
               c(TRUE, TRUE, TRUE))
  expect_equal(test_porder_in_concl(list_porder_3, list_porder_1),
               c(TRUE, FALSE, FALSE))

})

