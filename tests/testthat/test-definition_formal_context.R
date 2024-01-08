# -------------- Definition of the objects for testing

# Test of nominal and ordinal scaling
attr_nominal_4 <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1"))
attr_nominal_5 <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1", "factor_2"))
attr_numeric_4 <- as.numeric(c(1.2, 1, 1.6, 2))
attr_numeric_5 <- as.numeric(c(1.2, 1, 1.6, 2, 1.5))


nominal_context <- array(0, c(4, 2))
nominal_context[, 1] <- c(1, 0, 0, 1)
nominal_context[, 2] <- c(0, 1, 1, 0)
colnames(nominal_context) <- c("nominal: factor_1", "nominal: factor_2")
nominal_context_2 <- array(0, c(5, 2))
nominal_context_2[, 1] <- c(1,0,0,1,0)
nominal_context_2[, 2] <- c(0,1,1,0,1)
colnames(nominal_context_2) <- c("nominal: factor_1", "nominal: factor_2")

ordinal_context <- array(0, c(4, 4))
ordinal_context[, 1] <- c(0, 1, 0, 0)
ordinal_context[, 2] <- c(1, 1, 0, 0)
ordinal_context[, 3] <- c(1, 1, 1, 0)
ordinal_context[, 4] <- c(1, 1, 1, 1)
colnames(ordinal_context) <- c(
  "numeric: x<=1", "numeric: x<=1.2",
  "numeric: x<=1.6", "numeric: x<=2")
ordinal_context_2 <- array(0, c(5, 5))
ordinal_context_2[, 1] <- c(0, 1, 0, 0, 0)
ordinal_context_2[, 2] <- c(1, 1, 0, 0, 0)
ordinal_context_2[, 3] <- c(1, 1, 0, 0, 1)
ordinal_context_2[, 4] <- c(1, 1, 1, 0, 1)
ordinal_context_2[, 5] <- c(1, 1, 1, 1, 1)
colnames(ordinal_context_2) <- c(
  "numeric: x<=1", "numeric: x<=1.2", "numeric: x<=1.5",
  "numeric: x<=1.6", "numeric: x<=2")

dual_ordinal_context <- array(0, c(4, 4))
dual_ordinal_context[, 1] <- c(1, 1, 1, 1)
dual_ordinal_context[, 2] <- c(1, 0, 1, 1)
dual_ordinal_context[, 3] <- c(0, 0, 1, 1)
dual_ordinal_context[, 4] <- c(0, 0, 0, 1)
colnames(dual_ordinal_context) <- c(
  "numeric: x>=1", "numeric: x>=1.2",
  "numeric: x>=1.6", "numeric: x>=2")


# Objects for testing partial order scaling
relation_1 <- matrix(0, ncol = 4, nrow = 4)
diag(relation_1) <- 1

relation_2 <- relation_1
relation_2[1, c(2,3,4)] <- 1
relation_2[2, c(3,4)] <- 1
relation_2[3, 4] <- 1

list_porder_1 <- list(relation_1, relation_2)

porder_context <- matrix(NA, ncol = 32, nrow = 2)
porder_context[1, ] <- c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1,
                         1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0)
porder_context[2, ] <- c(1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1,
                         1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0)
colnames(porder_context) <-  c(
  unlist(lapply(seq(1, 4), FUN = function(y) {lapply(seq(1, 4), FUN = function(x){paste0(y, "<=", x)})})),
  unlist(lapply(seq(1, 4), FUN = function(y) {lapply(seq(1, 4), FUN = function(x){paste0(y, " neq<=", x)})}))
)



# objects for testing spatial scaling
# Points on one line except for one point
point_pattern_line_plus <- array(c(1,2,3,4,4,1,1,1,1,0), dim = c(5,2))
colnames(point_pattern_line_plus) <- c("x", "y")
rownames(point_pattern_line_plus) <- head(letters, 5)
# plot(point_pattern_line_plus, pch = rownames(point_pattern_line_plus))

fc_spatial_line_plus <- matrix(1, nrow = 5, ncol = 20)
fc_spatial_line_plus[c(17, 18, 19, 33, 34, 44, 55, 60, 65, 75, 80,
             81, 90, 91, 92, 96, 97, 98)] <- 0
colnames(fc_spatial_line_plus) <- c("ab_>=0", "ac_>=0", "ad_>=0", "ae_>=0",
                          "bc_>=0", "bd_>=0", "be_>=0",
                          "cd_>=0", "ce_>=0",
                          "de_>=0",
                          "ab_<=0", "ac_<=0", "ad_<=0", "ae_<=0",
                          "bc_<=0", "bd_<=0", "be_<=0",
                          "cd_<=0", "ce_<=0",
                          "de_<=0")
rownames(fc_spatial_line_plus) <- c("a", "b", "c", "d", "e")



# All points on one line
point_pattern_line <- array(c(1,2,3,4,1,1,1,1), dim = c(4,2))
colnames(point_pattern_line) <- c("x", "y")
rownames(point_pattern_line) <- head(letters, 4)
# plot(point_pattern_line, pch = rownames(point_pattern_line))

# All points define are convex chull
point_pattern_convex <- array(c(0,1,0,-1,1,0,-1,0), dim = c(4,2))
colnames(point_pattern_convex) <- c("x", "y")
rownames(point_pattern_convex) <- head(letters, 4)
# plot(point_pattern_convex, pch = rownames(point_pattern_convex))

fc_spatial_convex <- matrix(1, nrow = 4, ncol = 12)
fc_spatial_convex[c(6, 10, 11, 19, 27, 28, 32, 37, 40, 41, 45, 46)] <- 0
colnames(fc_spatial_convex) <- c("ab_>=0", "ac_>=0", "ad_>=0",
                                    "bc_>=0", "bd_>=0",
                                    "cd_>=0",
                                    "ab_<=0", "ac_<=0", "ad_<=0",
                                    "bc_<=0", "bd_<=0",
                                    "cd_<=0")
rownames(fc_spatial_convex) <- c("a", "b", "c", "d")








#---------- Testing

testthat::test_that("calculate_nominal_scaling_vec works", {
  testthat::expect_equal(
    compute_nominal_scaling_vec(
      attr_nominal_4,
      "nominal"
    ),
    nominal_context
  )
  testthat::expect_equal(
    compute_nominal_scaling_vec(
      attr_nominal_5,
      "nominal"
    ),
    nominal_context_2
  )
})

testthat::test_that("calculate_ordinal_scaling_vec works", {
  testthat::expect_equal(
    compute_ordinal_scaling_vec(
      attr_numeric_4,
      "numeric"
    ),
    ordinal_context
  )
  testthat::expect_equal(
    compute_ordinal_scaling_vec(
      attr_numeric_5,
      "numeric"
    ),
    ordinal_context_2
  )
})

testthat::test_that("calculate_dualordinal_scal_vec works", {
  testthat::expect_equal(
    compute_dualordinal_scal_vec(
      attr_numeric_4,
      "numeric"
    ),
    dual_ordinal_context
  )
})


testthat::test_that("calculate_spatial_scaling_mat", {
  testthat::expect_equal(
    compute_spatial_scaling_mat(point_pattern_line_plus)$context, fc_spatial_line_plus )
  testthat::expect_equal(compute_spatial_scaling_mat(point_pattern_convex)$context,
                         fc_spatial_convex)
})


testthat::test_that("compute_conceptual_scaling works", {
  expect_error(compute_conceptual_scaling())
  expect_error(compute_conceptual_scaling(input_factor = attr_nominal_4,
                                          input_ordinal_numeric = attr_nominal_5))
  expect_error(compute_conceptual_scaling(input_spatial = list(c(1,2), c(2,3))))
  expect_error(compute_conceptual_scaling(input_porder = list(1,b,c)))
  expect_error(compute_conceptual_scaling(input_factor = c(ab,d,e, 1,2)))
  expect_error(compute_conceptual_scaling(input_ordinal_numeric = list(c,1,3,5)))
  expect_error(compute_conceptual_scaling(input_spatial = point_pattern_line))
  expect_equal(compute_conceptual_scaling(input_factor = attr_nominal_4,
                                          input_ordinal_numeric = attr_numeric_4),
               cbind(nominal_context, ordinal_context, dual_ordinal_context))
  expect_equal(compute_conceptual_scaling(input_porder = list_porder_1),
               porder_context)
  expect_equal(compute_conceptual_scaling(input_porder = list_porder_1,
                                          scaling_methods = c("porder_edge")),
               porder_context[, seq(1,16)])
  expect_equal(compute_conceptual_scaling(input_spatial = point_pattern_line_plus),
               fc_spatial_line_plus)
  expect_equal(compute_conceptual_scaling(input_spatial = point_pattern_convex,
                                          input_ordinal_numeric = attr_numeric_4),
               cbind(fc_spatial_convex, ordinal_context, dual_ordinal_context))
})
