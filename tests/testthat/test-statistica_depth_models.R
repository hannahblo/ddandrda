test_that("compute_probs_depth_model works", {
  result <- compute_probs_depth_model(depths = (1:10), scale = 1, p = 1, decay_type = "inverse")
  expect_equal(result, NULL)
})




test_that("sample_betweenness_model works", {
  set.seed(1234567)
  context <- random_context(1000, 5)
  context <- cbind(context, 0)
  model1 <- sample_from_betweenness_model(
    context = context, modus = context[1, ],
    scale = 100, p = 1, n = 10, decay_type = "exp"
  )
  model2 <- sample_from_betweenness_model(
    context = context, modus = context[1, ],
    scale = 100, p = 1, n = 10, decay_type =
      "inverse"
  )
  model3 <- sample_from_betweenness_model(
    context = context, modus = context[1, ],
    scale = 100, p = 1, n = 10, decay_type =
      "pearson_vii", df = 10
  )
  expect_equal(nrow(model1), 10)
})

test_that("sample_from_expl_depth_model works", {
  context <- random_context(100, 5)
  result <- sample_from_expl_depth_model(context, context[20, ],
    scale = 1, p = 1, n = 10,
    decay_type = "exp", depth_function = compute_local_tukeys_depth, quasiconcavize = TRUE
  )
  expect_equal(length(result), 50)

  context <- compute_all_partial_orders(4, list = FALSE, complemented = TRUE)
  result <- sample_from_expl_depth_model(context, context[20, ],
    scale = 1, p = 1, n = 10,
    decay_type = "exp", depth_function = compute_local_tukeys_depth, quasiconcavize = TRUE
  )
  expect_equal(length(result), 320)



  result <- sample_from_expl_depth_model(context[-20, ], context[20, ],
    scale = .0000001000, p = 1, n = 10,
    decay_type = "inverse", depth_function = compute_weighted_tukeys_depth, quasiconcavize = TRUE, parameters = list(alpha_weight = 0.01, beta_weight = 0.01), complemented = TRUE
  )
  expect_equal(length(result), 320)

  # result <- sample_from_expl_depth_model(context[-20,],context[20,],scale=.0000001000,p=1,n=10,
  # decay_type="pearsonvii", depth_function = compute_weighted_tukeys_depth,quasiconcavize=TRUE,parameters=list(alpha_weight=0.01,beta_weight=0.01),complemented=TRUE)
  # expect_equal(length(result),320)
})
#<- function(context, modus,
#                                   scale, p, n, decay_type = "exp",
#                                  depth_function, quasiconcavize=FALSE, ...) {")
