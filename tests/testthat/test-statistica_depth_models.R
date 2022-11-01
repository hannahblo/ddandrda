test_that("simple_betweenness_model works", {
  context <- random_context(1000,5)
  context <- cbind(context,0)
  model1 <- sample_from_betweenness_model(context=context, modus=context[1,],
                                     scale=100, p=1, n=10, decay_type = "exp")
  model2 <- sample_from_betweenness_model(context=context, modus=context[1,],
                                     scale=100, p=1, n=10, decay_type =
                                       "inverse")
  model3 <- sample_from_betweenness_model(context=context, modus=context[1,],
                                     scale=100, p=1, n=10, decay_type =
                                       "pearson_vii",df=10)
  expect_equal(nrow(model1), 10)
})
