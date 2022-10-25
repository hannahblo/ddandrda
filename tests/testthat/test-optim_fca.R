test_that("sample_concept works", {

  q <- 4
  CT <- compute_context_all_porders(q=q,complemented=FALSE)
  corders <- compute_all_partial_orders(q=q,complemented=TRUE,list=TRUE)
  context <- compute_all_partial_orders(q=q,complemented=TRUE,list=FALSE)
  i <- sample((1:nrow(context)),size=50)
  reference_context <- context[i,]
  g <- function(intent,context){compute_tukeys_depth(c(intent,1-intent),
                                                     reference_context)}

  ans1 <- sample_concept(CT,steps=1000,g)
  ans2 <- compute_tukeys_median_order(corders[i])



  expect_equal(as.vector(ans2$median)[(1:q^2)],ans1)
})
