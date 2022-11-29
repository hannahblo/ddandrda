# a,b,c,d
porder_1 <- list(
  matrix(c(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
  )),
  matrix(c(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
  ))
)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
