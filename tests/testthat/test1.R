test_that("An invalid number of parameters",{
  expect_error(find.max.dist())
})
test_that("Result is a data.frame",{
  expect_true(is.data.frame(find.max.dist(spa_poland, 51.1, 17.0)))
})
test_that("Results are correct",{
  data("spa_poland")
  expect_equal(round(find.max.dist(spa_poland, 51.1, 17.0)[1,3]), 491714)
})
