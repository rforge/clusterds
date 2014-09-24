test_that("test_lossyCounting", {
  
  num <- c(1,1,1,1,2,2,2,3,3,4)
  hash <- lossyCounting(data=as.integer(num))
  expect_that(hash[["1"]][1], equals(c(freq=4)))
  expect_that(hash[["2"]][1], equals(c(freq=3)))
  expect_that(hash[["4"]], equals(NULL))

})