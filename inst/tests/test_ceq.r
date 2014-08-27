context("All tests of 'ceq'")

test_that("default params work",{
   x <- c(NA,1,0,NA,Inf,Inf,0)
   y <- c(0,1,NA,NA,-Inf,1,0)
   expect_equal(ceq(x,y), c(F, T, F, F, F, F, T))
   expect_equal(ceq(y,x), c(F, T, F, F, F, F, T))
})

test_that("na.equal set to TRUE work",{
   x <- c(NA,1,0,NA,Inf,Inf,0)
   y <- c(0,1,NA,NA,-Inf,1,0)
   expect_equal(ceq(x,y, na.equal=T), c(F, T, F, T, F, F, T))
   expect_equal(ceq(y,x, na.equal=T), c(F, T, F, T, F, F, T))
})

