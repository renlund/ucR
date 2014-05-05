# These are tests of 'grepRet'

context("All tests of 'grepRet'")

test_that("Basic functionality", {
   x <- c("Henrik", "Kenneth", "Conny", "Johnny", "Herakleitos")
   expect_equivalent(grepRet("h", x), c("Kenneth", "Johnny"))
   expect_equivalent(grepRet("h|H", x), x[-3])
})