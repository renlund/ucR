context("All tests of 'makeDF'")

test_that("makeDF.default works", {
   x <- 1
   class(x) <- paste(sample(letters, 15, T), collapse="")
   expect_equivalent(makeDF(x),data.frame())
} )

test_that("makeDF works with different environments", {
   df <- data.frame(x=1:3, y=letters[1:3], z=factor(LETTERS[1:3]))
   df[['y']] <- as.character(df[['y']])
   li <- as.list(df)
   x0 <- 1:3
   y0 <- letters[1:3]
   z0 <- factor(LETTERS[1:3])
   this_env <- environment()
   expect_equal(makeDF(object = c("x", "y"), env=df), data.frame(x=x0, y=y0))
   expect_equal(makeDF(object = c("x", "z"), env=li), data.frame(x=x0, z=z0))
   expect_equal(makeDF(object = c("x0", "y0"), env=this_env), data.frame(x0=x0, y0=y0))
})

test_that("makeDF.list works", {
   li <- list('a'=1:2, '2'=letters[1:2])
   expect_equal(makeDF(li), data.frame(a=1:2, X2=c("a", "b")))
})

test_that("makeDF.survfit works", {
   library(survival)
   t <- rexp(100)
   e <- rbinom(100,1,0.2)
   f <- factor(rep(LETTERS[1:3], len=100))
   S <- Surv(t, e)
   M1 <- survfit(S~1)
   M1df <- makeDF(M1)
   expect_equal( class(M1df), "data.frame")
   expect_equal( M1df$strata, NULL)
   M2 <- survfit(S~f)
   M2df <- makeDF(M2)
   expect_equal( class(M2df), "data.frame")
   expect_equal( is.null(M2df$strata), FALSE)
})
