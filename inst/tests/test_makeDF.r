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

test_that("makeDF.data.frame works", {
  DF <- data.frame(x=1:2, y=2:1)
  expect_warning(makeDF(c("DF", "DF")))
  expect_identical(makeDF(DF), DF)
  foo <- new.env()
  assign(x="Hoo", value=data.frame(z=LETTERS, x=1:26), env=foo)
  expect_identical(makeDF(object="Hoo", env = foo), get("Hoo", foo))
  assign(x="Boo", value=data.frame(z=letters, x=26:1), env=foo)
  R <- rbind(get("Hoo", foo), get("Boo", foo))
  R$object <- rep(1:2, each=26)
  expect_identical(makeDF(c("Hoo", "Boo"), foo), R)
  assign(x="Gro", value=data.frame(Z=letters, x=26:1), env=foo)
  expect_error(makeDF(c("Hoo", "Gro"), foo))
  expect_error(makeDF("Bar", foo))
  rm(foo, DF, R)
})

test_that("makeDF.list works", {
   li <- list('a'=1:2, '2'=letters[1:2])
   expect_equal(makeDF(li), data.frame(a=1:2, X2=c("a", "b")))
   baz <- new.env()
   assign(x="L1", value=list('a'=1:2, 'QWERTY'=letters[1:2]), envir = baz)
   df1 <- as.data.frame(get("L1", baz))
   assign(x="L2", value=list('a'=2:1, 'QWERTY'=LETTERS[1:2]), envir = baz)
   df2 <- as.data.frame(get("L2", baz))
   assign(x="L3", value=list('a'=3:6, 'wtf'=letters[1:4]), envir = baz)
   expect_identical(makeDF("L1", baz), df1)
   R <- rbind(df1, df2)
   R$object <- rep(1:2, each=2)
   expect_identical(makeDF(c("L1", "L2"), baz), R)
   expect_error(makeDF(c("L1", "L2", "L3"), baz))
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
