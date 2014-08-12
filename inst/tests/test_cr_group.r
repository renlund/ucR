# library(testthat)
# source("C:/R/P_package/ucR/R/cr_group.r")
context("All test of 'cr_group' and its methods")

test_that("setting class and attributes works", {
    m <- CReator()
    expect_equal(class(m), c("cr_group", "matrix"))
    expect_equal(attr(m, "rgroup"), letters[1:3])
    expect_equal(attr(m, "cgroup"), LETTERS[1:2])
    expect_equal(attr(m, "colnames"), sprintf('GR-%d', 1:2))
    df <- CReator(df=TRUE)
    expect_equal(class(df), c("cr_group", "data.frame"))
})

test_that("indexing works", {
    m <- CReator(n=5,m=6)
    i <- 4:2
    j <- 5:2
    y <- m[i,j]
    expect_equal(class(y), c("cr_group", "matrix"))
    expect_equal(attr(y, "rgroup"), letters[1:5][i])
    expect_equal(attr(y, "cgroup"), LETTERS[1:6][j])
    expect_equal(attr(y, "colnames"), sprintf("GR-%d", 1:6)[j])
    i <- c(5,2,3,1)
    j <- c(1,6)
    y <- m[i,j]
    expect_equal(class(y), c("cr_group", "matrix"))
    expect_equal(attr(y, "rgroup"), letters[1:5][i])
    expect_equal(attr(y, "cgroup"), LETTERS[1:6][j])
    expect_equal(attr(y, "colnames"), sprintf("GR-%d", 1:6)[j])
    m <- CReator(n=5,m=6, df=TRUE)
    i <- 4:2
    j <- 5:2
    y <- m[i,j]
    expect_equal(class(y), c("cr_group", "data.frame"))
    expect_equal(attr(y, "rgroup"), letters[1:5][i])
    expect_equal(attr(y, "cgroup"), LETTERS[1:6][j])
    expect_equal(attr(y, "colnames"), sprintf("GR-%d", 1:6)[j])
    i <- c(5,2,3,1)
    j <- c(1,6)
    y <- m[i,j]
    expect_equal(class(y), c("cr_group", "data.frame"))
    expect_equal(attr(y, "rgroup"), letters[1:5][i])
    expect_equal(attr(y, "cgroup"), LETTERS[1:6][j])
    expect_equal(attr(y, "colnames"), sprintf("GR-%d", 1:6)[j])
})

test_that("replacement index works", {
    m <- CReator()
    m[3,2] <- -1
    expect_equal(class(m), c("cr_group", "matrix"))
    expect_equal(attr(m, "rgroup"), letters[1:3])
    expect_equal(attr(m, "cgroup"), LETTERS[1:2])
    expect_equal(attr(m, "colnames"), sprintf('GR-%d', 1:2))
    expect_equal(as.numeric(m[3,2]), -1) 
})
