context("tests of 'base_tab_prep'")

test_that("'magnitude' works", {
    expect_equal(magnitude(0), 0)
    expect_equal(magnitude(1), 1)
    expect_equal(magnitude(9.99), 1)
    expect_equal(magnitude(56789), 10000)
    expect_equal(magnitude(0.0999), 0.01)
    expect_equal(magnitude(-0.00101), 0.001)
})

test_that("'trunc_digits' work",{
    expect_equal(trunc_digits(0.0999, 1), 0.09)
    expect_equal(trunc_digits(0.1937, 3), 0.193)
    expect_equal(trunc_digits(0.9, 2), 0.9)
    expect_equal(trunc_digits(59.97, 1), 50)
    expect_equal(trunc_digits(59.97, 3), 59.9)
    expect_equal(trunc_digits(-59.97, 2), -59)
})
