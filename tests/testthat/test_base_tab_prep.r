context("tests of 'base_tab_prep'")

test_that("'base_tab_prep' works (manual test)", {
   if(FALSE){
      n <- 1000
      df <- data.frame(
         x = runif(n),
         z = as.Date(rpois(n, 10000), origin = "1960-01-01"),
         b = rbinom(n, 1, 0.2),
         f = factor(sample(c(letters, LETTERS), n, T)),
         foo = sample(letters[1:3], n, T),
         norm = rnorm(n, 100, 10),
         bar = lubridate::ymd(paste(sample(1900:2015, n, T), sample(1:12, n, T), sample(1:28, n, T), sep = "-")),
         u = rpois(n, 50),
         stringsAsFactors = FALSE
      )
      str(df)
      str(base_tab_prep(df))
   }
})