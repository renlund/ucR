context("test-ish of 'data_man' functions")

test_that("'data_man' works", {
   library(testthat)
   if(exists("data_man_container")) rm(data_man_container)
   #data_man("foo", "x", "CDB")
   data_man("foo", "x", "CDB", check=FALSE)
   expect_true(exists("data_man_container"))
   expect_equal(data_man_container[["foo"]]$name, "foo")
   expect_equal(data_man_container[["foo"]]$var, "x")
   expect_equal(data_man_container[["foo"]]$where, "CDB")
   data_man("bar", "y", "ADB", check=FALSE)
   # data_man_container
   expect_equal(length(data_man_container), 2)
   if(FALSE){
      class(data_man_container) <- "foobar"
      data_man("baz", "z", "CDB")
      data_man_container
   }
   rm(data_man_container, envir = .GlobalEnv)
   if(FALSE){
      n <- 1000
      df <- data.frame(
         x = runif(n),
         b = rbinom(n, 1, 0.2),
         f = sample(c(letters, LETTERS), n, T),
         foo = sample(letters[1:3], n, T),
         z = rnorm(n, 100, 10),
         u = rpois(n, 50)
      )
      data_man("new_x", "x", "df", rg = "Arg!")
      data_man("new_b", "b", "df")
      data_man("new_b", "b", "df", recode = list("0" = "No", "1" = "Yes"))
      data_man("new_f", "f", "df", rg = "Arg!")
      data_man("new_foo", "foo", "df")
      data_man("new_foo2", "foo", "df", recode = list("a" = c("ab", "b"), "c"="new_c"))
      data_man("new_z", "z", "df")
      data_man("new_u", "u", "df", comment = "wtf?")

      data_man_get_recode()
      makeDF(data_man_container)
      (x <- makeDF(data_man_container, comment = FALSE))
      str(x)
      str(makeDF(data_man_container, comment = FALSE, rgroup = FALSE))
      rm(n, df, x)
   }
})

test_that("'data_man_create' test must be performed manually...", {
   if(FALSE){
      library(testthat)
      GOR <- data.frame(id = 1:3, foo = letters[1:3], bar = LETTERS[1:3])
      BRI <- data.frame(foo = letters[1:3], id=3:1, bar = LETTERS[1:3])
      FAX <- data.frame(id = c(2,1,3), baz = letters[1:3], quux = LETTERS[1:3])
      if(exists("data_man_container", envir = .GlobalEnv)) rm(data_man_container, envir = .GlobalEnv)

      data_man(name = "new_foo", var = "foo", where = "BRI")
      data_man(name = "new_bar", var = "bar", where = "GOR")
      data_man(name = "quux", var = "quux", where = "FAX")
      expect_equal(names(data_man_container), c("new_foo", "new_bar", "quux"))
      expect_equal(
         data_man_create(id = GOR$id, id.name = "id"),
         structure(list(id = 1:3, new_foo = structure(c(3L, 2L, 1L), .Label = c("a","b", "c"), class = "factor"), new_bar = structure(1:3, .Label = c("A", "B", "C"), class = "factor"), quux = structure(c(2L, 1L, 3L), .Label = c("A",                                                                                                                                                                                                                         "B", "C"), class = "factor")), .Names = c("id", "new_foo", "new_bar",  "quux"), row.names = c(NA, -3L), class = "data.frame")
      )

      names(GOR)[1] = "gor.id"
      names(BRI)[2] = "bri.id"
      names(FAX)[1] = "fax.id"

      expect_equal(
         data_man_create(
            id = GOR$gor.id,
            id.name = list("id", "GOR" = "gor.id", "BRI" = "bri.id", "FAX" = "fax.id")
         ),
         structure(list(id = 1:3, new_foo = structure(c(3L, 2L, 1L), .Label = c("a", "b", "c"), class = "factor"), new_bar = structure(1:3, .Label = c("A",  "B", "C"), class = "factor"), quux = structure(c(2L, 1L, 3L), .Label = c("A", "B", "C"), class = "factor")), .Names = c("id", "new_foo", "new_bar", "quux"), row.names = c(NA, -3L), class = "data.frame")
      )
      expect_equal(
         data_man_create(
            id = GOR$gor.id,
            id.name = list("GOR" = "gor.id", "BRI" = "bri.id", "FAX" = "fax.id")
         ),
         structure(list(gor.id = 1:3, new_foo = structure(c(3L, 2L, 1L
         ), .Label = c("a", "b", "c"), class = "factor"), new_bar = structure(1:3, .Label = c("A","B", "C"), class = "factor"), quux = structure(c(2L, 1L, 3L), .Label = c("A","B", "C"), class = "factor")), .Names = c("gor.id", "new_foo", "new_bar", "quux"), row.names = c(NA, -3L), class = "data.frame")
      )
      rm(BRI, FAX, GOR)
      rm(data_man_container, envir = .GlobalEnv)
   }
   if(FALSE){
      data_man("FOO", "foo")
      data_man("BAR", "bar")
      data_man_create(id = GOR$gor.id, id.name = list("GOR"="gor.id"), na.where = "GOR")
      data_man_create(id = GOR$gor.id, id.name = list("hey", "GOR"="gor.id"), na.where = "GOR")
      rm(data_man_container, envir = .GlobalEnv)
   }
})

test_that("'makeDF.data_man_container' test are manual...", {
   if(FALSE){
      if(exists("data_man_container")) rm(data_man_container)
      data_man(name = "new_foo", var = "foo", where = "BRI")
      data_man(name = "new_bar", var = "bar", where = "GOR")
      data_man(name = "quux", var = "quux", where = "FAX")
      makeDF(data_man_container)

      rm(data_man_container, envir = .GlobalEnv)
      data_man(name = "new_foo", var = "foo", where = "BRI", rg = "Base")
      data_man(name = "new_bar", var = "bar", where = "GOR", rg = "Booze")
      data_man(name = "quux", var = "quux", where = "FAX", rg = "Base")
      (foo <- makeDF(object = data_man_container))
      attributes(foo)

      rm(data_man_container, envir = .GlobalEnv)
      data_man(name = "new_foo", var = "foo", where = "BRI", comment = "Blo")
      data_man(name = "new_bar", var = "bar", where = "GOR", rg = "Quux!")
      data_man(name = "quux", var = "quux", where = "FAX")
      (foo <- makeDF(object = data_man_container))
      attributes(foo)

      rm(data_man_container, envir = .GlobalEnv)
      data_man(name = "new_foo", var = "foo", where = "BRI", comment = "Fnu")
      data_man(name = "new_bar", var = "bar", where = "GOR", rg = "Bah")
      data_man(name = "quux", var = "quux", where = "FAX")
      makeDF(data_man_container, rgroup = FALSE, comment=FALSE)
      rm(data_man_container, envir = .GlobalEnv)
      rm(foo)
   }
})
