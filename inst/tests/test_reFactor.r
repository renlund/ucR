context("All tests of 'reFactor' : ")

test_that("simple recoding works", {
   x <- LETTERS[1:2]
   L <- list(A="A_new", B="B_new")
   test1 <- function() expect_equal(factor(c("A_new", "B_new")), reFactor(x,L) ); test1()
   x <- factor(x); test1()
   
   x <- 1:2
   L <- list("1"="A")
   y_facit <- factor(c("A","2"), levels=c("A", "2"))
   expect_equal(y_facit, reFactor(x,L))
})

test_that("simple reordering works", {
   x <- LETTERS[1:2]
   L <- list(B=NULL, A=NULL)
   test1 <- function() expect_equal(factor(x, levels=LETTERS[2:1]), reFactor(x,L) ); test1()
   x <- factor(x); test1()
   x <- LETTERS[2:1]; test1()
   x <- factor(x); test1()
})

test_that("new levels can be added", {
   x <- LETTERS[1:2]
   L <- list(C=NULL)
   y_facit <- factor(LETTERS[1:2], levels=LETTERS[c(3,1:2)])
   expect_equal(y_facit, reFactor(x,L))
   
   x <- LETTERS[1:2]
   L <- list(C=NULL)
   y_facit <- factor(LETTERS[1:2], levels=LETTERS[c(1:3)])
   expect_equal(y_facit, reFactor(x,L, new.last=TRUE))
})


test_that("warnings work", {
   x <- LETTERS[1:3]
   expect_equal(factor(x), reFactor(x))
   
   L <- list(A=NULL, B=c("B_new","A"))
   expect_warning(y <- reFactor(x,L))
   y_facit <- factor(c("A", "B_new", "C"))
   expect_equal(y, y_facit)
   
   L <- list(A="A_new", B=c("B_new","A", "C"))
   expect_warning(y <- reFactor(x,L))
   y_facit <- factor(c("A_new", "B_new", "B_new"))
   expect_equal(y, y_facit)
   
   L <- list(A=c("A_new","A_newer"))
   expect_warning(y <- reFactor(x,L))
   y_facit <- factor(c("A_new", "B", "C"))
   expect_equal(y, y_facit)
   
   L <- list(A=c("C","A_new"))
   y_facit <- factor(c("A_new", "B", "A_new"))
   expect_equal(reFactor(x,L),y_facit)  
   
   L <- list(A=c("A_new"), B=c("A","C","B_new"))
   expect_warning(y <- reFactor(x,L))
   y_facit <- factor(c("A_new", "B_new", "B_new"))
   expect_equal(y,y_facit)
      
   L <- list(A=c("A_new", "B"),B=NULL)
   expect_warning(y <- reFactor(x,L))
   y_facit <- factor(c("A_new", "A_new", "C"))
   expect_equal(y, y_facit)
   
   x <- LETTERS[1:6]; x[6] <- NA
   L <- list(B=NULL, A="Foo", E=c("D", "Bar"), Missy=c("Borta", "C"))
   expect_warning(reFactor(x,L,na.level="Missy", exclude="A"))
})

test_that("errors work", {
   L <- 1:10
   expect_error(reFactor(x,L))
   
   L <- list(A=c("A_new"), B="A_new")
   expect_error(reFactor(x,L))
})

test_that("exclude works", {
   x <- factor(letters[3:1])
   y <- reFactor(x, exclude="b")
   y_facit <- factor(c("c", NA, "a"), levels=c("a", "c"))
   expect_equal(y, y_facit)
})

test_that("na.level works", {
   x <- factor(c(letters[3:1],NA))
   y <- reFactor(x, na.level="Borta")
   y_facit <- factor(c(letters[3:1], "Borta"), levels=c("a", "b", "c", "Borta"))
   expect_equal(y, y_facit)
})

test_that("na.level + eclude works", {
   x <- factor(c(letters[3:1],NA))
   y <- reFactor(x, na.level="Aorta", exclude=c("a", "c"))
   y_facit <- factor(c(NA,"b",NA,"Aorta"), levels=c("b", "Aorta"))
   expect_equal(y, y_facit)
})

test_that("random examples work", {
   x <- LETTERS[1:5]
   L <- list(B=c("B_new", "C"),E=NULL, D="D_new")
   y_facit <- factor(c("A", "B_new", "B_new", "D_new", "E"),
      levels=c("B_new", "E", "D_new", "A"))
   expect_equal(y_facit, reFactor(x,L))
   
   x <- LETTERS[1:6]
   L <- list(B=NULL, A="Foo", E=c("D", "Bar"))
   y_facit <- factor( c("Foo", "B", "C", "Bar", "Bar", "F"),
      levels=c("B", "Foo", "Bar", "C", "F") )
   expect_equal(y_facit, reFactor(x,L))
   
   x <- factor(LETTERS[1:3], levels=c("B","A","D", "C"))
   L <- list(D=c("None", "B"), C=NULL)
   y_facit <- factor(c("A","None","C"), levels=c("None", "C", "A"))
   expect_equal(y_facit, reFactor(x,L))
   
   x <- LETTERS[1:6]
   L <- list(B=NULL, A="Foo", E=c("D", "Bar"))
   y_facit <- factor( c("Foo", "B", "C", "Bar", "Bar", NA),
      levels=c("B", "Foo", "Bar", "C") )
   expect_equal(y_facit, reFactor(x,L,exclude="F"))
   
   x <- LETTERS[1:6]; x[6] <- NA
   L <- list(B=NULL, A="Foo", E=c("D", "Bar"))
   y_facit <- factor( c("Foo", "B", "C", "Bar", "Bar", "Missy"),
      levels=c("B", "Foo", "Bar", "C", "Missy") )
   expect_equal(y_facit, reFactor(x,L,na.level="Missy"))
   
   x <- LETTERS[1:6]; x[6] <- NA
   L <- list(B=NULL, A="Foo", E=c("D", "Bar"), Missy=c("Borta", "C"))
   y_facit <- factor( c("Foo", "B", "Borta", "Bar", "Bar", "Borta"),
      levels=c("B", "Foo", "Bar", "Borta") )
   expect_equal(y_facit, reFactor(x,L,na.level="Missy"))
   
   x <- -1:2
   L <- list("0"="No", "1"="Yes", "2"="Unknown", "-1"="Missing")
   y <- c("Missing", "No", "Yes", "Unknown")
   y_facit <- factor(y, levels=y[c(2:4,1)])
   expect_equal(y_facit, reFactor(x,L))
   
   x <- LETTERS[1:6]; x[6] <- NA
   L <- list(B=NULL, A=NULL, E=c("D", "Bar"), Missy=c("Borta", "C"))
   y_facit <- factor( c(NA, "B", "Borta", "Bar", "Bar", "Borta"),
      levels=c("B", "A", "Bar", "Borta") )
   expect_equal(y_facit, reFactor(x,L,na.level="Missy", exclude="A"))
   
   x <- LETTERS[1:6]; x[6] <- NA
   L <- list(B=NULL, A=NULL, E=c("D", "Bar"), Missy=c("Borta", "C"))
   y_facit <- factor( c(NA, "B", "Borta", "Bar", "Bar", "Borta"),
      levels=c("B", "Bar", "Borta", "A") )
   expect_equal(y_facit, reFactor(x,L,na.level="Missy", exclude="A", new.last=TRUE))
   
})

test_that("recoding with same value works", {
   x <- letters[1:2]
   L <- list(b="b")
   y_facit <- factor(letters[1:2], levels=letters[2:1])
   expect_equal(y_facit, reFactor(x,L))
   
   x <- factor(LETTERS[1:4], levels=LETTERS[4:1])
   L1 <- list(C=c("C_new","C"), B=c("B","A", "D"))
   L2 <- list(C="C_new", B=c("A", "D"))
   expect_equal(reFactor(x,L1),reFactor(x,L2))
   
   L1 <- list(C=c("B.new", "B"))
   L2 <- list(B=c("B.new", "C"))
   expect_equal(reFactor(x,L1),reFactor(x,L2))
})
