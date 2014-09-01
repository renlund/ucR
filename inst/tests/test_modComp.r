context("all tests of 'modComp', 'transFormat' and their helper functions")


test_that("'modComp' works for model = lm", {
   DF <- data.frame(x1=c(1,2,3,4),x2=c(3,4,0,1))
   DF$y <- 2*DF$x1 + DF$x2 + c(0.1, -0.2, 0.05,0.05)
   expect_equivalent(
      modComp(resp = "y", 
              vars = c("x1", "x2"), 
              model = lm, 
              covars = list(1:2), 
              data = DF, 
              uni = FALSE, 
              ci = TRUE, 
              round=2),
      matrix(c("1.93 (0.72,3.14)", "0.92 (0.07,1.77)"), nrow=2)
   )
   expect_equivalent(
      modComp(resp = "y", 
              vars = c("x1", "x2"), 
              model = lm, 
              covars = list(1:2), 
              data = DF, 
              uni = TRUE, 
              ci = TRUE, 
              round=3),
      matrix(c("1.01 (-1.796,3.816)","1.93 (0.725,3.135)","-0.045 (-2.984,2.894)","0.92 (0.068,1.772)"), nrow=2, byrow=TRUE)
   )
   expect_equivalent(
      modComp(resp = "y", 
              vars = c("x1", "x2"), 
              model = lm, 
              covars = list(1,1:2), 
              data = DF, 
              uni = FALSE, 
              ci = FALSE, 
              signif=1),
      matrix(c("1","2.0",NA,"0.9"), nrow=2, byrow=TRUE)
   )
   expect_equivalent(
      modComp(resp = "y", 
              vars = c("x1", "x2"), 
              model = lm, 
              covars = list(1:2), 
              data = DF, 
              uni = TRUE, 
              ci = FALSE, 
              signif=2,
              fun=exp),
      matrix(c("2.7","6.9","0.96","2.5"), nrow=2, byrow=TRUE)
   )
   rm(DF)
}) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

test_that("'modComp' works for model = glm", {
   DF <- data.frame(
      x = c(3,1,2,3,2,4,5,6,4,5,3,2,4,1,1,2,3,4,6,7,8,1,1,2,6,4,2,1,1,3,4),
      y = c(0,1,1,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,0),
      z =  rep(letters[1:2], length.out=31),
      u = rep(c(1:5), length.out=31)
      )
   Model <- function(formula, data) glm(formula=formula, family="binomial",data=data)
   expect_equivalent(
   modComp(resp = "y", 
           vars = c("x", "z"), 
           model=Model,
           covars=list(1,1:2), 
           data = DF, 
           uni=TRUE,
           ci=FALSE,
           signif=3),
   matrix(c("-0.75","-0.75","-0.808","-0.501",NA,"-0.931"),nrow=2,byrow=TRUE)
   )
   rm(DF, Model)
}) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

test_that("'modComp' works for coxph", {   
   DF <- data.frame(
      x = c(3,1,2,3,2,4,5,6,4,5,3,2,4,1,1,2,3,4,6,7,8,1,1,2,6,4,2,1,1,3,4),
      y = c(0,1,1,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,0),
      z =  rep(letters[1:2], length.out=31),
      u = rep(c(1:5), length.out=31)
   )
   DF$cox_endp <- with(DF, Surv(x,y))
   expect_equivalent(
      modComp(resp = "cox_endp", 
              vars = c("z", "u"), 
              model=coxph,
              covars=list(1:2), 
              data = DF, 
              uni=TRUE,
              ci=TRUE,
              round=1),
      matrix(c("-0.3 (-1.6,1.0)","-0.4 (-1.7,0.9)","0.2 (-0.2,0.6)","0.2 (-0.2,0.6)"),nrow=2,byrow=TRUE)
   )
   rm(DF)
}) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

test_that("'transFormat' works", {
   expect_equal(transFormat(c(-0.1, 4.0001, 3.5001), digits=2),c("-0.1", " 4.0", " 3.5"))
   expect_equal(transFormat(c(-0.1, 4.0001, 3.5001), exp),c(" 0.9048374", "54.6036101", "33.1187637"))
   expect_equal(transFormat(c(-0.1, 4.0001, 3.5001), exp, digits=2),c(" 0.9", "54.6", "33.1"))
}) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

test_that("'klister' works", {
   expect_equal(klister(matrix(1:6, nrow=2)), c("(1,3,5)", "(2,4,6)"))
}) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

test_that("spann works", {
   expect_equal(spann(3,4), 3:6)
   expect_equal(spann(c(-2,7),c(3,2)), c(-2:0,7:8))
   expect_equal(spann(c(1,3,4,7), c(2,1,2,5)), c(1:5,7:11))
}) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
