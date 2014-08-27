## ----cache=FALSE, echo=FALSE, include=FALSE------------------------------
library(ucR)

## ----motivating_example--------------------------------------------------
lev <- c(" ", "never", "some", "a lot")
smoking <- factor(sample(x=lev, 100, TRUE), levels=lev)
table(smoking)

## ----solution------------------------------------------------------------
L <- list(never="no", some=c("yes", "a lot"), " "="unknown")
table(reFactor(smoking, L))

## ----basic_rename--------------------------------------------------------
(x <- factor(LETTERS[1:2]))
L <- list(A="newA", B="newB")
reFactor(x,L)

## ----basic_rename_reorder------------------------------------------------
(x <- factor(LETTERS[1:2], levels=LETTERS[2:1]))
L <- list(A="newA", B="newB")
reFactor(x,L)

## ----basic_reorder-------------------------------------------------------
(x <- factor(LETTERS[1:3]))
L <- list(B=NULL, A=NULL, C=NULL)
reFactor(x,L)

## ----basic_reorder_incomplete--------------------------------------------
(x <- factor(LETTERS[1:5]))
L <- list(D=NULL, B="newB")
reFactor(x,L)

## ----new_level-----------------------------------------------------------
(x <- factor(LETTERS[1:2]))
L <- list(Foo=NULL)
reFactor(x,L)

## ----new_level_last------------------------------------------------------
(x <- factor(LETTERS[1:2]))
L <- list(Foo=NULL, Bar=NULL)
reFactor(x,L, new.last=TRUE)

## ----merge_levels--------------------------------------------------------
(x <- factor(LETTERS[1:3]))
L <- list(A="B") # level B to be absorbed by level A
reFactor(x,L)

## ----merge_levels_new_name-----------------------------------------------
(x <- factor(LETTERS[1:3]))
L <- list(A=c("merge_AB", "B"))
reFactor(x,L)

## ----merge_levels_new_name_alt-------------------------------------------
(x <- factor(LETTERS[1:3]))
L <- list(B=c("merge_AB", "A"))
reFactor(x,L)

## ----merge_levels_new_name_alt2------------------------------------------
(x <- factor(LETTERS[1:3]))
L <- list(B=c("A", "merger", "new_name"))
reFactor(x,L)

## ----merge_levels_new_name_semifail--------------------------------------
(x <- factor(LETTERS[1:3]))
L <- list(B=c("merge_AB","A"), C=c("new_C", "A"))
reFactor(x,L)

## ----exclude-------------------------------------------------------------
(x <- factor(LETTERS[1:4]))
reFactor(x,exclude=c("A","C"))

## ----na_level------------------------------------------------------------
(x <- factor(c(LETTERS[1:2],NA)))
reFactor(x,na.level=TRUE)

## ----na_level_new_name---------------------------------------------------
(x <- factor(c(LETTERS[1:2],NA)))
reFactor(x,na.level="saknas")

## ----complex_1-----------------------------------------------------------
(x <- factor(c(LETTERS[1:2],NA)))
reFactor(x, exclude="A", na.level=TRUE)

## ----complex_2-----------------------------------------------------------
(x <- factor(c(LETTERS[1:2],NA)))
L <- list(A=c("missing", "NA_name"))
reFactor(x, L, na.level="NA_name")

