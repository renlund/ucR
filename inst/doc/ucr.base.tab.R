## ----cache=FALSE, echo=FALSE, include=FALSE------------------------------
library(Hmisc)
library(ucR)
set.seed(19790424)

## ----generating_dataset, cache=TRUE--------------------------------------
n <- 972 # Data set size.
r <- runif(n)
group <- ifelse(r < 0.15, "Small",
         ifelse(r < 0.65, "Medium", "Large"))
age <- round(runif(n, 20, 80))
hgt <- ifelse(group == "Small", rnorm(n, 160, 5),
       ifelse(group == "Medium", rnorm(n, 170, 5),
       rnorm(n, 180, 5)))
bmi <- round(rnorm(n, 25, 3), digits=2)
# 1 missing in Small group.
ix <- which(group == "Small")
ix.na <- sample(ix, size = min(length(ix), 1), replace=F)
bmi[ix.na] <- NA
# 2 missing in Medium group.
ix <- which(group == "Medium")
ix.na <- sample(ix, size = min(length(ix), 2), replace=F)
bmi[ix.na] <- NA
# 3 missing in Large group.
ix <- which(group == "Large")
ix.na <- sample(ix, size = min(length(ix), 3), replace=F)
bmi[ix.na] <- NA

r <- runif(n)
gender <- ifelse(group == "Small",  ifelse(r < 0.8, "Woman", "Man"),
          ifelse(group == "Medium", ifelse(r < 0.5, "Woman", "Man"),
                                    ifelse(r < 0.2, "Woman", "Man")))
r <- runif(n)
country <- ifelse(group == "Small",
                  ifelse(r < 0.10, "Sweden",
                  ifelse(r < 0.25, "Germany",
                  ifelse(r < 0.75, "Spain",
                  ifelse(r < 0.80, "Australia",
                  "Japan")))),
           ifelse(group == "Medium",
                  ifelse(r < 0.20, "Sweden",
                  ifelse(r < 0.40, "Germany",
                  ifelse(r < 0.60, "Spain",
                  ifelse(r < 0.80, "Australia",
                  "Japan")))),
          # Large:
                  ifelse(r < 0.30, "Sweden",
                  ifelse(r < 0.50, "Germany",
                  ifelse(r < 0.65, "Spain",
                  ifelse(r < 0.80, "Australia",
                  "Japan"))))))
# 5 missing in Small group.
ix <- which(group == "Small")
ix.na <- sample(ix, size = min(length(ix), 5), replace=F)
country[ix.na] <- NA
# 2 missing in Medium group.
ix <- which(group == "Medium")
ix.na <- sample(ix, size = min(length(ix), 2), replace=F)
country[ix.na] <- NA
## Add unused level 'Brazil'
# country <- reFactor(factor(country), list(Australia=NULL, Brazil=NULL))
country <- factor(country)
# Correct group order.
group <- reFactor(group, list(Small=NULL, Medium=NULL))
data.set <- data.frame(group, age, hgt, bmi, gender, country)
label(data.set$gender) <- "Gender"
label(data.set$age) <- "Age (years)"
label(data.set$hgt) <- "Height (cm)"
label(data.set$bmi) <- "BMI"
label(data.set$country) <- "Country"
# Use only two groups.
use.2.groups <- TRUE
if (use.2.groups) {
  data.set$group[data.set$group == "Medium"] <- "Large"
  data.set$group <- factor(data.set$group)
}

## ----create_table, results='asis', include=TRUE, echo=TRUE, cache=FALSE----
# Default table.
res <- ucr.base.tab(data=data.set, group.name="group")
dummy <- latex(res, file="", where="!h", caption="A table 1", label="tab:1")

