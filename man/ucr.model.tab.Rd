% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ucr_model_tab.r
\name{ucr.model.tab}
\alias{ucr.model.tab}
\title{Description of analysis models.}
\usage{
ucr.model.tab(
  model,
  x.names = NULL,
  data,
  model.name,
  rm.intercept = "(Intercept)",
  allow.missing.coef = F,
  allow.duplicated.coef = F,
  factor.sep = "",
  link.fcn = exp,
  num.scale = NULL,
  include.ref = T,
  or.hr = "OR",
  include.p = T,
  n = NULL,
  max.ucl = 100
)
}
\arguments{
\item{model}{The model object, or a list of model objects. The following
classes are recognized: \code{glm}, \code{stripped.glm},
\code{lrm}, \code{cph}, \code{mipo}, \code{glmerMod},
\code{matrix}.}

\item{x.names}{Names of variables to be included in the table. Variable
names (in data frame), not coefficient names.}

\item{data}{A data frame, typically the one that the model was built on.
Used to extract coefficient names.}

\item{model.name}{The name of the model. Used as column heading.}

\item{rm.intercept}{A string specifying the name of the intercept term,
which will be removed from the tables. If one wants to
keep the intercept, the parameter should be set to
\code{NULL}.}

\item{allow.missing.coef}{Logical telling whether missing (absent)
coefficients are allowed. If so, the corresponding
table entry will be empty. O/W, the program will
stop with an error.}

\item{allow.duplicated.coef}{Logical telling whether duplicated coefficient
names are allowed in the model. Occasionally,
this can be handy, e.g. for penalized terms in
a Cox model.}

\item{factor.sep}{Separation between factor names and levels. Empty string
for classical R models (\code{glm} etc.), but '='
for rms models, corresponding to typical coefficient
names like \code{genderMale} or \code{gender=Male}.}

\item{link.fcn}{A link function, e.g. the exponential function for logistic
regression.}

\item{num.scale}{A list to specify the scale for numerical variables. The
list may contain one element for each numerical variable,
named as the variable itself. These elements are themselves
lists with the values "type" and (optionally) "digits" and
"unit". Allowed values for the "type" field:
\itemize{
\item "sd": By standard deviation.
\item "iqr": By IQR.
\item A numerical value, such as 10.
}
If the "unit" field is provided, then this unit will be
reported in the table. A typical result in the "Level"
column could be "By IQR: 1.4 mmol/L". (In this example,
\code{digits} = 1).}

\item{include.ref}{A logical specifying whether reference levels should be
included in the table.}

\item{or.hr}{A string providing a name for the estimete, e.g. "OR" for odds
ratios.}

\item{include.p}{Logical specifying whether P-values are to be reported.}

\item{n}{Number of individuals. Will be reported if non-\code{NULL}.}

\item{max.ucl}{Confidence intervals are omitted (replaced by a bar) if the
upper limit exeeds this value.}
}
\value{
The return value is an S3 object of class \code{ucr.model.tab}.
}
\description{
Creates a table to summarize one or several analysis models.
}
\author{
Lars Lindhagen
}
