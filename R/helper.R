# VP Helper Functions
#
# 10/2/18 v0.01
# initial package
#
# 14/2/18 v0.02
# Set round_df to default to 2dp
# Edit v.test so that y defaults to NULL
# Added correction adjustments to pv
#
# run devtools::document() to build
#############
# HELPER FUNCTIONS

#' A better t-test
#'
#' @param x First vector to compare.
#' @param y Second vector to compare.
#' @param paired Set FALSE (default) for independent test, TRUE for paired test
#' @return Neat t-test output
#' @examples
#' v.test(somevar, someothervar)
#' @export
v.test<-function(x,y=NULL,paired=FALSE){
  t<-t.test(x,y,paired=paired)
  return(paste0("t = ",r(t$statistic),", p = ",pv(t$p.value),", 95% CI = [",r(t$conf.int[1]),", ",r(t$conf.int[2]),"]"))
}

#' Quick rounding
#' Updated 11/2/18
#'
#' @param x vector to be rounded.
#' @param dp Number of decimal places (defaults to 2).
#' @return Rounded output
#' @examples
#' r(4.66544645454)
#' r(somevector, 3)
#' @export
r<-function(x, dp=2){ #easy rounding
  as.numeric(format(round(x,dp),nsmall=dp))
}

#' round_df function from http://jeromyanglim.tumblr.com/post/50228877196/round-numbers-in-data-frame-that-contains-non
#'
#' @param x a dataframe.
#' @param digits Number of digits to round to.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' tocome
#' @export
round_df <- function(x, digits=2) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  print(x)
}

#' pv neatly formats the significance of a p-value
#'
#' @param x A p-value
#' @return Neatly formatted p-output
#' @examples
#' pv(dataobject$p.value)
#' @export
pv<-function(x,correction="none"){
  x<-p.adjust(x, correction)
  if (x < 0.0005) { # adapted from http://my.ilstu.edu/~wjschne/444/IndependentSamples.html#(18). Annoying to have to do this!
    x <- "< 0.001"
  } else {
    x <- as.character(paste0(x %>% formatC(digits = 3, format = "f")))
  }
  return(x)
}

#' SimpleCap capitalises all first letters in a string
#'
#' @param x A string
#' @return Capitalised String
#' @examples
#' tocome
#' @export
simpleCap <- function(x) { #capitalise all first letters
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#' vmode shows the most common value in a vector
#'
#' @param x A vector
#' @return mode value within the vector
#' @examples
#' tocome
#' @export
vmode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
