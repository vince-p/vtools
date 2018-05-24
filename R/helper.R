# VP Helper Functions
#
# 24/5/18
# Removed anon.csv  This is better suited to specific scripts
#
# 23/5/18
# Added anon.csv function for removing identifying information from Qualtrics csv files
# 10/2/18 v0.01
# initial package
#
# 14/2/18 v0.02
# Set round_df to default to 2dp
# Edit v.test so that y defaults to NULL
# Added correction adjustments to pv
#
# 4/5/18
# Add WTF function
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
pv<-function(p,correction="none",n=length(p)){
  p<-p.adjust(p, correction,n)
  if (p < 0.0005) { # adapted from http://my.ilstu.edu/~wjschne/444/IndependentSamples.html#(18). Annoying to have to do this!
    p <- "< 0.001"
  } else {
    p <- as.character(paste0(p %>% formatC(digits = 3, format = "f")))
  }
  return(p)
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


#' wtf is stolen from the internet somewhere. Uses excel to view objects
#'
#' @param x A dataframe (or similar?)
#' @return opens in excel
#' @examples
#' tocome
#' @export
wtf <- function (x) {
  tempFilePath = paste(tempfile(), ".csv")
  tempPath = dirname(tempFilePath)
  preferredFile = paste(deparse(substitute(x)), ".csv", sep = "")
  preferredFilePath = file.path(tempPath, preferredFile)

  if(length(dim(x))>2){
    stop('Too many dimensions')
  }
  if(is.null(dim(x))){
    x = as.data.frame(x)
  }
  if (is.null(rownames(x))) {
    tmp = 1:nrow(x)
  }else {
    tmp = rownames(x)
  }
  rownames(x) = NULL
  x = data.frame(RowLabels = tmp, x)
  WriteAttempt = try(
    write.table(x, file=preferredFilePath, quote=TRUE, sep=",", na="",
                row.names=FALSE, qmethod="double"),
    silent = TRUE)
  if ("try-error" %in% class(WriteAttempt)) {
    write.table(x, file=tempFilePath, , quote=TRUE, sep=",", na="",
                row.names=FALSE, qmethod="double")
    shell.exec(tempFilePath)
  } else {
    shell.exec(preferredFilePath)
  }
}
