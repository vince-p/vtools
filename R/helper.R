# VP Helper Functions
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
pv<- function (p, method = "none", n = length(p)) {
  p <- p.adjust(p, method, n)
  ifelse (p < 5e-04, "< 0.001", as.character(paste0(p %>% formatC(digits = 3, format = "f"))))
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


#' fq displays counts and percentages of each value in a vector
#'
#' @param x A vector
#' @return dataframe with counts and percentages
#' @examples
#' tocome
#' @export
fq <- function(x,dp=1) {
  a <- table(x)
  b <- r(table(x)/sum(table(x))*100,dp)
  c <- rbind(count=a,"%"=b)
  c
}

#' ecor displays a neatly formatted correlation table for a dataframe
#'
#' @param data A dataframe
#' @param digits Number of digits to report (default =3)
#' @param method correlation method, any of c("pearson", "kendall", "spearman"
#' @return dataframe with correlations
#' @examples
#' ecor(df,2,"spearman")
#' @export

ecor<-function(data,digits=3,method="pearson"){
  x<-cor(data, method=method)
  x[upper.tri(x)] <- NA # erase the upper triangle
  diag(x) <- NA # erase the diagonal 0's
  round_df(x,digits)
}
