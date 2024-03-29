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
#'
#' @param x vector to be rounded.
#' @param dp Number of decimal places (defaults to 2).
#' @return Rounded output
#' @examples
#' r(4.66544645454)
#' r(somevector, 3)
#' @export
r<-function(x, dp=2){ #easy rounding
  format(round(as.numeric(x),dp),nsmall=dp)
}


#' Rounds values in a dataframe
#'
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

#' pv2 neatly formats the significance of a p-value with equals sign
#'
#' @param x A p-value
#' @return Neatly formatted p-output
#' @examples
#' pv2(dataobject$p.value)
#' @export
pv2<- function (p, method = "none", n = length(p)) {
  p <- p.adjust(p, method, n)
  ifelse (p < 5e-04, "< 0.001", as.character(paste0("= ", p %>% formatC(digits = 3, format = "f"))))
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


#' Shows the most common value in a vector
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


#' WTF is in this dataframe
#'
#' Uses excel to view R objects. Stolen from the internet somewhere.
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


#' Frequency counts and percentages
#'
#' Displays a tally of counts and percentages of each value in a vector
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

#' Generate a correlation table
#'
#' Creates a nicely formatted correlation table with significance indicators.
#' uses lsr:correlate and adapts a function from http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#'
#' @param x A matrix or dataframe containing variables to be correlated
#' @param y Optionally, a second set of variables to be correlated with those in x
#' @param corr.method What kind of correlations should be computed? Default is "pearson", but "spearman" and "kendall" are also supported
#' @param removeTriangle Specifies the formatting of the output. Which section of the table to remove c("none", "upper", "lower")
#' @param result Format for output c("none", "html", "latex")
#' @return dataframe with correlations
#' @examples
#' megacor(df)
#' megacor(df[1:4],df[5:8],corr.method = "spearman", removeTriangle = "upper", result = "html")
#' @importFrom xtable xtable
#' @importFrom lsr correlate
#' @export
megacor <-function(x,y=NULL,corr.method="pearson",p.adjust.method="holm", removeTriangle=c("none","upper", "lower"),
                   result=c("none", "html", "latex")){

  z <- lsr::correlate(as.data.frame(x),as.data.frame(y),test=TRUE,corr.method=corr.method,p.adjust.method=p.adjust.method)

  R <- as.matrix(z[[1]])
  p <- as.matrix(z[[2]])

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   ")))

  R <- as.matrix(round_df(R))

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(z[[1]]))
  rownames(Rnew) <- rownames(z[[1]])
  colnames(Rnew) <- colnames(z[[1]])

  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
    Rnew <- Rnew[-1,1:length(Rnew)-1]
  }

  ## remove lower triangle of correlation matrix
  if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
    Rnew <- Rnew[1:length(Rnew)-1,-1]
  }

  else if(removeTriangle[1]=="none"){ # I ADDED THIS TO KEEP THE WHOLE TABLE
    Rnew <- as.data.frame(Rnew)
  }

  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex")
  }
}

#' Select based on multiple matches (multiple contains)
#'
#' Select a subset of columns based on multiple search strings
#'
#' @param data A dataframe
#' @param matchvector A vector of search terms
#' @return dataframe with selected cols
#' @examples
#' tocome
#' @export
vselect <- function(data,matchvector){
  matchExpression <- paste(matchvector, collapse = "|")
  select(data,matches(matchExpression))
}


#' Nicely formatted table
#'
#' Displays a table with values less than 0.001 represented as "<.001". All other numeric values displayed to 3dp.
#'
#' @param x A dataframe
#' @return A nicely formatted dataframe
#' @examples
#' tocome
#' @export
nicetable <- function(table){
  as.data.frame(lapply(table, function(x) {
    if (is.numeric(x)) {
      vapply(x,
             function(y) {
               ifelse(abs(y) < 0.001, "<.001", sprintf("%.3f", y))
             },
             character(1))
    } else {
      x
    }
  }))
}
