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

#' megacor presents a correlation table with significance indicators
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

  z <- lsr::correlate(x,y,test=TRUE,corr.method=corr.method,p.adjust.method=p.adjust.method)

  R <- as.matrix(z[[1]])
  p <- as.matrix(z[[2]])

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))

  R <- as.matrix(round_df(R))

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(z[[1]]))
  diag(Rnew) <- paste(diag(R), " ", sep="")
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
    Rnew <- as.matrix(Rnew)
    #Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  if (result[1]=="none") print(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex")
  }
}
