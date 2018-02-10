# VP Helper Functions
# 10/2/18

# if (!require("pacman")) install.packages("pacman")
# library(pacman)
# p_load(tidyverse,psych)  

#############
# HELPER FUNCTIONS

v.test<-function(x,y,paired=FALSE){
  t<-t.test(x,y,paired=paired)
  return(paste0("t = ",r(t$statistic),", p = ",pv(t$p.value),", 95% CI = [",r(t$conf.int[1]),", ",r(t$conf.int[2]),"]"))
}

r<-function(x, dp=2){ #easy rounding
  as.numeric(format(round(x,dp),nsmall=dp))
}

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

pv<-function(x){
  if (x < 0.0005) { # adapted from http://my.ilstu.edu/~wjschne/444/IndependentSamples.html#(18). Annoying to have to do this!
    x <- "< 0.001" 
  } else { 
    x <- as.character(paste0(x %>% formatC(digits = 3, format = "f"))) 
  }
  return(x)
}

#############

