#' Import probability table.
#'
#' This function allows the user to pull the table of conditional probabilities into R and perform 
#' tranformations necessary for further analysis.
#' @details The table that is pulled in comes from an online source. Caching is available by default
#' so the user doesn't have to constantly connect to pull the table in.
#' @param cache.value Default TRUE. If you want to pull off of the web again, set to FALSE.
#' @return The name of the data frame generated is \code{prb}. Once the function has completed execution,
#' the element is available for use.
#' @export
cprb<-function(cache.value=TRUE){
  require(bit64)
  prb<<-repmis:::source_data("https://dl.dropboxusercontent.com/u/13235684/dx.csv",cache=cache.value)
  prb<<-prb[,2:ncol(prb)]
  prb[,1]<<-gsub("s","",prb[,1])
  x<-which(nchar(prb[,1])==16);prb<<-prb[-x,]
  names(prb)[5]<<-"D|0";names(prb)[6]<<-"D|1"
}