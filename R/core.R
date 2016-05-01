## Set workding dir and bring in reqd packages
#require(openxlsx)
#setwd("C:/Users/Kaui Yogi/Dropbox/SCHOOL/2015-2016 Spring Stuff/MATH 699")
#setwd("~/Dropbox/SCHOOL/2015-2016 Spring Stuff/MATH 699") #[Mac]
## 

##Bring in data, cut it down.
#data.long<-read.xlsx("MasterThesis(7) r=.25 n=16 with discounted values.xlsx",1)
#data<-data.long[,c(2:3)]
## 
##################################################################################################################
#' Data retreival function
#'
#' This function automatically loads data and applies appropriate transformations.
#' v()
dat<-function(){
  offline<<-FALSE
  if(offline==FALSE){
    require(bit64)
    data.long<<-repmis:::source_data("https://dl.dropboxusercontent.com/u/13235684/MasterThesis(7)%20r%3D.25%20n%3D16%20with%20discounted%20values.csv",sep=",",cache=TRUE)
  } else {
    setwd("C:/Users/Kaui Yogi/Dropbox/SCHOOL/2015-2016 Spring Stuff/MATH 699")
    data.long<<-read.csv("MasterThesis(7) r=.25 n=16 with discounted values.csv")
  }
  data.sh<<-data.long[,c(1,3)];data.sh[,1]<<-gsub("s","",data.sh[,1])
  
  ## Add columns for string length
  length.st<<-vector(length=nrow(data.sh),"numeric")
  for(i in 1:nrow(data.sh)){length.st[i]<<-nchar(data.sh[i,1])}
  data.ext<<-as.data.frame(cbind(data.sh,length.st))
  names(data.ext)[length(names(data.ext))]<<-"St.length"
  names(data.ext)[1]<<-"String"
  c("String","Complexity")->>names(data.sh)
  data.ext<<-data.ext[,c(1,3)]
  ##
  #Add column for deficiency. Calculate maximal complexity first!
  mx<<-vector(length=nrow(data.sh),"numeric")
  def<<-vector(length=nrow(data.sh),"numeric")
  for (i in 1:nrow(data.sh)){
    mx[i]<<-floor(nchar(data.sh[i,1])/2)+1
  }
  for (j in 1:length(mx)){
    def[j]<<-(mx[j]-data.sh[j,2])
  }
  data.sh<<-as.data.frame(cbind(data.sh,mx,def))
}
##
##Define subroutine to generate strings!
#####
#' Random string function.
#'
#' This function generates binary strings.
#' @param x length of string to generate.
#' nxt()
nxt<-function(x){
  a<-sample(c(0,1),1)
  x<-paste(x,as.character(a),sep="")
}
##
## Define subroutine to generate names of vectors systematically.
## Note that k refers to the number of steps ahead.
###
#' Name-generating function.
#'
#' This function allows you to generate names of vectors systematically.
#' @param k Number of bits you want to "look ahead."
#' @param n Length of the string (number of iterations.)
#' v()
v<-function(k,n=1){
  offline<<-FALSE
  suppressMessages(autocpx:::dat())
  if(k>15){stop("k must be no more than 15!")}
  if (k<=0) {stop("Values of k must be positive integers.")}
  if (as.integer(k)!=k){stop("Values of k must be positive integers!")}
  sb<<-subset(data.ext,data.ext$St.length<=k)
  x<-1
  vectorz<<-vector("character",nrow(sb))
  #initalize vectors
  for (j in 1:nrow(sb)){ 
    vectorx<<-vector("numeric",n)
    assign(paste("d.",sb$String[j],sep=""),vectorx,pos=1)
    vectorz[j]<<-sb$String[j]
  }
  remove(vectorx,pos=1)
}