#' Set working dir [PC]
#'
#' This function allows you to go straight to the appropriate working directory [for PC] 
#' when working locally.
#' @export
wd<-function(){
  setwd("C:/Users/Kaui Yogi/Dropbox/SCHOOL/2015-2016 Spring Stuff/MATH 699")
}
#' Set working dir [Mac]
#'
#' This function allows you to go straight to the appropriate working directory [for Mac] 
#' when working locally.
#' @export
wd.mac<-function(){
  setwd("~/Dropbox/SCHOOL/2015-2016 Spring Stuff/MATH 699")
}
#' Set working dir as autocpx subdir
#'
#' This function allows you to go set the working directory to the "autocpx" subdirectory.
#' This is necessary to use the document() function in devtools.
#' @export
autowd<-function(){
  setwd("./autocpx")
}
#' Reinstall local version of autocpx.
#'
#' This function reinstalls autocpx from local data.
#' @export
icpx<-function(){
  devtools:::install("autocpx")
}
#' Update documentation and reinstall.
#'
#' This function updates the documentation and reinstalls autocpx from local memory.
#' @param OS Enter "pc" if PC and "mac" if mac.
#' @export
upd<-function(OS="pc"){
  if(OS!="pc"&OS!="mac"){stop("Invalid OS.")}
  if(OS=="pc"){wd();autowd();devtools:::document();wd();devtools:::install("autocpx")}
  if(OS=="mac"){wd.mac();autowd();devtools:::document();wd.mac();devtools:::install("autocpx")}
}