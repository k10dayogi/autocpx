#' Set working dir
#'
#' This function allows you to go straight to the appropriate working directory
#' when working locally.
#' @param OS Default pc. If on the Mac, use \code{mac} as the argument.
#' @export
wd<-function(OS="pc"){
  if(OS!="pc"&OS!="mac"){stop("Invalid OS.")}
  if(OS=="pc"){setwd("C:/Users/Kaui Yogi/Dropbox/RESEARCH/MA Research")}
  if(OS=="mac"){setwd("~/Dropbox/RESEARCH/MA Research")}
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
#' Public WD
#'
#' This function sets WD to the "public" directory.
#' @param OS Enter "pc" if PC and "mac" if mac.
#' @export
pubwd<-function(OS="pc"){
  if(OS=="pc"){setwd("C:/Users/Kaui Yogi/Dropbox/Public")}
  if(OS=="mac"){setwd("~/Dropbox/Public")}
}