## Set workding dir and bring in reqd packages
#require(openxlsx)
#setwd("C:/Users/Kaui Yogi/Dropbox/SCHOOL/2015-2016 Spring Stuff/MATH 699") 
#' Deficiency simulator
#'
#' This function allows you to randomly generate a 16-length string and plot the deficiency at each point.
#' @param n Default 1.Number of simulations to generate.
#' @param file Default FALSE. If the plot is being printed to file, use file=TRUE.
#' @param start Default 1. If you want to start the file numbering at a number other than 1, specify it here.
#' path()
#' @export
path<-function(n=1,file=FALSE,start=1){
  offline<<-FALSE
  if(file==TRUE & dir.exists("./autoplot")==TRUE){
    dx<<-getwd(); setwd(paste(dx,"/autoplot",sep=""))
  }
  if (file==TRUE & dir.exists("./autoplot")==FALSE){
    dx<<-getwd();dir.create("./autoplot"); setwd("./autoplot")
  }
    suppressMessages(autocpx:::dat())

  for(j in 1:n){                 #this for loop cycles over the number of simulations.
    z.0<<-vector("character",15)
    z.1<<-vector("numeric",15)
    b<<-as.character(sample(c(0,1),1))
    b->>z.0[1]
    z.1[1]<<-data.sh[grep(paste("^",b,"$",sep=""),data.sh[,1]),4]
      for(i in 2:16){                #this for loop cycles over the max length of the strings in database.
         autocpx:::nxt(b)->>b
         z.0[i]<<-b
         z.1[i]<<-data.sh[grep(paste("^",b,"$",sep=""),data.sh[,1]),4]
      }
    db<<-as.data.frame(cbind(z.0,z.1));names(db)<<-c("String","Deficiency")
    db[,2]<<-as.numeric(as.character(db[,2]))
    print(db)
  #plot commands here.
    p<<-ggplot2::qplot(1:16,db[,2])+ggplot2::geom_line()
    print(p)
      if(file==TRUE){
          for(i in (start):(start+n-1)){
            jpeg(paste("autoplot",(start+(j-1)),".jpeg",sep=""))
            print(p)
            dev.off()
          }
      }
    }
  if(file==TRUE){
    setwd(dx)
  }
}