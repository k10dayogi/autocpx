#' Conditional probability table
#'
#' This function allows the user to pull the table of conditional probabilities into R and 
#' allows the user to work with the data and see the conditional probability table.
#' @details The table that is pulled in comes from an online source. Caching is available by default
#' so the user doesn't have to constantly connect to pull the table in.
#' @param cache.value Default TRUE. If you want to pull off of the web again, set to FALSE.
#' @return Returns two data frames: One is \code{prb} which is the full table of strings, deficiencies, and movement factors.
#' The other is \code{tab} which shows probabilities of increase and decrease at various levels.
#' @export
cprb<-function(cache.value=TRUE){
  require(bit64)
  pb<-repmis:::source_data("https://dl.dropboxusercontent.com/u/13235684/pb.csv",cache=cache.value)
  prb<<-pb
  prb[,1]<<-gsub("s","",prb[,1])
  names(prb)[5]<<-"D|0";names(prb)[6]<<-"D|1";names(prb)[7]<<-"C|0";names(prb)[8]<<-"C|1"
  prb$`C|0`<<-as.factor(prb$`C|0`);prb$`C|1`<<-as.factor(prb$`C|1`)
#######
  def.0<-subset(prb,prb$def==0)
  def.1<-subset(prb,prb$def==1)
  def.2<-subset(prb,prb$def==2)
  def.3<-subset(prb,prb$def==3)
  def.4<-subset(prb,prb$def==4)
  def.5<-subset(prb,prb$def==5)
  def.6<-subset(prb,prb$def==6)
  def.7<-subset(prb,prb$def==7)
#######
  tb.0<-c(length(which(def.0$`C|0`=="Dec")),length(which(def.0$`C|0`=="Con")),length(which(def.0$`C|0`=="Inc")))/nrow(def.0)*100
  tb.1<-c(length(which(def.1$`C|0`=="Dec")),length(which(def.1$`C|0`=="Con")),length(which(def.1$`C|0`=="Inc")))/nrow(def.1)*100
  tb.2<-c(length(which(def.2$`C|0`=="Dec")),length(which(def.2$`C|0`=="Con")),length(which(def.2$`C|0`=="Inc")))/nrow(def.2)*100
  tb.3<-c(length(which(def.3$`C|0`=="Dec")),length(which(def.3$`C|0`=="Con")),length(which(def.3$`C|0`=="Inc")))/nrow(def.3)*100
  tb.4<-c(length(which(def.4$`C|0`=="Dec")),length(which(def.4$`C|0`=="Con")),length(which(def.4$`C|0`=="Inc")))/nrow(def.4)*100
  tb.5<-c(length(which(def.5$`C|0`=="Dec")),length(which(def.5$`C|0`=="Con")),length(which(def.5$`C|0`=="Inc")))/nrow(def.5)*100
  tb.6<-c(length(which(def.6$`C|0`=="Dec")),length(which(def.6$`C|0`=="Con")),length(which(def.6$`C|0`=="Inc")))/nrow(def.6)*100
  tb.7<-c(length(which(def.7$`C|0`=="Dec")),length(which(def.7$`C|0`=="Con")),length(which(def.7$`C|0`=="Inc")))/nrow(def.7)*100
  tab<<-as.data.frame(rbind(tb.0,tb.1,tb.2,tb.3,tb.4,tb.5,tb.6,tb.7))
  tab<<-as.data.frame(cbind(formatC(tab[,1],digits=2,format="f"),formatC(tab[,2],digits=2,format="f"),
                            formatC(tab[,3],digits=2,format="f")))
  names(tab)<<-c("P(Dec)","P(Con)","P(Inc)");row.names(tab)<<-0:7
}