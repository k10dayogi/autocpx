#' Conditional probability table
#'
#' This function allows the user to pull the table of conditional probabilities into R and 
#' allows the user to work with the data and see the conditional probability table.
#' @details The table that is pulled in comes from an online source. Caching is available by default
#' so the user doesn't have to constantly connect to pull the table in.
#' @param cache.value Default TRUE. If you want to pull off of the web again, set to FALSE.
#' @return Returns three data frames: One is \code{prb} which is the full table of strings, deficiencies, and movement factors.
#' One is \code{tcpx} which shows probabilities of increase and decrease at various complexity levels. The final is \code{tlen} which shows
#' probabilties of increase and decrease at various string lengths.
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
  tb.0<-c(length(which(def.0$`C|0`=="Dec"))+length(which(def.0$`C|1`=="Dec")),length(which(def.0$`C|0`=="Con"))+length(which(def.0$`C|1`=="Con")),length(which(def.0$`C|0`=="Inc"))+length(which(def.0$`C|1`=="Inc")))/(2*nrow(def.0))*100
  tb.1<-c(length(which(def.1$`C|0`=="Dec"))+length(which(def.1$`C|1`=="Dec")),length(which(def.1$`C|0`=="Con"))+length(which(def.1$`C|1`=="Con")),length(which(def.1$`C|0`=="Inc"))+length(which(def.1$`C|1`=="Inc")))/(2*nrow(def.1))*100
  tb.2<-c(length(which(def.2$`C|0`=="Dec"))+length(which(def.2$`C|1`=="Dec")),length(which(def.2$`C|0`=="Con"))+length(which(def.2$`C|1`=="Con")),length(which(def.2$`C|0`=="Inc"))+length(which(def.2$`C|1`=="Inc")))/(2*nrow(def.2))*100
  tb.3<-c(length(which(def.3$`C|0`=="Dec"))+length(which(def.3$`C|1`=="Dec")),length(which(def.3$`C|0`=="Con"))+length(which(def.3$`C|1`=="Con")),length(which(def.3$`C|0`=="Inc"))+length(which(def.3$`C|1`=="Inc")))/(2*nrow(def.3))*100
  tb.4<-c(length(which(def.4$`C|0`=="Dec"))+length(which(def.4$`C|1`=="Dec")),length(which(def.4$`C|0`=="Con"))+length(which(def.4$`C|1`=="Con")),length(which(def.4$`C|0`=="Inc"))+length(which(def.4$`C|1`=="Inc")))/(2*nrow(def.4))*100
  tb.5<-c(length(which(def.5$`C|0`=="Dec"))+length(which(def.5$`C|1`=="Dec")),length(which(def.5$`C|0`=="Con"))+length(which(def.5$`C|1`=="Con")),length(which(def.5$`C|0`=="Inc"))+length(which(def.5$`C|1`=="Inc")))/(2*nrow(def.5))*100
  tb.6<-c(length(which(def.6$`C|0`=="Dec"))+length(which(def.6$`C|1`=="Dec")),length(which(def.6$`C|0`=="Con"))+length(which(def.6$`C|1`=="Con")),length(which(def.6$`C|0`=="Inc"))+length(which(def.6$`C|1`=="Inc")))/(2*nrow(def.6))*100
  tb.7<-c(length(which(def.7$`C|0`=="Dec"))+length(which(def.7$`C|1`=="Dec")),length(which(def.7$`C|0`=="Con"))+length(which(def.7$`C|1`=="Con")),length(which(def.7$`C|0`=="Inc"))+length(which(def.7$`C|1`=="Inc")))/(2*nrow(def.7))*100
  tab<-as.data.frame(rbind(tb.0,tb.1,tb.2,tb.3,tb.4,tb.5,tb.6,tb.7))
  tab<-as.data.frame(cbind(0:7,formatC(tab[,1],digits=2,format="f"),formatC(tab[,2],digits=2,format="f"),
                            formatC(tab[,3],digits=2,format="f")))
  names(tab)<-c("Def","P(Dec)","P(Con)","P(Inc)")
  for(i in 1:ncol(tab)){tab[,i]<-as.numeric(as.character(tab[,i]))}
  tcpx<<-tab
  print(tcpx,row.names = FALSE)
##########
  l.0<-subset(prb,nchar(prb[,1])==0)
  l.1<-subset(prb,nchar(prb[,1])==1)
  l.2<-subset(prb,nchar(prb[,1])==2)
  l.3<-subset(prb,nchar(prb[,1])==3)
  l.4<-subset(prb,nchar(prb[,1])==4)
  l.5<-subset(prb,nchar(prb[,1])==5)
  l.6<-subset(prb,nchar(prb[,1])==6)
  l.7<-subset(prb,nchar(prb[,1])==7)
  l.8<-subset(prb,nchar(prb[,1])==8)
  l.9<-subset(prb,nchar(prb[,1])==9)
  l.10<-subset(prb,nchar(prb[,1])==10)
  l.11<-subset(prb,nchar(prb[,1])==11)
  l.12<-subset(prb,nchar(prb[,1])==12)
  l.13<-subset(prb,nchar(prb[,1])==13)
  l.14<-subset(prb,nchar(prb[,1])==14)
  td.0<-c(length(which(l.0$`C|0`=="Dec"))+length(which(l.0$`C|1`=="Dec")),length(which(l.0$`C|0`=="Con"))+length(which(l.0$`C|1`=="Con")),length(which(l.0$`C|0`=="Inc"))+length(which(l.0$`C|0`=="Inc")))/(2*nrow(l.0))*100
  td.1<-c(length(which(l.1$`C|0`=="Dec"))+length(which(l.1$`C|1`=="Dec")),length(which(l.1$`C|0`=="Con"))+length(which(l.1$`C|1`=="Con")),length(which(l.1$`C|0`=="Inc"))+length(which(l.1$`C|0`=="Inc")))/(2*nrow(l.1))*100
  td.2<-c(length(which(l.2$`C|0`=="Dec"))+length(which(l.2$`C|1`=="Dec")),length(which(l.2$`C|0`=="Con"))+length(which(l.2$`C|1`=="Con")),length(which(l.2$`C|0`=="Inc"))+length(which(l.2$`C|0`=="Inc")))/(2*nrow(l.2))*100
  td.3<-c(length(which(l.3$`C|0`=="Dec"))+length(which(l.3$`C|1`=="Dec")),length(which(l.3$`C|0`=="Con"))+length(which(l.3$`C|1`=="Con")),length(which(l.3$`C|0`=="Inc"))+length(which(l.3$`C|0`=="Inc")))/(2*nrow(l.3))*100
  td.4<-c(length(which(l.4$`C|0`=="Dec"))+length(which(l.4$`C|1`=="Dec")),length(which(l.4$`C|0`=="Con"))+length(which(l.4$`C|1`=="Con")),length(which(l.4$`C|0`=="Inc"))+length(which(l.4$`C|0`=="Inc")))/(2*nrow(l.4))*100
  td.5<-c(length(which(l.5$`C|0`=="Dec"))+length(which(l.5$`C|1`=="Dec")),length(which(l.5$`C|0`=="Con"))+length(which(l.5$`C|1`=="Con")),length(which(l.5$`C|0`=="Inc"))+length(which(l.5$`C|0`=="Inc")))/(2*nrow(l.5))*100
  td.6<-c(length(which(l.6$`C|0`=="Dec"))+length(which(l.6$`C|1`=="Dec")),length(which(l.6$`C|0`=="Con"))+length(which(l.6$`C|1`=="Con")),length(which(l.6$`C|0`=="Inc"))+length(which(l.6$`C|0`=="Inc")))/(2*nrow(l.6))*100
  td.7<-c(length(which(l.7$`C|0`=="Dec"))+length(which(l.7$`C|1`=="Dec")),length(which(l.7$`C|0`=="Con"))+length(which(l.7$`C|1`=="Con")),length(which(l.7$`C|0`=="Inc"))+length(which(l.7$`C|0`=="Inc")))/(2*nrow(l.7))*100
  td.8<-c(length(which(l.8$`C|0`=="Dec"))+length(which(l.8$`C|1`=="Dec")),length(which(l.8$`C|0`=="Con"))+length(which(l.8$`C|1`=="Con")),length(which(l.8$`C|0`=="Inc"))+length(which(l.8$`C|0`=="Inc")))/(2*nrow(l.8))*100
  td.9<-c(length(which(l.9$`C|0`=="Dec"))+length(which(l.9$`C|1`=="Dec")),length(which(l.9$`C|0`=="Con"))+length(which(l.9$`C|1`=="Con")),length(which(l.9$`C|0`=="Inc"))+length(which(l.9$`C|0`=="Inc")))/(2*nrow(l.9))*100
  td.10<-c(length(which(l.10$`C|0`=="Dec"))+length(which(l.10$`C|1`=="Dec")),length(which(l.10$`C|0`=="Con"))+length(which(l.10$`C|1`=="Con")),length(which(l.10$`C|0`=="Inc"))+length(which(l.10$`C|0`=="Inc")))/(2*nrow(l.10))*100
  td.11<-c(length(which(l.11$`C|0`=="Dec"))+length(which(l.11$`C|1`=="Dec")),length(which(l.11$`C|0`=="Con"))+length(which(l.11$`C|1`=="Con")),length(which(l.11$`C|0`=="Inc"))+length(which(l.11$`C|0`=="Inc")))/(2*nrow(l.11))*100
  td.12<-c(length(which(l.12$`C|0`=="Dec"))+length(which(l.12$`C|1`=="Dec")),length(which(l.12$`C|0`=="Con"))+length(which(l.12$`C|1`=="Con")),length(which(l.12$`C|0`=="Inc"))+length(which(l.12$`C|0`=="Inc")))/(2*nrow(l.12))*100
  td.13<-c(length(which(l.13$`C|0`=="Dec"))+length(which(l.13$`C|1`=="Dec")),length(which(l.13$`C|0`=="Con"))+length(which(l.13$`C|1`=="Con")),length(which(l.13$`C|0`=="Inc"))+length(which(l.13$`C|0`=="Inc")))/(2*nrow(l.13))*100
  td.14<-c(length(which(l.14$`C|0`=="Dec"))+length(which(l.14$`C|1`=="Dec")),length(which(l.14$`C|0`=="Con"))+length(which(l.14$`C|1`=="Con")),length(which(l.14$`C|0`=="Inc"))+length(which(l.14$`C|0`=="Inc")))/(2*nrow(l.14))*100
  tab.2<-as.data.frame(rbind(td.0,td.1,td.2,td.3,td.4,td.5,td.6,td.7,td.8,td.9,td.10,td.11,td.12,td.13,td.14))
  tab.2<-as.data.frame(cbind(0:14,formatC(tab.2[,1],digits=2,format="f"),formatC(tab.2[,2],digits=2,format="f"),
                              formatC(tab.2[,3],digits=2,format="f")))
  names(tab.2)<-c("Length","P(Dec)","P(Con)","P(Inc)")
  tlen<<-tab.2
  print(tlen,row.names = FALSE)
}