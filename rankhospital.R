rankhospital<-function(state,outcome,num="best"){
  dat<-read.csv(file = "outcome-of-care-measures.csv",colClasses = "character",na.strings = c("NA","Not Available"))
  if(!any(dat$State==state)){
    stop("No vaild state")
  }
  if(outcome=="heart attack"){
    i<-11
  }
  else if(outcome=="heart failure"){
    i<-17
  }
  else if(outcome=="pneumonia"){
    i<-23
  }
  else{
    stop("No vaild outcome")
  }
  h.rates<-dat[dat$State==state,]
  h.rates[,i]<-as.numeric(x=h.rates[,i])
  h.rates<-h.rates[complete.cases(h.rates),]
  if(num=="best"){
    num<-1
  }
  if(num=="worst"){
    num<-nrow(h.rates)
  }
  h.rates.name<-h.rates[order(h.rates[,i],h.rates$Hospital.Name),]$Hospital.Name
  return(h.rates.name[num])
}







