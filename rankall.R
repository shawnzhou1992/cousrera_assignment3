rankall<-function(outcome,num="best"){
  #读取数据
  read.csv(file = "outcome-of-care-measures.csv",colClasses = "character",na.strings = c("NA","Not Available"))
  #check数据
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
    stop("invaild outcome")
  }
  h.rank<-data.frame()
  for (state in unique(dat$State)){
    dat.state<-dat[dat$State==state,]
    dat.state[,i]<-as.numeric(x=dat.state[,i])
    dat.state<-dat.state[!is.na(dat.state[,i]),]
    if(num=="best"){
      rnum<-1
    }
    else if(num=="worst"){
      rnum<-nrow(dat.state)
    }
    else{
      rnum=num
    }
    dat.state.name<-dat.state[order(dat.state[,i],dat.state$Hospital.Name),]$Hospital.Name
    h.name<-dat.state.name[rnum]
    h.rank<-rbind(h.rank,data.frame(hospital=h.name,state=state))
  }
  h.rank<-h.rank[order(h.rank$state),]
  return(h.rank)
}