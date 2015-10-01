best <- function(state, outcome){
  ##read in our data frame, states, and conditions
  mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("Not Available"))
  allinfo <- mydata[ ,c(2,7,11,17,23)]
  conditions <- c("heart attack", "heart failure", "pneumonia")
  loc <- as.vector(unique(mydata$State))
  
  if(outcome %in% conditions){
    ##print("valid outcome")
    if(state %in% loc){
      
      ##print("valid state")
      ###meat of program goes here
      
      mystate <- subset(allinfo, allinfo$State == state)
      
      
      ##create a character vector that assigns possible outcomes to the correct column entries in mystate
      
      ##colnum <- c("heart attack" = "mystate[,11]", "heart failure" = "mystate[,17]", "pneumonia" = "mystate[,23]")
      
      ##Now, create another if else loop to see if we can do something with the outcomes
      if(outcome == "heart attack"){
        ##create a dataframe of just the heart attack
        info <- mystate[,c(1,2,3)]
        good <- complete.cases(info)
        goodinf <- info[good,]
        ##work on this to get the order correct
        a <- goodinf[order(as.numeric(goodinf[,3]),goodinf[,1]),]
        print(a[1, 1])
      }else if(outcome == "heart failure"){
        info <- mystate[,c(1,2,4)]
        good <- complete.cases(info)
        goodinf <- info[good,]
        ##work on this to get the order correct
        a <- goodinf[order(as.numeric(goodinf[,3]),goodinf[,1]),]
        print(a[1, 1])
      }else if(outcome == "pneumonia"){
        info <- mystate[,c(1,2,5)]
        good <- complete.cases(info)
        goodinf <- info[good,]
        ##work on this to get the order correct
        a <- goodinf[order(as.numeric(goodinf[,3]),goodinf[,1]),]
        print(a[1, 1])
      }
      
      
      
    } else {
      stop("invalid state")
    }
    
  } else {
    stop("invalid outcome")
  }
  ##print(str(mystate))
}
