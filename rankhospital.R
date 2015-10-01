rankhospital <- function(state, outcome, num = "best"){
  ##read in our data frame, states, and conditions
  mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("Not Available"))
  allinfo <- mydata[ ,c(2,7,11,17,23)]
  conditions <- c("heart attack", "heart failure", "pneumonia")
  loc <- as.vector(unique(mydata$State))
  if(num == "best"){num <- 1}
  
  if(outcome %in% conditions){
    
    
    if(state %in% loc){
      
      ###meat of program goes here
      
      mystate <- subset(allinfo, allinfo$State == state)
      
      
      ##Now, create another if else loop to see if we can do something with the outcomes
      if(outcome == "heart attack"){
        ##create a dataframe of just the heart attack
        info <- mystate[,c(1,2,3)]
        good <- complete.cases(info)
        goodinf <- info[good,]
        ##work on this to get the order correct
        a <- goodinf[order(as.numeric(goodinf[,3]),goodinf[,1]),]
        rank <- c(1:nrow(a))
        w <- length(rank)
        if(num == "worst"){num <- w}
        goodrank <- cbind(a, rank)
        if(num > w){
          print("NA")
        }else{
          print(goodrank[num,1])
          }
        
      }else if(outcome == "heart failure"){
        info <- mystate[,c(1,2,4)]
        good <- complete.cases(info)
        goodinf <- info[good,]
        ##work on this to get the order correct
        a <- goodinf[order(as.numeric(goodinf[,3]),goodinf[,1]),]
        rank <- c(1:nrow(a))
        w <- length(rank)
        if(num == "worst"){num <- w}
        goodrank <- cbind(a, rank)
        if(num > w){
          print("NA")
        }else{
          print(goodrank[num,1])
        }
        
      }else if(outcome == "pneumonia"){
        info <- mystate[,c(1,2,5)]
        good <- complete.cases(info)
        goodinf <- info[good,]
        ##work on this to get the order correct
        a <- goodinf[order(as.numeric(goodinf[,3]),goodinf[,1]),]
        rank <- c(1:nrow(a))
        w <- length(rank)
        if(num == "worst"){num <- w}
        goodrank <- cbind(a, rank)
        if(num > w){
          print("NA")
        }else{
          print(goodrank[num,1])
        }
      }
      
      
      
    } else {
      stop("invalid state")
    }
    
  } else {
    stop("invalid outcome")
  }
  ##print(str(mystate))
}
