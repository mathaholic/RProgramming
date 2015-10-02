rankall <- function(outcome, num = "best"){
  
  ##read in our data frame, states, and conditions
  mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("Not Available"))
  
  ##throw out the irrelevant crap, to make things easier
  allinfo <- mydata[ ,c(2,7,11,17,23)]
  
  ##create a character vector of allowed outcomes
  conditions <- c("heart attack", "heart failure", "pneumonia")
  
  ##define best as the number 1.  Can't do that for worst, since worst is unique to each state
  if(num == "best"){num <- 1}
  
  ##create a data frame for each state's entry to go into
  sol <- data.frame()
  
  ## split allinfo by state
  ##bystate <- split(allinfo, allinfo$State)
  
  if(outcome %in% conditions){
    
    ###meat of program goes here
    
    if(outcome == "heart attack"){
      ##create a for loop that populates the sol data frame
      info <- allinfo[,c(1,2,3)]
      bystate <- split(info, info$State)
      for(i in length(bystate)){
        ##clean up data
        a <- as.data.frame(bystate[i])
        good <- complete.cases(a)
        gooda <- a[good,]
        b <- gooda[order(as.numeric(gooda[,3]),gooda[,1]),]
        ##now that we have out cleaned data, we can start getting what we want out of it
        ##  Things we want: 1. to populate the ith row of sol with the name and state of the hospital requested
        if(num == "worst"){num <- nrow(b)}
        if(num <= nrow(b)){
          sol <-rbind(sol, c(b[num,1],b[1,2]))
        }else{
          sol <-rbind(sol, c("NA",b[1,2]))}
        
      }
      colnames(sol) <- c("hospital", "state")
      print(sol)
      }
          
    else if(outcome == "heart failure"){
          ##dostuff
          
    }else if(outcome == "pneumonia"){
          ##dostuff
          
    }
  }
  else {
    stop("invalid outcome")
  }
          
    } 
    
          