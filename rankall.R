rankall <- function(outcome, num = "best") {
  ## Read outcome data
  measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  # no state, just outcome . . .
  validOutcomes = c("heart attack", "heart failure", "pneumonia")
  if(!is.element(outcome, validOutcomes)) {
    stop("invalid outcome")
  } else if (outcome == "heart attack") { # if valid, pick column corresponding to outcome
    thecolumn = 11 # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  } else if (outcome == "heart failure") {
    thecolumn = 17 # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  } else {
    thecolumn = 23 # Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  measures[, thecolumn] <- as.numeric(measures[, thecolumn]) # cast the needed column to numeric
  all_states <- unique(measures[,7]) # create list of all states
  all_states <- sort(all_states) # sort alphabetically
  # the_dataframe <- colnames(measures[0,c(2,7)]) # start with an empty data.frame
  the_dataframe <- data.frame(hospital=character(), state=character()) # start with an empty data.frame
  for (state in all_states) {
    measures_thestate <- measures[measures$State == state,][c(2,thecolumn)]
    # order by 2d column, then by 1st column
    measures_thestate <- measures_thestate[order(measures_thestate[2], measures_thestate[1], na.last = NA), ]
    # choose what to return
    hospital <- if (num == "best") {
      measures_thestate[1,1]
    } else if (num == "worst") {
      measures_thestate[nrow(measures_thestate),1]
    } else if ( as.numeric(num) > nrow(measures_thestate) ) {
      NA
    } else {
      measures_thestate[as.numeric(num), 1]
    } 
    # append to data.frame to be returned
    the_dataframe <- rbind(the_dataframe, data.frame(hospital, state))
  }
  the_dataframe # the return data.frame  
}

