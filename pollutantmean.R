pollutantmean <- function(directory, pollutant, id = 1:332){
  
  all_files <- list.files(directory, full.names = TRUE)
    ##loads all file names from the directory into a 
    ##character vector
  all_stns <- data.frame()
    ##creates an empty data frame for all the files in the
    ##directory
  for(i in id){
    all_stns <- rbind(all_stns, read.csv(all_files[i]))
  }
    ##fills the data frame with the information from each 
    ##file in the directory
  
  all_stns_id <- all_stns[which(all_stns[ , "ID"] %in% id), ]
  
  if (pollutant == "sulfate"){ 
    print(mean(all_stns_id[, "sulfate"], na.rm = TRUE))
    ##pulls the mean for sulfate
  }else if(pollutant == "nitrate"){
    print(mean(all_stns_id[, "nitrate"], na.rm = TRUE))
    ##pulls the mean for nitrate
  } else {
    print("I'm sorry, the polluant you have entered has no associated records.")
    ##tells the user their pollutant has no records
  }
}