library(testthat)
source("solution_part2.R")


####################################################
### HOW TO RUN THIS CODE                         ###
####################################################    
###                                              ###
### To run this script                           ###
### set the path for pos.csv, trd.csv,           ###
### solution_part2.R, test_solution_part2.R      ###
### e.g. path<- "C:\\Millennium"                 ###
### and run the whole script                     ###
####################################################

path  <- "."

main<-function(path){

  # read the csv files 
  pos   <- as.data.table(read.csv(paste(path,"pos.csv",sep="\\"),header=TRUE,sep=","))
  trd   <- as.data.table(read.csv(paste(path,"trd.csv",sep="\\"),header=TRUE,sep=","))
  
  # Get all potential crossing
  trdCrossing <- trd.crossing(trd)
  
  # run all the test on test_solution_part2.R
  test_results <- test_dir(path,reporter="summary")
  #write.csv(test_results,file="test_results.csv",row.names=FALSE)
  return( test_results)
}

#test_results <- main(path)
