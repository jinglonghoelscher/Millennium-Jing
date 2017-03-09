
###############################################
### HOW TO RUN THIS CODE                    ###
###############################################    
###                                         ###
### To run this script                      ###
### first uncomment the last line #main()   ###
### and run the whole script                ###
###                                         ###
###############################################

library(data.table)

#################################
### Maximum Overlap Intervals ###
#################################

max.overlap.intervals   <- function(){
  
  ##############################
  ### Define the given table ###
  ##############################
  
  procs<-data.frame(id=seq(10,80,by=10),
                    anest=c("baker","baker",rep("dow",6)), 
                    start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),
                    end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"))
  procs<-as.data.table(procs)
  
  # Change the type of start and eend to POSIXct
  procs[,`:=`(start=as.POSIXct(start,format="%H:%M"), end=as.POSIXct(end,format="%H:%M")),]
  
  
  ##############################################################
  ###   Part 1:  list out the ids each row is intersection   ###
  ##############################################################
  
  # Add columns start0,end0 to not include the end points of the time interval 
  # for the usage of function foverlaps which include the endpoints as in the intersection
  procs[,`:=`(start0=start+(1e-4), end0=end-(1e-4)),]
  procs<-setkeyv(procs,cols=c("anest","start0","end0"))
  
  # Get the ids which have overlapping intervals and merge into the original table procs
  overlaps_index<-foverlaps(procs,procs,type="any",which=TRUE)
  overlaps_id<-data.table(procs[,.(id),][unlist(overlaps_index[,1])],w=procs[,.(id),][unlist(overlaps_index[,2])])
  setnames(overlaps_id,"w.id","w")
  setkeyv(overlaps_id,cols=c("id","w"))
  overlaps<-as.data.table(aggregate(w ~ id,data=overlaps_id,paste,collapse=" "))
  procs<-merge(procs,overlaps,by="id",all.x=TRUE)
  
  ##################################################################################
  ###   Part 2:  list out the max # intersecting ids for each anest and each id  ###
  ##################################################################################

  # Get a sorted column of end points of all time intervals
  tmp_start <- setkey(procs[,.(startNum=.N),by=.(anest,start)],anest,start)
  tmp_end   <- setkey(procs[,.(endNum=.N),by=.(anest,end)],anest,end)
  tmp       <- merge(tmp_start,tmp_end,by.x=c("anest","start"),by.y=c("anest","end"),all.x=TRUE,all.y=TRUE,sort=TRUE)
  tmp[is.na(tmp)]<-0
  
  # Count the number of end points: start = 1 & end = -1
  tmp[,numint:=cumsum(startNum)-cumsum(endNum),]
  
  # Use the foverlaps functions for intersections of points and time intervals
  tmp[,end:=start,]
  setkey(tmp,anest,start,end)
  setkey(procs,anest,start,end)
  overlaps2_index<-foverlaps(tmp,procs,type="any",which=TRUE)
  overlaps_numint<-data.table(procs[,.(id),][unlist(overlaps2_index[,2])],tmp[,.(numint),][unlist(overlaps2_index[,1])])
  
  # Add the max number of intersecting intervals for each anest and each id
  procs<-merge(procs,overlaps_numint[,.(s=max(numint)),by=id],by="id",all.x=TRUE)
  procs<-procs[,.(id,anest,start=substr(start,12,16),end=substr(end,12,16),s,w),]
  
  # Print the result
  print("The result for the problem \'Maximum Overlap Intervals\' is below:")
  print(procs)
  #write.csv(procs,file="procs.csv",row.names=FALSE)
  return(procs)
}

##########################
###  Pascal Triangle   ###
##########################

pascal.triangle <- function(n){
  
  # Define the output to be a vector of list 
  output <- vector('list',n)
  
  # Initilize the output vector
  output[[1]] <- 1
  
  # Display the output
  print(paste0("The Pascal Triangle for ", n, " is below:"))
  print(output[[1]])
  
  # Define recursively of the Pascal Triangle
  for(i in 2:(n+1)){
    output[[i]] <- c(output[[i-1]],0) + c(0,output[[i-1]])
    print(output[[i]])
  }
  return(output)
}


##############################
###  Portfolio VaR & CVaR  ###
##############################

# loading adj.close for given tickers and dates from yahoo finance
# based on the online script by Fotis Papailias & Dimitrios Thomakos

data.loading       <- function(tickers, start.date, end.date){
  # Change the locale
  sl <- Sys.setlocale(locale="US")
  
  # Create the universe of dates
  all.dates <- seq(as.Date(start.date)-1, as.Date(end.date), by="day")
  all.dates <- subset(all.dates,weekdays(all.dates) != "Sunday" & weekdays(all.dates) != "Saturday")
  all.dates.char <- as.matrix(as.character(all.dates))
  
  # Create sparse matrices
  ret <- matrix(NA, NROW(all.dates.char), length(tickers))
  
  
  # Name the rows correctly
  rownames(ret) <- all.dates.char
  
  
  # Split the start and end dates to be used in the ULR later on
  splt <- unlist(strsplit(start.date, "-"))
  a <- as.character(as.numeric(splt[2])-1)
  b <- splt[3]
  c <- splt[1]
  
  splt <- unlist(strsplit(end.date, "-"))
  d <- as.character(as.numeric(splt[2])-1)
  e <- splt[3]
  f <- splt[1]
  
  # Create the two out of the three basic components for the URL loading
  str1 <- "http://ichart.finance.yahoo.com/table.csv?s="
  str3 <- paste("&a=", a, "&b=", b, "&c=", c, "&d=", d, "&e=", e, "&f=", f, "&g=d&ignore=.csv", sep="")
  
  # Main loop for all assets
  for (i in seq(1,length(tickers),1))
  {
    str2 <- tickers[i]
    strx <- paste(str1,str2,str3,sep="")
    x <- read.csv(strx)
    
    dates <- as.matrix(x[1])
    
    replacing <- match(dates, all.dates.char)
    tmp       <- as.data.table(x[7])
    tmp[,r:=(Adj.Close - shift(Adj.Close,n=1L, fill=NA,type="lag"))/shift(Adj.Close,n=1L, fill= NA ,type="lag"),]
    ret[replacing,i] <- as.matrix(tmp[,r,])
  }
  
  # Name the cols correctly
  ret           <-na.omit(ret)
  ret           <- as.data.table(ret,keep.rownames=TRUE)
  colnames(ret) <- c("Date",tickers)
  
  
  # Return the ouput
  return(ret)
}

# Functions to compute the historical VaR, historical CVaR, 
# parametric VaR, parametric CVaR
portfolio.var.cvar <- function(){
 
  # Portfolio constituents
  tickers    <- c("AAPL","IBM","GOOG","BP","XOM","COST","GS")
  
  # Time period
  start.date <- "2016-01-01"
  end.date   <- "2016-12-31"
  
  # load data from yahoo finance for adj.close of the price
  # of the stocks in the portfolio
  data       <- data.loading(tickers,start.date,end.date)
  NDates     <- nrow(data)
  
  # Compute the historical portfolio return for 2016
  porf.wt    <- matrix(c(0.15,0.2,0.2,0.15,0.1,0.15,0.05),
                       nrow=7,
                       ncol=1,
                       byrow=FALSE,
                       dimnames=list(tickers,c("weight")))
  data[,PRet:=as.matrix(data[,2:8]) %*% porf.wt,]
  
  # Historical VaR and CVaR
  alpha       <-  0.95    # confidence level
  hvar        <- abs(quantile(data$PRet,1-alpha))
  hcvar       <- abs(sum(data[PRet<= -hvar,.(PRet),])/(NDates*(1-alpha)))
  
  # Compute the covariance of the returns, and portfolio vol using parametric method, assuming the normal distribution
  covariance         <- cov(data[,2:8,])
  vol.porf           <- sqrt(t(as.matrix(porf.wt)) %*% covariance %*% as.matrix(porf.wt))
  avg.porf           <- mean(data$PRet)
  dimnames(vol.porf) <- list(c("porfVol"),c("porfVol"))
  parvar             <- abs(qnorm(1-alpha,0,1)*vol.porf)    # Assume the expected portfolio return is zero, since it is very close to zero
  dimnames(parvar)   <- list(c("parvar"),c("parvar"))
  parcvar            <- vol.porf * dnorm(qnorm(1-alpha,0,1),0,1)/(1-alpha) # Assume expected portfolio return is zero.
  dimnames(parcvar)  <- list(c("parcvar"),c("parcvar"))
  
  # Define the output table for historical var, historical cvar,
  # parametric var, parameteric cvar.
  output <- data.table(hvar = hvar, hcvar = hcvar, parvar, parcvar)
  
  # Print the output
  print("The historical VaR/CVaR, parametric VaR/CVaR are below:")
  print(output)
  #write.csv(output,file="portfolioVaR.csv",row.names = FALSE)
  return(output)
}
  
# optimal portfolio holding 
optimalPort        <- function(lambda){
  
  #load data of 2015 and 2016
  data2              <- data.loading(tickers,"2015-01-01","2016-12-31")
  setkey(data2,Date)
  
  #create the output optimal portfolio weights for each month
  output           <-  matrix(NA, 12, length(tickers))
  month.list       <-  unique(data2[substr(Date,1,4)=="2016",(Month=substr(Date,1,7)),])
  rownames(output) <-  month.list
  colnames(output) <-  tickers
  
  #For each month, find the optimal weights
  for(i in 1:length(month.list)){
    
    #For each month, take the 250 business date data prior to the first of the month. 
    month    <- month.list[i]
    dt       <- head(data2[substr(Date,1,7)==month,Date,],1)
    index    <- which(data2$Date == dt)
    data0    <- copy(data2[(index -250):(index-1)])
    
    #compute the expected returns for each stock and covariance matrix
    Er       <- sapply(data0[,2:8],mean)
    covar.inv<- solve(cov(data0[,2:8]))
    
    #calculate the optimal weights for the month
    e        <- matrix(1,nrow=7,ncol=1)
    w.minvar <- t(covar.inv %*% e)/(t(e)%*% covar.inv %*% e)[1,1]
    w.mk     <- t(covar.inv %*% Er)/ (t(Er) %*% covar.inv %*% e)[1,1]
    s        <- (t(Er) %*% covar.inv %*% e)[1,1]/(2*lambda)
    sum((1-s)*w.minvar +s*w.mk)
    output[month,] <-  (1-s)*w.minvar +s*w.mk
  }
  output <- as.data.table(output,keep.rownames = TRUE)
  colnames(output)[1] <- "Month"
  
  # Print the output
  print("The optimal portfolio weights for 2016 are below: ")
  print(output)
  #write.csv(output,file="optimalPortfolio.csv",row.names = FALSE)
  return(output)
}


#################################################
###  Main function to run all three problems  ###
#################################################
main<-function(){
  
  ### Maximum Overlap Intervals 
  procs    <- max.overlap.intervals()
  
  ###  Pascal Triangle 
  pascal   <- pascal.triangle(10)
  
  ###  Portfolio VaR & CVaR
  varTbl   <- portfolio.var.cvar()
  
  ### optimal portfolio holding 
  # lambda represents the risk-aversion
  porf.wt  <- optimalPort(lambda=0.5)
  
  return(0)
}

#main()
