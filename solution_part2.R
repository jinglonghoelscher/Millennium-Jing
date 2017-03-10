
####################################################
### HOW TO RUN THIS CODE                         ###
####################################################    
###                                              ###
### To run this script                           ###
### set the path for pos.csv, trd.csv,           ###
### e.g. path<- "C:\\Millennium"                 ###
### and run the whole script                     ###
####################################################

library(data.table)

#############################
###  Position calculator  ###
#############################


#	From "pos.csv", calculate the netted position per each user
pos.netting  <- function(pos,usernames){
  
  # Group the positions by user
  output <- pos[,.(pos=sum(pos)),by=user]
  
  # if invalid user name provided, throw error
  # if usernames == "All", return the whole table
  if(all(usernames %in% c("A","B","C","D","E"))){
      return(output[user %in% usernames,.(user,pos),])
  } else if(usernames=="All"){
    #write.csv(output,file="nettedPos.csv",row.names=FALSE)
      return(output)
  } else {
      stop("Invalid usernames provided!")
  }
}

#	List out all the boxed positions.
pos.boxed    <- function(pos){
  
  # Add the columns of the number of brokers and the number of sides 
  # for each user and symbol
  pos[,`:=`(nopb=uniqueN(pb),signPos =prod(sign(pos))),by=.(user,sym)]
  setkey(pos,user,sym,pb)
  
  # Identify the boxed positions
  output<-pos[(nopb==2) & signPos <0,.(user,pb,sym,pos),]
  #write.csv(output,file="boxedPos.csv",row.names=FALSE)
  print(output)
  
  return(output)
}

# Find all potential crossing
trd.crossing <- function(trd){
  
  # sort the trade by symbol and quantity
  setkey(trd,sym,qty)
  
  # add the column of the total quantity per symbol per side, 
  # and the column of the portion of each user's quantity over the total of each symbol each side 
  trd[,`:=`(sidedQtySum=sum(qty),perc=qty/sum(qty)),by=.(sym,sign(qty))]
  
  # add the column of the number of sides per symbol, 
  # and the column of the crossing quantity per symbol
  trd[,`:=`(noSign=uniqueN(sign(qty)),jrnlTotal=sign(qty)*min(abs(sidedQtySum))),by=.(sym)]
  
  # add the column jrnl and trd
  trd[,jrnl:=ifelse(noSign==1,0,jrnlTotal*perc*(-1)),]
  trd[,trd:=qty + jrnl,]
  output <- trd[,.(sym,user,qty,jrnl=round(jrnl),trd=round(trd)),]
  #write.csv(output,file="trdCrossing.csv",row.names=FALSE)
  print(output)
  return(output)
}

#Find the total quantity to trade group by sym
trd.total    <- function(trdCrossing,symbols="All"){
  
  # aggregate by symbols
  output <- trdCrossing[,.(trdTotal = sum(trd)),by=sym]
  
  # if invalid symbol is provided, throw error
  # if symbols = "All", return the whole table
  if(is.null(symbols)){
    stop("Null symbols provided!")
  } else if((length(symbols)>0) & all(symbols %in% unique(trdCrossing$sym))){
    return(output[sym %in% symbols, .(sym, trdTotal),])
  } else if(symbols == "All"){
    #write.csv(output,file="trdTotal.csv",row.names=FALSE)
    return(output)
  } else {
    stop("Invalid symbols provided!")
  }
}

#Find the final position after merging with the trd.csv
pos.final    <- function(pos,trd,usernames="All",symbols="All"){
  
  # merge the pos and trd, replace NA with 0
  output <- merge(pos[,.(pos=sum(pos)),by=.(user,sym)],trd,all = TRUE,by=c("user","sym"))
  output[is.na(output)] <- 0
  
  # Update the position
  output <- output[,.(user,sym,pos=pos+qty),]
  
  # if invalid symbols/usernames is provided, throw error
  # if symbols = "All" and usernames="All", return the whole thing
  if(is.null(symbols)){
    stop("Null symbols provided!")
  }else if(is.null(usernames)){
    stop("Null usernames provided!")
  }else if((symbols=="All") && (usernames=="All")){
    #write.csv(output,file="pos.final.csv",row.names=FALSE)
    return(output)
  }else if((length(symbols)>0) && length(usernames)>0 &&
     (all(symbols %in% union(unique(trd$sym),unique(pos$sym)))) && 
     (all(usernames %in% union(unique(trd$user),unique(pos$user))))){
    return(output[(sym %in% symbols) & (user %in% usernames),.(user,sym,pos),])
  } else if((length(usernames)==0) ||  !(all(usernames %in% union(unique(trd$user),unique(pos$user))))){
    stop("Invalid usernames provided!")
  } else {
    stop("Invalid symbols provided!")
  }
}

main<-function(path="."){
  
  # read the csv files
  pos <- as.data.table(read.csv(paste(path,"pos.csv",sep="\\"),header=TRUE,sep=","))
  trd <- as.data.table(read.csv(paste(path,"trd.csv",sep="\\"),header=TRUE,sep=","))
  
  #	From "pos.csv", calculate the netted position per each user
  pos.net     <- pos.netting(pos,usernames="All")

  #	List out all the boxed positions.
  pos.box     <- pos.boxed(pos)

  # Find all potential crossing
  trdCrossing <- trd.crossing(trd)
    
  #Find the total quantity to trade group by sym
  trdTotal    <- trd.total(trdCrossing,symbols="All")

  #Find the final position after merging with the trd.csv
  posFinal    <- pos.final(pos,trd,usernames="All",symbols="All")
    
  return(list(pos.net,pos.box,trdCrossing,trdTotal,posFinal))
}

#output <- main(path=".")
