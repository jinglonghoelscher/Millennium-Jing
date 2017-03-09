
####################################################
### HOW TO RUN THIS CODE                         ###
####################################################    
###                                              ###
### To run this script                           ###
### first uncomment the last line #main()        ###
### change to the correct path for pos and trd   ###
### and run the whole script                     ###
####################################################

library(data.table)

#############################
###  Position calculator  ###
#############################


#	From "pos.csv", calculate the netted position per each user
pos.netting  <- function(pos,username=NULL){
  
  # Group the positions by user
  output <- pos[,.(pos=sum(pos)),by=user]
  
  # if invalid user name provided, return the whole table
  if((length(username)>0) && (username %in% c("A","B","C","D","E"))){
      return(output[user %in% username]$pos)
  } else {
      print("Invalid user provided!")
      #write.csv(output,file="nettedPos.csv",row.names=FALSE)
      print(output)
      return(output)
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
trd.total    <- function(trdCrossing,symbol=NULL){
  
  # aggregate by symbols
  output <- trdCrossing[,.(trdTotal = sum(trd)),by=sym]
  
  # if invalid symbol is provided, return the whole table
  if((length(symbol)>0) && (symbol %in% unique(trdCrossing$sym))){
    return(output[sym %in% symbol]$trdTotal)
  } else {
    print("Invalid symbol provided!")
    #write.csv(output,file="trdTotal.csv",row.names=FALSE)
    print(output)
    return(output)
  }
}

#Find the final position after merging with the trd.csv
pos.final    <- function(pos,trd,username=NULL,symbol=NULL){
  
  # merge the pos and trd, replace NA with 0
  output <- merge(pos[,.(pos=sum(pos)),by=.(user,sym)],trd,all = TRUE,by=c("user","sym"))
  output[is.na(output)] <- 0
  
  # Update the position
  output <- output[,.(user,sym,pos=pos+qty),]
  
  # if invalid symbol/username is provided, return the whole table
  if((length(symbol)>0) && length(username)>0 && 
     (symbol %in% union(unique(trd$sym),unique(pos$sym))) && 
     (username %in% union(unique(trd$user),unique(pos$user)))){
    return(output[(sym %in% symbol) & (user %in% username),.(user,sym,pos),])
  } else {
    print("Invalid symbol/username provided!")
    #write.csv(output,file="pos.final.csv",row.names=FALSE)
    print(output)
    return(output)
  }
}

main<-function(path=""){
  
  # read the csv files
  pos <- as.data.table(read.csv(paste0(path,"pos.csv"),header=TRUE,sep=","))
  trd <- as.data.table(read.csv(paste0(path,"trd.csv"),header=TRUE,sep=","))
  
  #	From "pos.csv", calculate the netted position per each user
  pos.net     <- pos.netting(pos,username=NULL)

  #	List out all the boxed positions.
  pos.box     <- pos.boxed(pos)

  # Find all potential crossing
  trdCrossing <- trd.crossing(trd)
    
  #Find the total quantity to trade group by sym
  trdTotal    <- trd.total(trdCrossing,symbol=NULL)

  #Find the final position after merging with the trd.csv
  posFinal    <- pos.final(pos,trd,username=NULL,symbol=NULL)
    
  return(0)
}

main(path="")
