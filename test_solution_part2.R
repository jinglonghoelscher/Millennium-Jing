
#############################
###  Position calculator  ###
###  unit testing         ###
#############################

#	From "pos.csv", calculate the netted position per each user
# test the function pos.netting

test_that("invalid username", {
  # Test for any error
  expect_that(pos.netting(pos, "H"), throws_error())
  
  # Test specifically for an error string containing "Invalid usernames"
  expect_that(pos.netting(pos, "H"), throws_error("Invalid usernames"))

  # Test specifically for an error string containing "Invalid symbols" or "invalid symbols"
  expect_that(pos.netting(pos, "H"), throws_error("[Ii]nvalid symbols"))
})

test_that("valid two usernames", {
  posNetting <- pos.netting(pos, c("A","B"))
  expect_that( posNetting, is_a("data.table") )
  expect_that( dim(posNetting), equals(c(2,2)) )
  expect_that( as.character(posNetting$user), equals( c("A","B")))
})

test_that("valid one username", {
  posNetting <- pos.netting(pos, "C")
  expect_that( posNetting$pos, is_a("integer") )
  expect_that( dim(posNetting), equals(c(1,2)) )
  expect_that( posNetting$pos, equals(5430))
})

# Find the total quantity to trade group by sym
# unit test trd.total   

test_that("invalid symbol", {
  
  # Test for any error
  expect_that(trd.total(trdCrossing, symbols=NULL), throws_error())
  
  # Test specifically for an error string containing "Invalid usernames"
  expect_that(trd.total(trdCrossing, symbols=NULL), throws_error("Invalid usernames"))
  
  # Test specifically for an error string containing "Invalid symbols" or "invalid symbols"
  expect_that(trd.total(trdCrossing, symbols=NULL), throws_error("[Ii]nvalid symbols"))
  
  # Test specifically for an error string containing "Null symbol" or "null symbols"
  expect_that(trd.total(trdCrossing, symbols=NULL), throws_error("[Nn]ull symbols"))
  
})

test_that("valid multi symbols", {
  trdTotal <- trd.total(trdCrossing, symbols=c("1310.T","1003.T","1984.T"))
  expect_that( trdTotal, is_a("data.table") )
  expect_that( dim(trdTotal), equals(c(3,2)) )
  expect_that(trdTotal$trdTotal, equals(c(6278,6562,5293)))
})

# Find the final position after merging with the trd.csv
# unit test pos.final

test_that("NULL symbols", {
  
  # Test for any error
  expect_that(pos.final(pos,trd,usernames=c("A","B"), symbols=NULL), throws_error())
  
  # Test specifically for an error string containing "Invalid usernames"
  expect_that(pos.final(pos,trd,usernames=c("A","B"), symbols=NULL), throws_error("Invalid usernames"))
  
  # Test specifically for an error string containing "Invalid symbols" or "invalid symbols"
  expect_that(pos.final(pos,trd,usernames=c("A","B"), symbols=NULL), throws_error("[Ii]nvalid symbols"))
  
  # Test specifically for an error string containing "Null symbol" or "null symbols"
  expect_that(pos.final(pos,trd,usernames=c("A","B"), symbols=NULL), throws_error("[Nn]ull symbols"))
  
  # Test specifically for an error string containing "Null usernames" or "null usernames"
  expect_that(pos.final(pos,trd,usernames=c("A","B"), symbols=NULL), throws_error("[Nn]ull usernames"))
  
})

test_that("NULL usernames", {
  
  # Test for any error
  expect_that(pos.final(pos,trd,usernames=NULL, symbols=c("1003.T","1020.T","1694.T","1982.T")), throws_error())
  
  # Test specifically for an error string containing "Invalid usernames"
  expect_that(pos.final(pos,trd,usernames=NULL, symbols=c("1003.T","1020.T","1694.T","1982.T")), throws_error("Invalid usernames"))
  
  # Test specifically for an error string containing "Invalid symbols" or "invalid symbols"
  expect_that(pos.final(pos,trd,usernames=NULL, symbols=c("1003.T","1020.T","1694.T","1982.T")), throws_error("[Ii]nvalid symbols"))
  
  # Test specifically for an error string containing "Null symbol" or "null symbols"
  expect_that(pos.final(pos,trd,usernames=NULL, symbols=c("1003.T","1020.T","1694.T","1982.T")), throws_error("[Nn]ull symbols"))
  
  # Test specifically for an error string containing "Null usernames" or "null usernames"
  expect_that(pos.final(pos,trd,usernames=NULL, symbols=c("1003.T","1020.T","1694.T","1982.T")), throws_error("[Nn]ull usernames"))
  
})


test_that("invalid usernames", {
  
  # Test for any error
  expect_that(pos.final(pos,trd,usernames="G", symbols=c("1003.T","1020.T","1694.T","1982.T")), throws_error())
  
  # Test specifically for an error string containing "Invalid usernames"
  expect_that(pos.final(pos,trd,usernames="G", symbols=c("1003.T","1020.T","1694.T","1982.T")), throws_error("Invalid usernames"))
  
  # Test specifically for an error string containing "Invalid symbols" or "invalid symbols"
  expect_that(pos.final(pos,trd,usernames="G", symbols=c("1003.T","1020.T","1694.T","1982.T")), throws_error("[Ii]nvalid symbols"))
  
  # Test specifically for an error string containing "Null symbol" or "null symbols"
  expect_that(pos.final(pos,trd,usernames="G", symbols=c("1003.T","1020.T","1694.T","1982.T")), throws_error("[Nn]ull symbols"))
  
  # Test specifically for an error string containing "Null usernames" or "null usernames"
  expect_that(pos.final(pos,trd,usernames="G", symbols=c("1003.T","1020.T","1694.T","1982.T")), throws_error("[Nn]ull usernames"))
  
})

test_that("invalid symbol", {
  # Test for any error
  expect_that(pos.final(pos,trd,usernames=c("C","D"), symbols=c("1003.J","1020.T","1694.T","1982.T")), throws_error())
  
  # Test specifically for an error string containing "Invalid usernames"
  expect_that(pos.final(pos,trd,usernames=c("C","D"), symbols=c("1003.J","1020.T","1694.T","1982.T")), throws_error("Invalid usernames"))
  
  # Test specifically for an error string containing "Invalid symbols" or "invalid symbols"
  expect_that(pos.final(pos,trd,usernames=c("C","D"), symbols=c("1003.J","1020.T","1694.T","1982.T")), throws_error("[Ii]nvalid symbols"))
  
  # Test specifically for an error string containing "Null symbol" or "null symbols"
  expect_that(pos.final(pos,trd,usernames=c("C","D"), symbols=c("1003.J","1020.T","1694.T","1982.T")), throws_error("[Nn]ull symbols"))
  
  # Test specifically for an error string containing "Null usernames" or "null usernames"
  expect_that(pos.final(pos,trd,usernames=c("C","D"), symbols=c("1003.J","1020.T","1694.T","1982.T")), throws_error("[Nn]ull usernames"))
  
})

test_that("valid multi symbols", {
  posTrd <- pos.final(pos,trd,usernames=c("C","D"), symbols=c("1003.T","1020.T","1694.T","1982.T"))
  expect_that( posTrd, is_a("data.table") )
  expect_that( dim(posTrd), equals(c(8,3)) )
  expect_that( unique(as.character(posTrd$user)), equals(c("C","D")) )
  expect_that( length(unique((posTrd$sym))), equals(4))
  expect_that(posTrd[(user=="C")&(sym=="1003.T")]$pos, equals(2380))
})
