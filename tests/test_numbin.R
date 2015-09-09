# Test Numeric Binning

test.Numbin.withoutNAsInTrain <- function() {
  data <- data.frame(1:10, c(10,12,8,6,6,12,8,8,10,12), c(T,F,T,T,F,T,T,F,T,F))
  names(data) <- c('id','a','outcome')
  
  nb <- createNumbin(data$a, data$outcome, 2)
#   print(data)
#   print(nb)
  checkEquals(nb$binRank, c(1,3,2,4))
  
  result <- applyNumbin(nb, c(6,7,NA,10,8,NA))
  #print(result)
  checkDeltaEquals(result, c(0.5, 0.5, NA, 0.6, 0.6666667, NA))
}

test.Numbin.withNAsInTrain <- function() {
  data <- data.frame(1:10, c(10,12,8,NA,6,12,8,NA,10,12), c(T,F,T,T,F,T,T,F,T,F))
  names(data) <- c('id','a','outcome')
  
  nb <- createNumbin(data$a, data$outcome, 2)
#   print(data)
#   print(nb)
  checkEquals(nb$binRank, c(3,2,1))
  
  result <- applyNumbin(nb, c(6,7,10,8,NA))
  #print(result)
  checkDeltaEquals(result, c(0.6666667, 0.6666667, 0.6, 0.6666667, 0.5))
}


#checkEquals(6, factorial(3))
#checkEqualsNumeric(6, factorial(3))
#checkIdentical(6, factorial(3))
#checkTrue(2 + 2 == 4, 'Arithmetic works')
#checkException(log('a'), 'Unable to take the log() of a string')
#DEACTIVATED('Deactivating this test function')
