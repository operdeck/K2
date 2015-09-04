# Test Symbolic Binning

# works for scalars as well as vectors
checkDeltaEquals <- function(expected, actual, delta=1e-5)
{
  ok <- all ( abs(expected-actual) < delta )
  if (!ok) {
    cat("Expected: ", expected, class(expected), fill=T)
    cat("Actual  : ", actual, class(actual), fill=T)
    print(abs(expected-actual))
    print(abs(expected-actual) < delta )
  }
  checkTrue(ok) 
}

test.LogLoss.1 <- function()
{
  checkDeltaEquals( 0.1446215, logLoss(c(T,F,T),c(0.8,0.1,0.9)) )
}

test.Symbin.someBelowThreshold <- function() {
  data <- data.frame(1:10, c(10,12,8,6,6,12,8,8,10,12), c(T,F,T,T,F,T,T,F,T,F))
  names(data) <- c('id','a','outcome')
  
  sb <- createSymbin(data$a, data$outcome, 0.25)
  
  print(sb)
  checkEquals(sb$binRank, c(1,2,3))
  
  result <- applySymbin(sb, c(6,7,10,8,NA))
  #print(result)
  checkDeltaEquals(result, c(0.75, 0.75, 0.75, 0.6666667, 0.75))
}

test.Symbin.allBelowThreshold <- function() {
  data <- data.frame(1:10, c(10,12,8,6,6,12,8,8,10,12), c(T,F,T,T,F,T,T,F,T,F))
  names(data) <- c('id','a','outcome')
  
  checkDeltaEquals(sum(data$outcome)/nrow(data), 0.6)
  sb <- createSymbin(data$a, data$outcome, 0.5)
  print(sb)

  checkEquals(sb$binRank, c(1))
  result <- applySymbin(sb, c(6,7,10,8,NA))
  #print(result)
  checkDeltaEquals(result, rep(0.6, 5))
}

test.Symbin.simple <- function() {
  data <- data.frame(1:10, c(10,12,8,6,6,12,8,8,10,12), c(T,F,T,T,F,T,T,F,T,F))
  names(data) <- c('id','a','outcome')
  
  sb <- createSymbin(data$a, data$outcome, 0.0)
  #print(data)
  print(sb)
  
  checkEquals(sb$binRank, c(1,2,4,5,3))
  
  result <- applySymbin(sb, c(6,7,6,10,8,NA))
  #print(result)
  checkDeltaEquals(result, c(0.5, 0.6, 0.5, 1.0, 0.6666667, 0.6))
}


#checkEquals(6, factorial(3))
#checkEqualsNumeric(6, factorial(3))
#checkIdentical(6, factorial(3))
#checkTrue(2 + 2 == 4, 'Arithmetic works')
#checkException(log('a'), 'Unable to take the log() of a string')
#DEACTIVATED('Deactivating this test function')
