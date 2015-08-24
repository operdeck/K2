# tests for funcs

# works for scalars as well as vectors
checkDeltaEquals <- function(expected, actual, delta=1e-5)
{
  print(abs(expected-actual))
  checkTrue( all ( abs(expected-actual) < delta ) ) 
}

test.LogLoss.1 <- function()
{
  checkDeltaEquals( 0.1446215, logLoss(c(T,F,T),c(0.8,0.1,0.9)) )
}

test.SymBin <- function() {
  data <- data.frame(1:10, c(10,12,8,6,6,12,8,8,10,12), c(T,F,T,T,F,T,T,F,T,F))
  setnames(data, c('id','a','outcome'))
  
  sb <- createSymBin2(data, "a", "outcome", 0.0)

  print(data)
  print(sb)
  
  checkEquals(sb[["val"]], c(12,6,8,10,NA)) # NB expecting ordered by avg outcome - not the case
  
  result <- applySymBin(sb, data$a)
  
  print(result)
  
  zz <- data.frame(1:4, c(12,6,8,6))
  setnames(zz, c('id','a'))
  zz <- join(zz, a_symbinning, by='a')
  
  checkEquals(zz$a, c(12, 6, 8, 6))
  checkDeltaEquals(zz$a_symbin, c(0.3333333, 0.5, 0.6666667, 0.5))
}


#checkEquals(6, factorial(3))
#checkEqualsNumeric(6, factorial(3))
#checkIdentical(6, factorial(3))
#checkTrue(2 + 2 == 4, 'Arithmetic works')
#checkException(log('a'), 'Unable to take the log() of a string')
#DEACTIVATED('Deactivating this test function')
