# Test Symbolic Binning

test.Symbin.someBelowThreshold <- function() {
  data <- data.frame(1:10, c(10,12,8,6,6,12,8,8,10,12), c(T,F,T,T,F,T,T,F,T,F))
  names(data) <- c('id','a','outcome')
  
  sb <- createSymbin(data$a, data$outcome, 0.25)
#   print(sb)
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
#   print(sb)

  checkEquals(sb$binRank, c(1))
  result <- applySymbin(sb, c(6,7,10,8,NA))
  #print(result)
  checkDeltaEquals(result, rep(0.6, 5))
}

test.Symbin.withoutNAsInTrain <- function() {
  data <- data.frame(1:10, c(10,12,8,6,6,12,8,8,10,12), c(F,F,T,T,F,T,T,F,F,F))
  names(data) <- c('id','a','outcome')
  
  sb <- createSymbin(data$a, data$outcome, 0.0)
  #print(data)
  print(sb)
  checkEquals(sb$binIndex, c(1,2,3,4,5))
  
  result <- applySymbin(sb, c(6,7,NA,10,8,NA))
  #print(result)
  checkDeltaEquals(result, c(0.5, 0.6, NA, 1.0, 0.6666667, NA))
}

# <A>          <== map 'A' here
# <B>          <== map 'B' here
# <residual>   <== map residual here. Value will be NA if there were no remaining in train set.
# <NA>         <== map 'NA' here. Value will be NA if there were no NAs in train set.

# No residuals and no NA in train set
test.Symbin.allMapped <- function()
{
  vec <- c('A','B','A','B','A','B','A','B')
  out <- c( T,  F,  F,  T,  T,  F , T , F )
  sb <- createSymbin(vec, out, 0)
  checkDeltaEquals( applySymbin(sb, c('A', 'B', 'C', NA)), c(0.75, 0.25, NA, NA) )
}

# Residuals but no NA in train set
test.Symbin.withResidual <- function()
{
  vec <- c('A','B','C','A','B','A','B','A')
  out <- c( T,  F,  T , F,  T,  T,  F , T )
  sb <- createSymbin(vec, out, 0.25) # now, 'C' will not be in the binning
#   print(sb)
#   print(applySymbin.internal(sb, c('A', 'B', 'C', NA)))
  checkDeltaEquals( applySymbin(sb, c(0.75, 0.333333, 1.0, NA), c('A', 'B', 'C', NA)) )
}

# Everything below threshold
test.Symbin.allResidual <- function()
{
  vec <- c('A','B','C','A','B','A','B','A')
  out <- c( T,  F,  T , F,  T,  T,  F , T )
  sb <- createSymbin(vec, out, 0.8)
    print(sb)
    print(applySymbin.internal(sb, c('A', 'B', 'C', NA)))
  checkDeltaEquals( c( 0.625,  0.625,  0.625, NA), applySymbin(sb, c('A', 'B', 'C', NA)) )
}

# Everything below threshold with some missing
test.Symbin.allResidualSomeMissing <- function()
{
  vec <- c('A','B','C',NA,'B',NA,'B','A')
  out <- c( T,  F,  T , F, T, T,  F , T )
  sb <- createSymbin(vec, out, 0.8)
#   print(sb)
#   print(applySymbin.internal(sb, c('A', 'B', 'C', NA)))
  checkDeltaEquals( c( 0.66666,  0.66666,  0.66666, 0.5), applySymbin(sb, c('A', 'B', 'C', NA)) )
}

# Everything below threshold with some missing
test.Symbin.someResidualSomeMissing <- function()
{
  vec <- c('A','B','C',NA,'B',NA,'B','A')
  out <- c( T,  F,  T , F, T, T,  F , T )
  sb <- createSymbin(vec, out, 0.2)
  print(sb)
  print(applySymbin.internal(sb, c('A', 'B', 'C', NA)))
  checkDeltaEquals( c( 0.66666,  0.66666,  0.66666, 0.5), applySymbin(sb, c('A', 'B', 'C', NA)) )
}

# values not in normal set --> 
# NAs in sym binning train set --> 

#checkEquals(6, factorial(3))
#checkEqualsNumeric(6, factorial(3))
#checkIdentical(6, factorial(3))
#checkTrue(2 + 2 == 4, 'Arithmetic works')
#checkException(log('a'), 'Unable to take the log() of a string')
#DEACTIVATED('Deactivating this test function')
