# Test Symbolic Binning

test.Symbin.someBelowThreshold <- function() {
  sb <- createSymbin(c(10,12,8,6,6,12,8,8,10,12), 
                     c(T, F, T,T,F,T, T,F,T, F), 0.30)
  checkEquals(sb$binRank, c(2,1,3,4))
  checkDeltaEquals(c(0.75, 0.75, 0.75, 0.6666667, NA),
                   applySymbin(sb, c(6,7,10,8,NA)))
}

test.Symbin.allBelowThreshold <- function() {
  sb <- createSymbin(c(10,12,8,6,6,12,8,8,10,12), 
                     c(T,F,T,T,F,T,T,F,T,F), 0.5)
  checkEquals(sb$binRank, c(1,2))
  checkDeltaEquals(c(rep(0.6, 4),NA),
                   applySymbin(sb, c(6,7,10,8,NA)))
}

test.Symbin.withoutNAsInTrain <- function() {
  sb <- createSymbin(c(10,12,8,6,6,12,8,8,10,12), 
                     c(F,F,T,T,F,T,T,F,F,F), 0.0)
  checkEquals(sb$binRank, c(3,4,1,2,5,6))
  checkDeltaEquals(c(0.5, NA, NA, 0, 0.6666667, NA),
                   applySymbin(sb, c(6,7,NA,10,8,NA)))
}

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
  checkDeltaEquals( c(0.75, 0.333333, 1.0, NA),
                    applySymbin(sb, c('A', 'B', 'C', NA)))
}

# Everything below threshold
test.Symbin.allResidual <- function()
{
  vec <- c('A','B','C','A','B','A','B','A')
  out <- c( T,  F,  T , F,  T,  T,  F , T )
  sb <- createSymbin(vec, out, 0.8)
  checkDeltaEquals( c( 0.625,  0.625,  0.625, NA), 
                    applySymbin(sb, c('A', 'B', 'C', NA)) )
}

# Everything below threshold with some missing
test.Symbin.allResidualSomeMissing <- function()
{
  vec <- c('A','B','C',NA,'B',NA,'B','A')
  out <- c( T,  F,  T , F, T, T,  F , T )
  sb <- createSymbin(vec, out, 0.8)
  checkDeltaEquals( c( 0.66666,  0.66666,  0.66666, 0.5), 
                    applySymbin(sb, c('A', 'B', 'C', NA)) )
}

# Everything below threshold with some missing
test.Symbin.someResidualSomeMissing <- function()
{
  vec <- c('A','B','C',NA,'B',NA,'B','A')
  out <- c( T,  F,  T , F, T, T,  F , T )
  sb <- createSymbin(vec, out, 0.2)
  checkDeltaEquals( c( 1,  0.33333,  1, 0.5), 
                    applySymbin(sb, c('A', 'B', 'C', NA)) )
}

test.Symbin.minimal <- function()
{
  sb <- createSymbin(c('A'),c(T),0.0)
  checkEquals(3, nrow(sb))
  checkEquals(c(1,2,3),sb$binRank)
  checkDeltaEquals(c(1, NA, NA), applySymbin(sb, c('A','C',NA)))
}

test.Symbin.someNAs <- function()
{
  sb <- createSymbin(c(NA,'A',NA),c(T,T,F),0.0)
  checkEquals(3, nrow(sb))
  checkEquals(c(2,3,1),sb$binRank)
  checkDeltaEquals(c(1, NA, 0.5), applySymbin(sb, c('A','C',NA)))
}

test.Symbin.someNAsNoResidual <- function()
{
  sb <- createSymbin(c(NA,'A','B','C',NA,'A','B'),c(T,T,F,T,F,T,F),0.0)
  checkEquals(5, nrow(sb))
  checkEquals(c(3,1,4,5,2),sb$binRank)
  checkDeltaEquals(c(1, 0, NA, 0.5), applySymbin(sb, c('A','B','D',NA)))
}  

test.Symbin.someNAsSomeResidual <- function()
{
  sb <- createSymbin(c(NA,'A','B','C',NA,'A','B'),c(T,T,F,T,F,T,F),0.2) # exclude C
  checkEquals(4, nrow(sb))
  checkEquals(c(3,1,4,2),sb$binRank)
  checkDeltaEquals(c(1, 0, 1, 0.5), applySymbin(sb, c('A','B','D',NA)))
}

test.Symbin.empty <- function()
{
  sb <- createSymbin(c(NA),c(T),0.0)
  checkEquals(2, nrow(sb))
  checkEquals(c(2,1),sb$binRank)
  checkDeltaEquals(c(NA,NA,NA,1), applySymbin(sb, c('A','B','D',NA)))
}

test.Symbin.allBelowThreshold <- function()
{
  sb <- createSymbin(c('A','B'),c(T,T),0.6)
  checkEquals(2, nrow(sb))
  checkEquals(c(1,2),sb$binRank)
  checkDeltaEquals(c(1,1,1,NA), applySymbin(sb, c('A','B','D',NA)))
}


# examples:

#checkEquals(6, factorial(3))
#checkEqualsNumeric(6, factorial(3))
#checkIdentical(6, factorial(3))
#checkTrue(2 + 2 == 4, 'Arithmetic works')
#checkException(log('a'), 'Unable to take the log() of a string')
#DEACTIVATED('Deactivating this test function')
