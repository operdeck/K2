# Test Measurements

test.LogLoss <- function()
{
  checkDeltaEquals( 0.1446215, logLoss(c(T,F,T),c(0.8,0.1,0.9)) )
}
