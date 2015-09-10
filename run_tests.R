library('RUnit')

macWD <- "~/Documents/science/kaggle/springleaf/K2"
winWD <- "D:/usr/science/kaggle/springleaf/K2"
if (file.exists(macWD)) {
  setwd(macWD)
} else {
  setwd(winWD)
}

source('funcs.R')

# works for scalars as well as vectors
checkDeltaEquals <- function(expected, actual, delta=1e-5)
{
  ok <- sum(is.na(expected)) == sum(is.na(actual))
  if (ok) {
    ok <- all(is.na(expected) == is.na(actual))
  }
  if (!all(is.na(expected))) {
    ok <- all ( abs(expected[!is.na(expected)] - actual[!is.na(actual)]) < delta )
  }
  
  if (!ok) {
    cat("Expected: ", expected, class(expected), fill=T)
    cat("Actual  : ", actual, class(actual), fill=T)
    print(abs(expected-actual))
    print(abs(expected-actual) < delta )
  }
  checkTrue(ok) 
}

test.suite <- defineTestSuite("example",
                              dirs = file.path("tests"),
                              testFileRegexp = '^test_.*\\.R')

test.result <- runTestSuite(test.suite, verbose=0)

printTextProtocol(test.result)
