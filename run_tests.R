library('RUnit')

macWD <- "~/Documents/science/kaggle/springleaf/K2"
winWD <- "D:/usr/science/kaggle/springleaf/K2"
if (file.exists(macWD)) {
  setwd(macWD)
} else {
  setwd(winWD)
}

source('funcs.R')

test.suite <- defineTestSuite("example",
                              dirs = file.path("tests"),
                              testFileRegexp = '^test_.*\\.R')

test.result <- runTestSuite(test.suite, verbose=0)

printTextProtocol(test.result)
