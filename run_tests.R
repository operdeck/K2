library('RUnit')

source('funcs.R')

test.suite <- defineTestSuite("example",
                              dirs = file.path("tests"),
                              testFileRegexp = '^test_.*\\.R')

test.result <- runTestSuite(test.suite, verbose=0)

printTextProtocol(test.result)
