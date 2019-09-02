Tool for testing: comparison of computed values with expected ones.

• Checks computed values, multiple values included.\
• Parameterizes the current output and error port to string ports and checks the outputs.\
• Catches and checks exceptions, syntax errors included.\
• Each test has its own optional time limit.\
• A test inherits parameters but does not affect those of the calling program.\
• Uses a custodian to kill threads made by a test, to close ports opened by a test, etc.\
• Double sided continuation barrier around each test.
