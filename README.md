Tool for testing: comparison of computed values with expected ones.

• Checks computed values, multiple values included.\
• Parameterizes the current output and error port to string ports and checks the outputs.\
• Catches and checks exceptions, syntax errors included.\
• Each test has its own optional time limit.\
• A test inherits parameters but does not affect those of the calling program.\
• Uses a custodian to kill threads made by a test, to close ports opened by a test, etc.\
• Double sided continuation barrier around each test.

More information in test.html which can be made from module test.scrbl.\
Keep all files in the same directory or else you probably have to make some adjustments in the require forms of all modules of the tester.

Have fun, Jos Koot
