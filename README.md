# ToLambda
A primitive compiler that takes a subset of Racket and converts it to Lambda calculus. 

To get started, run main.rkt and run commands in the interaction window. Takes input from input.txt.

Note: No effort was made (yet!) to make this any sort of readable or compact. Have fun reading the lambdas!
This is primarily due to currently lack of reduction for numbers, ideally could be reduced to a pair signed naturals, but for now are represented as a pair of a pair of (unreducible!) natural (unary!) numbers. (Read: Rationals.)

Recently added features:
If

Full list of valid constructs can be found in lambdaLang.rkt. (Also can be used to syntax check!)
