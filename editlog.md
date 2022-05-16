# EDIT LOG

## TODO

There are 3 shift reduce conflicts in our scanner

<details>
<summary>Julian - Mar 30</summary>

### Changes

Scanner:

1. Edited the comment so that it ends with newline
2. Changed error log message in scanner
3. Added def token
4. Rework syntax and bug fixes to make it compile

Parser:

1. Corrected token names
2. Edited lambda call, it needs an rtype so the new definition will be
3. Fix syntax to make it compile

```
   int lambda int x, int y : x + y
```

Ast:

1. Commented out pretty print functions
2. Restructured lambda_def
3. Changed type program decleration

Sast

1. Syntax fixes to make it compile

Semant:

1. Added in some of our built in functions
2. Remove the need for "main" function

### Notes:

For our lambda functions, they're only really useful for two cases, the first is if we can actually save them to a variable
to make it a short helper function, for example

```
add = int lambda int a, int b, : a + b
```

the second is if we can use it as a function within a function, so for example

```
//code to double every number
[2,4,6,8].apply(int lambda int a : a * 2)
```

of the two, I feel as though the second one is more useful, however if we want to implement that, it may require us to change the structure
of our function calls and function definitions so that we can pass in functions as well. This means we're going to have to do more work
to edit it

### Luke's Notes on Lambda Functions

I made lambda functions their own type, lambda, created as follows

lamb x;
lamb x = int x, int y -> int : ( body )

Added arrow token to Scanner

Added lambda_def and expr match with Lambda of lambda_def to Ast

Added Lambda to typ and lambda match for vdecl to Parser.mly

Added Lambda(lambda_def) case to check_expr in Semant.ml. To check a lambda function, I just called check_func lambda, since the only difference between
a lambda and a func is the presence of func.name, which isn't part of check_func. So, it should work. But I'm not sure!

### How to compile, run, etc.

# Scan Testing

~~in scan_test folder:~~

~~ocamlbuild scan_test.native~~

~~./scan_test~~

# Parse Testing

in main folder:

ocamlbuild parser_test.native
./parser_test

# Julian's Updates

## Scan Testing

in main folder:

ocamlbuild scantest.native

./scantest.native

</details>
