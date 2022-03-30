# EDIT LOG

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

</details>
